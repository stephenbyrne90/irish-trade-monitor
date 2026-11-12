# Page module "Concentration" (internal name `structure`).
#
# How concentrated is Irish goods trade, is it getting worse, what is inside
# pharma, and is a move in value a move in volume or in price?
#
#   mod_structure_ui(id)
#   mod_structure_server(id, td, flow)   # td: bundle (plain list); flow: reactive "1"/"2"
#
# Every chart / table is built by a pure structure_* function taking
# (td, flow_code, ...) so it can be tested without Shiny. The heavy 5-digit work
# (rolling 12-month sums by product, concentration statistics, group mapping) is
# done once per flow in structure_prep() and cached per session.
#
# Conventions: shares are percent; "latest" is td$latest_date; the true flow
# total (td$total_tf sum12) is the denominator of every product share and HHI,
# so confidential trade (missing from the 5-digit table) counts as unallocated.

# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------

structure_flow_name <- function(flow_code) {
  if (identical(as.character(flow_code), "1")) "Imports" else "Exports"
}

# Shift first-of-month dates by k months.
structure_shift_months <- function(d, k) {
  lt <- as.POSIXlt(as.Date(d), tz = "UTC")
  idx <- (lt$year + 1900L) * 12L + lt$mon + as.integer(k)
  as.Date(sprintf("%04d-%02d-01", as.integer(idx %/% 12L), as.integer(idx %% 12L) + 1L))
}

# Mix a colour with white (amount 0 = unchanged, 1 = white).
structure_tint <- function(colour, amount) {
  v <- grDevices::col2rgb(colour)[, 1]
  m <- round(v + (255 - v) * amount)
  grDevices::rgb(m[1], m[2], m[3], maxColorValue = 255)
}

#' Sequential family of n colours derived from one colour (dark to light).
#' @param colour hex colour. @param n number of shades.
#' @return character vector of hex colours.
structure_shades <- function(colour, n) {
  if (n <= 0) return(character(0))
  v <- grDevices::col2rgb(colour)[, 1]
  dark <- round(v * 0.72)
  dark <- grDevices::rgb(dark[1], dark[2], dark[3], maxColorValue = 255)
  light <- structure_tint(colour, 0.72)
  if (n == 1) return(colour)
  grDevices::colorRampPalette(c(dark, colour, light))(n)
}

# Dates at which the 12-month rolling product / partner sums are evaluated:
# quarter-end months from 2015-03, the latest 72 months (KPI look-backs) and
# the latest month.
structure_eval_dates <- function(latest) {
  q <- seq(as.Date("2015-03-01"), latest, by = "3 months")
  lastm <- structure_shift_months(latest, -(0:72))
  sort(unique(c(q, lastm, latest)))
}

# Quarter-end subset (plus the latest month) used for the over-time charts.
structure_chart_dates <- function(latest) {
  q <- seq(as.Date("2015-03-01"), latest, by = "3 months")
  sort(unique(c(q, latest)))
}

# Complete monthly grid of one value column for several codes on `dates`.
structure_grid <- function(df, codes, dates, value = "value_eur") {
  g <- expand.grid(product_sitc5 = codes, date = dates, stringsAsFactors = FALSE)
  key_g <- paste(g$product_sitc5, as.numeric(g$date))
  agg <- stats::aggregate(df[[value]], by = list(product_sitc5 = df$product_sitc5, date = df$date), FUN = sum)
  key_a <- paste(agg$product_sitc5, as.numeric(agg$date))
  v <- agg$x[match(key_g, key_a)]
  v[is.na(v)] <- 0
  g$value <- v
  tibble::as_tibble(g)
}

# ---------------------------------------------------------------------------
# Cached heavy step
# ---------------------------------------------------------------------------

#' Per-flow pre-computation (computed once per flow, then reused).
#' Contains the flow's 5-digit rows, product -> group map, 12-month rolling sums
#' by product at the evaluation dates, product and partner concentration
#' statistics, and the group-level value / kg / unit-value table.
#' @param td bundle. @param flow_code "1" or "2".
#' @return list.
structure_prep <- function(td, flow_code) {
  flow_code <- as.character(flow_code)
  latest <- td$latest_date
  s5 <- td$monthly_sitc5[td$monthly_sitc5$flow == flow_code, , drop = FALSE]
  s5 <- tibble::as_tibble(s5)
  codes <- unique(s5$product_sitc5)
  grp <- stats::setNames(tm_assign_group(codes), codes)
  eval_dates <- structure_eval_dates(latest)
  roll <- tm_rolling_item_sums(s5, item = "product_sitc5", by = "flow", window = 12,
                               value = "value_eur", dates = eval_dates)
  tot <- td$total_tf[td$total_tf$flow == flow_code, c("date", "sum12", "value_eur")]
  names(tot) <- c("date", "total12", "total_month")

  conc <- tm_concentration(roll, item = "product_sitc5", by = "date", value = "value") |>
    dplyr::left_join(tot[, c("date", "total12")], by = "date") |>
    dplyr::mutate(
      ident_total = total,
      top1 = top1_share * ident_total / total12,
      top3 = top3_share * ident_total / total12,
      top10 = top10_share * ident_total / total12,
      hhi_products = hhi * (ident_total / total12)^2,
      n_eff = 10000 / hhi_products
    ) |>
    dplyr::select(date, top1, top3, top10, hhi_products, n_eff, ident_total, total12)

  pt <- td$monthly_partner_total[td$monthly_partner_total$flow == flow_code, , drop = FALSE]
  proll <- tm_rolling_item_sums(pt, item = "partner", by = "flow", window = 12,
                                value = "value_eur", dates = eval_dates)
  pconc <- tm_concentration(proll, item = "partner", by = "date", value = "value") |>
    dplyr::select(date, hhi_partners = hhi)

  # Identified value and kg by analytical group (value and kg from the same
  # 5-digit rows, so unit values are not distorted by confidential value).
  g_m <- tibble::tibble(group = unname(grp[s5$product_sitc5]), date = s5$date,
                        value_eur = s5$value_eur, quantity_kg = s5$quantity_kg) |>
    dplyr::group_by(group, date) |>
    dplyr::summarise(value_eur = sum(value_eur, na.rm = TRUE),
                     quantity_kg = sum(quantity_kg, na.rm = TRUE), .groups = "drop")
  g_c <- tm_complete_monthly(g_m, by = "group")
  uv <- tm_unit_values(g_c, by = "group", window = 12, base_year = 2019)

  # Share of the flow's 12m total, evaluated monthly over the full sample.
  list(flow = flow_code, latest = latest, s5 = s5, grp = grp, roll = roll, total = tot,
       conc = conc, pconc = pconc, uv = uv)
}

structure_get_prep <- function(td, flow_code, prep = NULL) {
  if (is.null(prep)) structure_prep(td, flow_code) else prep
}

# ---------------------------------------------------------------------------
# KPIs
# ---------------------------------------------------------------------------

# 12-month chemicals & pharma share (percent) by month for a flow.
structure_chem_share_series <- function(td, flow_code) {
  g <- td$group_tf[td$group_tf$flow == flow_code & td$group_tf$group %in% TM_CHEM_GROUPS, ]
  chem <- stats::aggregate(g$sum12, by = list(date = g$date), FUN = sum)
  tot <- td$total_tf[td$total_tf$flow == flow_code, c("date", "sum12")]
  m <- merge(chem, tot, by = "date")
  m <- m[order(m$date), ]
  tibble::tibble(date = m$date, share = 100 * m$x / m$sum12)
}

#' Headline numbers behind the four KPI tiles (latest 12 months).
#' @param td bundle. @param flow_code "1"/"2". @param prep optional cached structure_prep().
#' @return list(chem, top3, neff, largest, latest).
structure_kpis <- function(td, flow_code, prep = NULL) {
  prep <- structure_get_prep(td, flow_code, prep)
  L <- td$latest_date
  d12 <- structure_shift_months(L, -12)
  d60 <- structure_shift_months(L, -60)
  at <- function(df, col, d) {
    x <- df[[col]][df$date == d]
    if (length(x) == 0) NA_real_ else x[1]
  }
  cs <- structure_chem_share_series(td, flow_code)
  chem <- list(
    share = at(cs, "share", L),
    d12 = at(cs, "share", L) - at(cs, "share", d12),
    d60 = at(cs, "share", L) - at(cs, "share", d60),
    spark = utils::tail(cs$share[cs$date <= L], 60)
  )
  cc <- prep$conc
  rl <- prep$roll[prep$roll$date == L, ]
  rl <- rl[order(-rl$value), ]
  T12 <- at(prep$total, "total12", L)
  top3_codes <- rl$product_sitc5[1:3]
  top <- list(
    share = at(cc, "top3", L), d12 = at(cc, "top3", L) - at(cc, "top3", d12),
    codes = top3_codes
  )
  neff <- list(
    neff = at(cc, "n_eff", L), hhi = at(cc, "hhi_products", L),
    neff12 = at(cc, "n_eff", d12), hhi12 = at(cc, "hhi_products", d12)
  )
  largest <- list(
    code = rl$product_sitc5[1], value = rl$value[1], share = 100 * rl$value[1] / T12,
    desc = td$sitc_labels$sitc_desc[match(rl$product_sitc5[1], td$sitc_labels$sitc_code)],
    share12 = {
      r0 <- prep$roll[prep$roll$date == d12 & prep$roll$product_sitc5 == rl$product_sitc5[1], ]
      if (nrow(r0) == 0) NA_real_ else 100 * r0$value[1] / at(prep$total, "total12", d12)
    }
  )
  list(chem = chem, top3 = top, neff = neff, largest = largest, latest = L)
}

structure_kpi_cards <- function(td, flow_code, prep = NULL) {
  k <- structure_kpis(td, flow_code, prep)
  fl <- tolower(structure_flow_name(flow_code))
  col <- TM_COL$exports
  lab <- function(codes) paste(codes, collapse = ", ")
  bslib::layout_columns(
    col_widths = c(3, 3, 3, 3), fill = FALSE,
    tm_kpi_card(
      "Chemicals & pharma share", tm_fmt_share(k$chem$share),
      lines = list(tm_delta_span(k$chem$d12, "pp", label = "vs 12 months earlier"),
                   tm_delta_span(k$chem$d60, "pp", label = "vs 5 years earlier")),
      spark = tm_sparkline(k$chem$spark, colour = col), accent = TM_GROUP_COLOURS[["Pharmaceuticals"]],
      footnote = paste0("Share of ", fl, ", 12 months to ", tm_fmt_month(k$latest), "; sparkline: last 5 years.")
    ),
    tm_kpi_card(
      "Top 3 products", tm_fmt_share(k$top3$share),
      lines = list(tm_delta_span(k$top3$d12, "pp", label = "vs 12 months earlier"),
                   paste0("SITC ", lab(k$top3$codes))),
      accent = "#ae1122",
      footnote = paste0("Three largest 5-digit SITC products as a share of ", fl, ", 12 months.")
    ),
    tm_kpi_card(
      "Effective number of products", formatC(k$neff$neff, format = "f", digits = 1),
      lines = list(paste0("HHI ", formatC(k$neff$hhi, format = "f", digits = 0, big.mark = ",")),
                   tm_delta_span(k$neff$neff - k$neff$neff12, "", label = "vs 12 months earlier")),
      accent = "#4a4f57",
      footnote = "10,000 / HHI across 5-digit products: a flow spread evenly over N products would have the same concentration."
    ),
    tm_kpi_card(
      "Largest single product", k$largest$code,
      lines = list(tm_sitc_label(k$largest$code, td$sitc_labels, 38),
                   paste0(tm_fmt_share(k$largest$share), " of ", fl, " (",
                          tm_fmt_bn(k$largest$value), ")"),
                   tm_delta_span(k$largest$share - k$largest$share12, "pp", label = "vs 12 months earlier")),
      accent = "#a3402b",
      footnote = "12 months to the latest month; share of the flow's true total."
    )
  )
}

# ---------------------------------------------------------------------------
# Chart 1: composition over time
# ---------------------------------------------------------------------------

#' Shares (percent) of the ten analytical groups in the flow, 12-month rolling.
#' @return tibble(date, group, share)
structure_area_data <- function(td, flow_code) {
  g <- td$group_tf[td$group_tf$flow == flow_code, c("date", "group", "sum12")]
  tot <- td$total_tf[td$total_tf$flow == flow_code, c("date", "sum12")]
  names(tot)[2] <- "tot12"
  d <- merge(g, tot, by = "date")
  d <- d[!is.na(d$sum12) & !is.na(d$tot12) & d$tot12 > 0, ]
  out <- tibble::tibble(date = d$date, group = factor(as.character(d$group), levels = TM_GROUP_LEVELS),
                        share = 100 * d$sum12 / d$tot12)
  out[order(out$group, out$date), ]
}

structure_chart_area <- function(td, flow_code, prep = NULL) {
  d <- structure_area_data(td, flow_code)
  if (nrow(d) == 0) return(tm_empty_plot("No data"))
  tm_chart_area_share(d, share = "share", group = "group", palette = TM_GROUP_COLOURS)
}

# ---------------------------------------------------------------------------
# Chart 2: treemap
# ---------------------------------------------------------------------------

structure_other_label <- function(group) {
  ifelse(startsWith(group, "Other "), paste0("Remaining ", sub("^Other ", "", group)),
         paste0("Other ", group))
}

#' Treemap nodes for the latest 12 months: root -> analytical group -> SITC 3-digit.
#' Group totals equal td$group_tf sum12 (confidential / unallocated remainder
#' added as its own leaf); leaf values sum to the 12-month flow total.
#' @return tibble(id, label, parent, value, colour, kind, group)
structure_treemap_data <- function(td, flow_code, prep = NULL, top_n = 8) {
  prep <- structure_get_prep(td, flow_code, prep)
  L <- td$latest_date
  rl <- prep$roll[prep$roll$date == L & prep$roll$value > 0, ]
  rl$group <- unname(prep$grp[rl$product_sitc5])
  rl$code3 <- substr(rl$product_sitc5, 1, 3)
  agg <- rl |>
    dplyr::group_by(group, code3) |>
    dplyr::summarise(value = sum(value), .groups = "drop")
  gt <- td$group_tf[td$group_tf$flow == flow_code & td$group_tf$date == L, c("group", "sum12")]
  gt$group <- as.character(gt$group)
  fl <- structure_flow_name(flow_code)
  rows <- list()
  for (g in TM_GROUP_LEVELS) {
    total <- gt$sum12[gt$group == g]
    if (length(total) == 0 || is.na(total) || total <= 0) next
    a <- agg[agg$group == g, ]
    a <- a[order(-a$value), ]
    keep <- utils::head(a, top_n)
    rest <- sum(a$value) - sum(keep$value)
    ident <- sum(a$value)
    resid <- total - ident
    gcol <- TM_GROUP_COLOURS[[g]]
    leaves <- tibble::tibble(
      id = paste0("l:", g, ":", keep$code3),
      label = tm_sitc_label(keep$code3, td$sitc_labels, 34),
      value = keep$value, colour = gcol, kind = "leaf"
    )
    if (rest > 0.5) {
      leaves <- dplyr::bind_rows(leaves, tibble::tibble(
        id = paste0("o:", g), label = structure_other_label(g), value = rest,
        colour = structure_tint(gcol, 0.35), kind = "other"))
    }
    if (resid > 0.5) {
      leaves <- dplyr::bind_rows(leaves, tibble::tibble(
        id = paste0("c:", g), label = "Confidential / unallocated", value = resid,
        colour = structure_tint(gcol, 0.6), kind = "confidential"))
    } else if (resid < -0.5) {
      # Negative remainder (source rounding): net it off the largest leaf so
      # the branch still equals the group total.
      leaves$value[1] <- leaves$value[1] + resid
    }
    leaves$parent <- paste0("g:", g)
    leaves$group <- g
    node <- tibble::tibble(id = paste0("g:", g), label = g, parent = "root", value = total,
                           colour = gcol, kind = "group", group = g)
    rows[[length(rows) + 1]] <- dplyr::bind_rows(node, leaves)
  }
  body <- dplyr::bind_rows(rows)
  root <- tibble::tibble(id = "root", label = fl, parent = "", value = sum(body$value[body$kind == "group"]),
                         colour = "#eef0ee", kind = "root", group = NA_character_)
  dplyr::bind_rows(root, body)
}

structure_chart_treemap <- function(td, flow_code, prep = NULL) {
  d <- structure_treemap_data(td, flow_code, prep)
  tm_chart_treemap(labels = d$label, parents = d$parent, values = d$value, ids = d$id,
                   colours = d$colour)
}

# ---------------------------------------------------------------------------
# Charts 3 and 4: concentration over time
# ---------------------------------------------------------------------------

#' Concentration statistics at the quarterly evaluation dates (12-month windows).
#' @return tibble(date, top1, top3, top10, hhi_products, hhi_partners, n_eff)
structure_concentration <- function(td, flow_code, prep = NULL) {
  prep <- structure_get_prep(td, flow_code, prep)
  keep <- structure_chart_dates(td$latest_date)
  d <- prep$conc[prep$conc$date %in% keep, ]
  d <- dplyr::left_join(d, prep$pconc, by = "date")
  d[order(d$date), c("date", "top1", "top3", "top10", "hhi_products", "hhi_partners", "n_eff")]
}

structure_chart_conc <- function(td, flow_code, prep = NULL) {
  d <- structure_concentration(td, flow_code, prep)
  long <- tibble::tibble(
    date = rep(d$date, 3),
    series = factor(rep(c("Top 1 product", "Top 3 products", "Top 10 products"), each = nrow(d)),
                    levels = c("Top 1 product", "Top 3 products", "Top 10 products")),
    share = c(d$top1, d$top3, d$top10)
  )
  pal <- c("Top 1 product" = "#ab9965", "Top 3 products" = "#2e7d6f", "Top 10 products" = "#004d44")
  tm_chart_lines(long, y = "share", colour_by = "series", palette = pal, y_suffix = "%",
                 y_title = paste0("Share of ", tolower(structure_flow_name(flow_code)), " (%)"))
}

structure_chart_hhi <- function(td, flow_code, prep = NULL) {
  d <- structure_concentration(td, flow_code, prep)
  long <- tibble::tibble(
    date = rep(d$date, 2),
    series = factor(rep(c("5-digit products", "Partner countries"), each = nrow(d)),
                    levels = c("5-digit products", "Partner countries")),
    hhi = c(d$hhi_products, d$hhi_partners)
  )
  long <- long[!is.na(long$hhi), ]
  pal <- c("5-digit products" = "#004d44", "Partner countries" = "#2c55a2")
  p <- tm_chart_lines(long, y = "hhi", colour_by = "series", palette = pal,
                      y_title = "HHI (0-10,000)")
  top <- max(c(long$hhi, 2600), na.rm = TRUE) * 1.06
  ref <- function(y) list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = y, y1 = y,
                          line = list(color = "rgba(107,114,128,0.55)", width = 1, dash = "dash"),
                          layer = "below")
  plotly::layout(p, shapes = list(ref(1500), ref(2500)),
                 yaxis = list(range = c(0, top), tickformat = ",.0f"))
}

# ---------------------------------------------------------------------------
# Chart 5: inside a product group
# ---------------------------------------------------------------------------

#' Top products (by latest 12m value) of one analytical group.
#' @return tibble(product_sitc5, value) ordered by value, at most n rows.
structure_group_top <- function(td, flow_code, group, n = 6, prep = NULL) {
  prep <- structure_get_prep(td, flow_code, prep)
  rl <- prep$roll[prep$roll$date == td$latest_date, ]
  rl <- rl[unname(prep$grp[rl$product_sitc5]) == group & rl$value > 0, c("product_sitc5", "value")]
  utils::head(rl[order(-rl$value), ], n)
}

#' 3-month average monthly value (EUR) of the top products of a group plus the rest.
#' @return tibble(date, series, value_eur) with `series` a factor (products then "Other <group>").
structure_group_series <- function(td, flow_code, group, n = 6, prep = NULL) {
  prep <- structure_get_prep(td, flow_code, prep)
  top <- structure_group_top(td, flow_code, group, n, prep)
  dates <- sort(unique(td$total_tf$date[td$total_tf$flow == flow_code]))
  lab <- tm_sitc_label(top$product_sitc5, td$sitc_labels, 45)
  oth <- structure_other_label(group)
  gtf <- td$group_tf[td$group_tf$flow == flow_code & as.character(td$group_tf$group) == group, c("date", "ma3")]
  gtf <- gtf[match(dates, gtf$date), ]
  prods <- NULL
  if (nrow(top) > 0) {
    s5 <- prep$s5[prep$s5$product_sitc5 %in% top$product_sitc5, ]
    gr <- structure_grid(s5, top$product_sitc5, dates)
    gr$value_eur <- gr$value
    gr$flow <- flow_code
    tf <- tm_add_transforms(gr, by = "product_sitc5")
    prods <- tibble::tibble(date = tf$date, series = lab[match(tf$product_sitc5, top$product_sitc5)],
                            value_eur = tf$ma3)
  }
  topsum <- if (is.null(prods)) rep(0, length(dates)) else
    as.numeric(tapply(prods$value_eur, prods$date, sum)[as.character(dates)])
  other <- tibble::tibble(date = dates, series = oth, value_eur = pmax(gtf$ma3 - topsum, 0))
  out <- dplyr::bind_rows(prods, other)
  out <- out[!is.na(out$value_eur), ]
  out$series <- factor(out$series, levels = c(lab, oth))
  out[order(out$series, out$date), ]
}

structure_chart_group <- function(td, flow_code, group = "Pharmaceuticals", prep = NULL) {
  d <- structure_group_series(td, flow_code, group, 6, prep)
  if (nrow(d) == 0) return(tm_empty_plot("No data for this group"))
  lv <- levels(d$series)
  oth <- lv[length(lv)]
  cols <- c(structure_shades(TM_GROUP_COLOURS[[group]], length(lv) - 1), "#c8cbd0")
  names(cols) <- lv
  p <- plotly::plot_ly()
  for (s in lv) {
    x <- d[d$series == s, ]
    p <- plotly::add_trace(p, type = "bar", x = x$date, y = x$value_eur / 1e9, name = s,
                           marker = list(color = cols[[s]]),
                           hovertemplate = paste0(intToUtf8(0x20AC), "%{y:,.2f}bn<extra>", s, "</extra>"))
  }
  L <- td$latest_date
  xmin <- structure_shift_months(L, -59) - 16
  xmax <- L + 20
  p <- tm_layout(p, y_title = "EUR bn per month (3-month average)", legend = "right",
                 range_buttons = TRUE, x_range = c(xmin, xmax))
  p <- plotly::layout(p, barmode = "stack", bargap = 0.12, xaxis = list(hoverformat = "%b %Y"))
  tm_add_events(p, x_min = d$date[1], x_max = L)
}

# ---------------------------------------------------------------------------
# Chart 6: one product against everything else
# ---------------------------------------------------------------------------

#' Top n 5-digit products of a flow by latest 12m value.
#' @return tibble(product_sitc5, value, label).
structure_top_codes <- function(td, flow_code, n = 25, prep = NULL, max_chars = 55) {
  prep <- structure_get_prep(td, flow_code, prep)
  rl <- prep$roll[prep$roll$date == td$latest_date & prep$roll$value > 0, c("product_sitc5", "value")]
  rl <- utils::head(rl[order(-rl$value), ], n)
  rl$label <- tm_sitc_label(rl$product_sitc5, td$sitc_labels, max_chars)
  rl
}

#' Contributions to 3-month y/y growth of the flow: selected product vs the rest.
#' @return tibble(flow, date, component, level, delta, contrib_pp, total_growth_pct), NA windows removed.
structure_product_contrib <- function(td, flow_code, code, prep = NULL) {
  prep <- structure_get_prep(td, flow_code, prep)
  tot <- td$total_tf[td$total_tf$flow == flow_code, c("date", "value_eur")]
  tot <- tot[order(tot$date), ]
  s5 <- prep$s5[prep$s5$product_sitc5 == code, ]
  gr <- structure_grid(s5, code, tot$date)
  prod_v <- gr$value[match(tot$date, gr$date)]
  lab <- tm_sitc_label(code, td$sitc_labels, 40)
  comp <- c(lab, "All other goods")
  df <- tibble::tibble(
    flow = flow_code,
    date = rep(tot$date, 2),
    component = factor(rep(comp, each = nrow(tot)), levels = comp),
    value_eur = c(prod_v, tot$value_eur - prod_v)
  )
  out <- tm_contributions(df, component = "component", basis = "3m", by = "flow")
  out <- out[!is.na(out$contrib_pp) & !is.na(out$total_growth_pct), ]
  out$component <- factor(as.character(out$component), levels = comp)
  out
}

structure_chart_product <- function(td, flow_code, code, prep = NULL) {
  d <- structure_product_contrib(td, flow_code, code, prep)
  if (nrow(d) == 0) return(tm_empty_plot("No data"))
  lv <- levels(d$component)
  g <- unname(structure_get_prep(td, flow_code, prep)$grp[code])
  pc <- if (is.na(g) || g == "Other goods") "#ae1122" else TM_GROUP_COLOURS[[g]]
  pal <- stats::setNames(c(pc, "#c8cbd0"), lv)
  p <- tm_chart_contrib(d, component = "component", palette = pal,
                        total_name = paste0("Total ", tolower(structure_flow_name(flow_code)), " growth (%)"))
  L <- td$latest_date
  p <- plotly::layout(p, xaxis = list(range = as.list(as.character(c(structure_shift_months(L, -59) - 16, L + 20)))))
  tm_add_events(p, x_min = min(d$date), x_max = L)
}

# ---------------------------------------------------------------------------
# Chart 7: value, volume, unit value
# ---------------------------------------------------------------------------

#' Value / volume / unit-value indices (12m rolling, 2019 = 100) for one group.
#' Built from the identified 5-digit rows so value and kg are consistent.
#' @return tibble(date, series, index) plus attribute "kg_cover" (share of latest 12m value with kg).
structure_uv_data <- function(td, flow_code, group, prep = NULL) {
  prep <- structure_get_prep(td, flow_code, prep)
  u <- prep$uv[as.character(prep$uv$group) == group, ]
  u <- u[order(u$date), ]
  lv <- c("Value (EUR)", "Volume (kg)", "Unit value (EUR per kg)")
  out <- tibble::tibble(
    date = rep(u$date, 3),
    series = factor(rep(lv, each = nrow(u)), levels = lv),
    index = c(u$value_index, u$kg_index, u$uv_index)
  )
  out <- out[!is.na(out$index), ]
  # Coverage of kg in the latest 12 months.
  L <- td$latest_date
  win <- seq(structure_shift_months(L, -11), L, by = "month")
  s <- prep$s5[prep$s5$date %in% win & unname(prep$grp[prep$s5$product_sitc5]) == group, ]
  cover <- if (nrow(s) == 0 || sum(s$value_eur) <= 0) NA_real_ else
    100 * sum(s$value_eur[!is.na(s$quantity_kg) & s$quantity_kg > 0]) / sum(s$value_eur)
  attr(out, "kg_cover") <- cover
  out
}

structure_chart_uv <- function(td, flow_code, group = "Pharmaceuticals", prep = NULL) {
  d <- structure_uv_data(td, flow_code, group, prep)
  if (nrow(d) == 0) return(tm_empty_plot("No volume data for this group"))
  pal <- c("Value (EUR)" = "#004d44", "Volume (kg)" = "#7a7f87", "Unit value (EUR per kg)" = "#9a7b2f")
  p <- tm_chart_lines(d, y = "index", colour_by = "series", palette = pal,
                      y_title = "Index, 2019 = 100")
  ref <- list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = 100, y1 = 100,
              line = list(color = "rgba(107,114,128,0.5)", width = 1, dash = "dot"), layer = "below")
  plotly::layout(p, shapes = list(ref))
}

structure_uv_subtitle <- function(td, flow_code, group, prep = NULL) {
  d <- structure_uv_data(td, flow_code, group, prep)
  cover <- attr(d, "kg_cover")
  note <- if (!is.na(cover) && cover < 90)
    paste0(" Kilograms are reported for only ", formatC(cover, format = "f", digits = 0), "% of this group's value.") else ""
  paste0("12-month rolling, 2019 = 100. Unit values are value divided by kilograms: they reflect ",
         "product mix and transfer pricing as well as prices, and are not a price index.", note)
}

# ---------------------------------------------------------------------------
# Top 25 table
# ---------------------------------------------------------------------------

#' Top n 5-digit products table (numeric columns).
#' @return tibble(rank, code, description, group, value_bn, share, cum_share, growth, share_5y)
structure_top_table <- function(td, flow_code, n = 25, prep = NULL) {
  prep <- structure_get_prep(td, flow_code, prep)
  L <- td$latest_date
  d12 <- structure_shift_months(L, -12)
  d60 <- structure_shift_months(L, -60)
  T0 <- prep$total$total12[prep$total$date == L]
  T12 <- prep$total$total12[prep$total$date == d12]
  T60 <- prep$total$total12[prep$total$date == d60]
  r0 <- prep$roll[prep$roll$date == L & prep$roll$value > 0, c("product_sitc5", "value")]
  r0 <- utils::head(r0[order(-r0$value), ], n)
  v12 <- prep$roll$value[prep$roll$date == d12][match(r0$product_sitc5, prep$roll$product_sitc5[prep$roll$date == d12])]
  v60 <- prep$roll$value[prep$roll$date == d60][match(r0$product_sitc5, prep$roll$product_sitc5[prep$roll$date == d60])]
  v12[is.na(v12)] <- 0; v60[is.na(v60)] <- 0
  tibble::tibble(
    rank = seq_len(nrow(r0)),
    code = r0$product_sitc5,
    description = td$sitc_labels$sitc_desc[match(r0$product_sitc5, td$sitc_labels$sitc_code)],
    group = unname(prep$grp[r0$product_sitc5]),
    value_bn = r0$value / 1e9,
    share = 100 * r0$value / T0,
    cum_share = cumsum(100 * r0$value / T0),
    growth = ifelse(v12 > 0, 100 * (r0$value / v12 - 1), NA_real_),
    share_5y = 100 * v60 / T60
  )
}

structure_table_widget <- function(td, flow_code, prep = NULL) {
  d <- structure_top_table(td, flow_code, 25, prep)
  fl <- structure_flow_name(flow_code)
  d$description[is.na(d$description)] <- ""
  num <- c("value_bn", "share", "cum_share", "growth", "share_5y")
  w <- DT::datatable(
    d, rownames = FALSE, class = "trade-dt row-border hover compact",
    colnames = c("Rank", "SITC", "Product", "Group", "Value, 12m (EUR bn)",
                 paste0("Share of ", tolower(fl), " (%)"), "Cumulative share (%)",
                 "Growth vs prior 12m (%)", "Share 5 years earlier (%)"),
    options = list(dom = "ft", paging = FALSE, ordering = TRUE, scrollX = TRUE,
                   columnDefs = list(list(className = "dt-right", targets = 4:8),
                                     list(className = "dt-left", targets = 1:3)))
  )
  w <- DT::formatRound(w, "value_bn", digits = 2)
  DT::formatRound(w, c("share", "cum_share", "growth", "share_5y"), digits = 1)
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

mod_structure_ui <- function(id) {
  ns <- shiny::NS(id)
  tl <- function(x) shiny::uiOutput(ns(x), inline = TRUE)
  groups <- as.character(TM_GROUPS$group)
  htmltools::tagList(
    tm_page_css(),
    shiny::uiOutput(ns("kpis")),
    bslib::layout_columns(
      col_widths = c(7, 5), fill = FALSE,
      tm_chart_card(tl("t_area"),
                    "Ten analytical groups as a share of the flow, 12-month rolling sums.",
                    plotly::plotlyOutput(ns("area"), height = "400px")),
      tm_chart_card(tl("t_tree"),
                    "Latest 12 months. Group tiles sum to the flow total; confidential trade is shown separately.",
                    plotly::plotlyOutput(ns("treemap"), height = "400px"))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6), fill = FALSE,
      tm_chart_card(tl("t_conc"),
                    "Share of the three tiers of 5-digit SITC products in the flow's true total, 12-month rolling, quarterly.",
                    plotly::plotlyOutput(ns("conc"), height = "360px")),
      tm_chart_card(tl("t_hhi"),
                    paste0("Herfindahl-Hirschman index, 12-month rolling, quarterly. Dashed lines: 1,500 (moderate) and ",
                           "2,500 (high concentration). Partners include a few tiny non-country codes."),
                    plotly::plotlyOutput(ns("hhi"), height = "360px"))
    ),
    tm_chart_card(tl("t_group"),
                  "Top six 5-digit products by latest 12-month value, 3-month average of monthly values.",
                  shiny::selectInput(ns("group"), "Product group", choices = groups,
                                     selected = "Pharmaceuticals", width = "320px"),
                  plotly::plotlyOutput(ns("group_chart"), height = "400px")),
    bslib::layout_columns(
      col_widths = c(6, 6), fill = FALSE,
      tm_chart_card(tl("t_prod"),
                    "Contributions to 3-month year-on-year growth of the whole flow, percentage points.",
                    shiny::selectInput(ns("product"), "Product (top 25 by latest 12-month value)",
                                       choices = character(0), width = "100%"),
                    plotly::plotlyOutput(ns("prod_chart"), height = "360px")),
      tm_chart_card(tl("t_uv"), shiny::uiOutput(ns("sub_uv")),
                    plotly::plotlyOutput(ns("uv_chart"), height = "420px"))
    ),
    tm_chart_card(tl("t_table"), "Ranked by value in the latest 12 months; share is of the flow's true total.",
                  DT::DTOutput(ns("table")))
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

mod_structure_server <- function(id, td, flow) {
  shiny::moduleServer(id, function(input, output, session) {
    cache <- new.env(parent = emptyenv())
    prep <- shiny::reactive({
      f <- as.character(flow())
      if (is.null(cache[[f]])) cache[[f]] <- structure_prep(td, f)
      cache[[f]]
    })
    fl <- shiny::reactive(structure_flow_name(flow()))
    fl_lc <- shiny::reactive(tolower(fl()))

    group_sel <- shiny::reactive({
      g <- input$group
      if (is.null(g) || !g %in% TM_GROUP_LEVELS) "Pharmaceuticals" else g
    })
    top25 <- shiny::reactive(structure_top_codes(td, as.character(flow()), 25, prep()))
    product_sel <- shiny::reactive({
      tp <- top25()
      p <- input$product
      if (!is.null(p) && p %in% tp$product_sitc5) p
      else if ("54154" %in% tp$product_sitc5) "54154"
      else tp$product_sitc5[1]
    })
    shiny::observe({
      tp <- top25()
      ch <- stats::setNames(tp$product_sitc5, tp$label)
      cur <- shiny::isolate(input$product)
      sel <- if (!is.null(cur) && cur %in% tp$product_sitc5) cur
             else if ("54154" %in% tp$product_sitc5) "54154" else tp$product_sitc5[1]
      shiny::updateSelectInput(session, "product", choices = ch, selected = sel)
    })

    output$kpis <- shiny::renderUI(structure_kpi_cards(td, as.character(flow()), prep()))

    output$t_area <- shiny::renderUI(paste0("Composition of ", fl_lc(), " over time"))
    output$area <- plotly::renderPlotly(structure_chart_area(td, as.character(flow()), prep()))

    output$t_tree <- shiny::renderUI(paste0("What we ", if (flow() == "2") "export" else "import", " now: ",
                                            tm_fmt_month(td$latest_date), ", 12 months"))
    output$treemap <- plotly::renderPlotly(structure_chart_treemap(td, as.character(flow()), prep()))

    output$t_conc <- shiny::renderUI(paste0("Product concentration over time: ", fl_lc()))
    output$conc <- plotly::renderPlotly(structure_chart_conc(td, as.character(flow()), prep()))

    output$t_hhi <- shiny::renderUI(paste0("Concentration of ", fl_lc(), ": products versus markets"))
    output$hhi <- plotly::renderPlotly(structure_chart_hhi(td, as.character(flow()), prep()))

    output$t_group <- shiny::renderUI(paste0("Inside a product group: ", group_sel(), " (", fl_lc(), ")"))
    output$group_chart <- plotly::renderPlotly(
      structure_chart_group(td, as.character(flow()), group_sel(), prep()))

    output$t_prod <- shiny::renderUI(paste0("One product against everything else: ", fl_lc()))
    output$prod_chart <- plotly::renderPlotly(
      structure_chart_product(td, as.character(flow()), product_sel(), prep()))

    output$t_uv <- shiny::renderUI(paste0("Value, volume and unit value: ", group_sel(), " (", fl_lc(), ")"))
    output$sub_uv <- shiny::renderUI(structure_uv_subtitle(td, as.character(flow()), group_sel(), prep()))
    output$uv_chart <- plotly::renderPlotly(structure_chart_uv(td, as.character(flow()), group_sel(), prep()))

    output$t_table <- shiny::renderUI(paste0("Top 25 products: ", fl_lc(), ", 12 months to ",
                                             tm_fmt_month(td$latest_date)))
    output$table <- DT::renderDT(structure_table_widget(td, as.character(flow()), prep()))
  })
}
