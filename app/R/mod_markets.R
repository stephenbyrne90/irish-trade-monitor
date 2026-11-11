# Markets page module: geographic exposure of Irish goods trade.
#
# Pure builders (markets_*) take the analytics bundle `td` and a flow code
# ("1" imports, "2" exports) and return plotly objects / tibbles, so they can be
# tested without Shiny. The module wires them to the UI.
#
# Auto-sourced by Shiny before app.R attaches packages: top level only defines
# functions and base-R constants; everything inside functions is namespace
# qualified (except the native pipe). ASCII only.

# ---------------------------------------------------------------------------
# Constants and small helpers
# ---------------------------------------------------------------------------

# Private cache for the partner-level transforms (keyed by a cheap fingerprint).
.markets_cache <- new.env(parent = emptyenv())

MARKETS_BASIS_CHOICES <- c("3-month y/y" = "3m", "Monthly y/y" = "m",
                           "12-month y/y" = "12m", "Year-to-date" = "ytd")

# Non-country partner codes: Q* (stores, unspecified, ...) except Qatar, and X*
# (Ceuta, Melilla, Serbia, Kosovo ...), as listed by the brief.
markets_is_country <- function(code) {
  code <- as.character(code)
  !((grepl("^Q", code) & code != "QA") | grepl("^X", code))
}

# Date shifted by `k` months (first-of-month in, first-of-month out).
markets_shift <- function(date, k) {
  vapply(as.Date(date), function(d) {
    as.numeric(seq(d, by = paste(k, "months"), length.out = 2)[2])
  }, numeric(1)) |> as.Date(origin = "1970-01-01")
}

markets_flow_name <- function(flow) unname(TM_FLOW_LABELS[as.character(flow)])

markets_name <- function(td, code) tm_partner_label(code, td$partner_labels)

markets_ordinal <- function(n) {
  n <- as.integer(n)
  suf <- if (n %% 100L %in% 11:13) "th" else switch(as.character(n %% 10L),
    "1" = "st", "2" = "nd", "3" = "rd", "th")
  paste0(n, suf)
}

# ---------------------------------------------------------------------------
# Partner-level transforms (cached)
# ---------------------------------------------------------------------------

#' Complete partner x flow x month grid with rolling transforms.
#' Cached per bundle fingerprint; ~74k rows.
#' @param td bundle.
#' @return tibble(flow, flow_label, partner, date, year, month, value_eur, sum3,
#'   sum12, ma3, ma12, yoy_pct, yoy3_pct, yoy12_pct, mom3_pct, ytd, ytd_yoy_pct).
markets_partner_tf <- function(td) {
  d <- td$monthly_partner_total
  key <- paste(nrow(d), format(td$latest_date), round(sum(d$value_eur)), sep = "|")
  hit <- .markets_cache[["pt"]]
  if (!is.null(hit) && identical(hit$key, key)) return(hit$value)
  d$flow <- as.character(d$flow)
  cm <- tm_complete_monthly(d, by = c("flow", "partner"), value_cols = "value_eur")
  pt <- tm_add_transforms(cm, by = c("flow", "partner"))
  assign("pt", list(key = key, value = pt), envir = .markets_cache)
  pt
}

# One row per country-or-code for `flow` at the latest date, with the measures
# used by the scatter, heatmap, table and balances.
#' @param td bundle. @param flow "1"/"2". @param countries_only drop Q*/X* codes.
#' @return tibble(partner, country, value12, share, share_5y, growth12, yoy3,
#'   exports12, imports12, balance12), sorted by value12 descending.
markets_country_table <- function(td, flow, countries_only = TRUE) {
  pt <- markets_partner_tf(td)
  ld <- td$latest_date
  d5 <- markets_shift(ld, -60)
  cur <- pt[pt$date == ld & pt$flow == flow, ]
  old <- pt[pt$date == d5 & pt$flow == flow, ]
  tot <- td$total_tf
  tot_cur <- tot$sum12[tot$flow == flow & tot$date == ld]
  tot_old <- tot$sum12[tot$flow == flow & tot$date == d5]
  ex <- pt[pt$date == ld & pt$flow == "2", c("partner", "sum12")]
  im <- pt[pt$date == ld & pt$flow == "1", c("partner", "sum12")]
  out <- tibble::tibble(
    partner = cur$partner,
    value12 = cur$sum12,
    growth12 = cur$yoy12_pct,
    yoy3 = cur$yoy3_pct
  )
  out$share <- 100 * out$value12 / tot_cur
  v_old <- old$sum12[match(out$partner, old$partner)]
  out$share_5y <- if (length(tot_old) == 1 && !is.na(tot_old) && tot_old > 0) 100 * v_old / tot_old else NA_real_
  out$exports12 <- ex$sum12[match(out$partner, ex$partner)]
  out$imports12 <- im$sum12[match(out$partner, im$partner)]
  out$exports12[is.na(out$exports12)] <- 0
  out$imports12[is.na(out$imports12)] <- 0
  out$balance12 <- out$exports12 - out$imports12
  out$country <- markets_name(td, out$partner)
  if (countries_only) out <- out[markets_is_country(out$partner), ]
  out <- out[!is.na(out$value12), ]
  out <- out[order(-out$value12), ]
  out[, c("partner", "country", "value12", "share", "share_5y", "growth12", "yoy3",
          "exports12", "imports12", "balance12")]
}

# ---------------------------------------------------------------------------
# KPI data
# ---------------------------------------------------------------------------

#' Share (percent) of a partner group in the flow's 12-month total, by month.
#' @return tibble(date, share).
markets_group_share_series <- function(td, flow, group) {
  g <- td$partner_group_tf
  g <- g[g$flow == flow & as.character(g$partner_group) == group, c("date", "sum12")]
  t <- td$total_tf
  t <- t[t$flow == flow, c("date", "sum12")]
  j <- merge(g, t, by = "date", suffixes = c("_g", "_t"))
  j <- j[order(j$date), ]
  j <- j[j$date <= td$latest_date, ]
  out <- tibble::tibble(date = j$date, share = 100 * j$sum12_g / j$sum12_t)
  out[!is.na(out$share), ]
}

#' Exports, imports and balance with one partner as 12-month sums, by month.
#' Both flows regardless of the page's flow switch.
#' @return tibble(date, exports, imports, balance); NA before 12 months exist.
markets_bilateral_series <- function(td, partner) {
  pt <- markets_partner_tf(td)
  pt <- pt[pt$partner == partner & pt$date <= td$latest_date, ]
  ex <- pt[pt$flow == "2", c("date", "sum12")]
  im <- pt[pt$flow == "1", c("date", "sum12")]
  dates <- sort(unique(c(ex$date, im$date)))
  out <- tibble::tibble(
    date = dates,
    exports = ex$sum12[match(dates, ex$date)],
    imports = im$sum12[match(dates, im$date)]
  )
  out$balance <- out$exports - out$imports
  out[!is.na(out$balance), ]
}

# Latest value, change vs 12 months earlier and trailing series of a series.
.markets_latest_change <- function(dates, values, ld, n_spark = 36) {
  i <- match(ld, dates)
  j <- match(markets_shift(ld, -12), dates)
  list(
    value = if (is.na(i)) NA_real_ else values[i],
    change = if (is.na(i) || is.na(j)) NA_real_ else values[i] - values[j],
    spark = utils::tail(values[dates <= ld], n_spark)
  )
}

#' Data behind the four KPI tiles.
#' @return list(us, ea, gb, bal), each list(value, change, spark); shares in
#'   percent / pp, balance in EUR.
markets_kpi_data <- function(td, flow) {
  ld <- td$latest_date
  sh <- function(g) {
    s <- markets_group_share_series(td, flow, g)
    .markets_latest_change(s$date, s$share, ld)
  }
  b <- markets_bilateral_series(td, "US")
  list(us = sh("United States"), ea = sh("Euro area"), gb = sh("Great Britain & NI"),
       bal = .markets_latest_change(b$date, b$balance, ld))
}

markets_kpi_ui <- function(td, flow, kpi, which) {
  k <- markets_kpi_data(td, flow)[[which]]
  fl <- tolower(markets_flow_name(flow))
  lab12 <- paste("vs", tm_fmt_month(markets_shift(td$latest_date, -12)))
  if (which == "bal") {
    tm_kpi_card(
      "Goods balance with the US", tm_fmt_bn(k$value),
      lines = list(tm_delta_span(k$change / 1e9, suffix = "bn", digits = 1, label = lab12)),
      spark = tm_sparkline(k$spark / 1e9, colour = TM_PARTNER_GROUP_COLOURS[["United States"]]),
      accent = TM_PARTNER_GROUP_COLOURS[["United States"]],
      footnote = "Exports minus imports, 12-month total")
  } else {
    ttl <- c(us = "United States share", ea = "Euro area share", gb = "Great Britain & NI share")[[which]]
    grp <- c(us = "United States", ea = "Euro area", gb = "Great Britain & NI")[[which]]
    col <- TM_PARTNER_GROUP_COLOURS[[grp]]
    tm_kpi_card(
      ttl, tm_fmt_share(k$value),
      lines = list(tm_delta_span(k$change, suffix = "pp", digits = 1, label = lab12)),
      spark = tm_sparkline(k$spark, colour = col), accent = col,
      footnote = paste0("Share of ", fl, ", 12 months to ", tm_fmt_month(td$latest_date)))
  }
}

# ---------------------------------------------------------------------------
# Chart builders
# ---------------------------------------------------------------------------

#' Partner-group shares of the flow on 12-month sums (stacked area).
markets_area_share <- function(td, flow) {
  g <- td$partner_group_tf
  g <- g[g$flow == flow & g$date <= td$latest_date & !is.na(g$sum12), ]
  t <- td$total_tf
  t <- t[t$flow == flow, c("date", "sum12")]
  g$share <- 100 * g$sum12 / t$sum12[match(g$date, t$date)]
  g <- g[!is.na(g$share), ]
  tm_chart_area_share(g, x = "date", share = "share", group = "partner_group",
                      palette = TM_PARTNER_GROUP_COLOURS)
}

#' Data for the front-loading chart: 3-month average of the group and of all
#' other markets, each indexed to its 2024 average = 100.
#' @return tibble(date, series [factor], ma3, index).
markets_frontload_data <- function(td, flow, group = "United States") {
  g <- td$partner_group_tf
  g <- g[g$flow == flow & as.character(g$partner_group) == group, c("date", "ma3")]
  t <- td$total_tf
  t <- t[t$flow == flow, c("date", "ma3")]
  j <- merge(t, g, by = "date", suffixes = c("_t", "_g"))
  j <- j[order(j$date), ]
  j <- j[j$date <= td$latest_date & !is.na(j$ma3_t) & !is.na(j$ma3_g), ]
  j$ma3_o <- j$ma3_t - j$ma3_g
  yr <- as.integer(format(j$date, "%Y"))
  idx <- function(x) {
    b <- mean(x[yr == 2024])
    if (!is.finite(b) || b <= 0) rep(NA_real_, length(x)) else 100 * x / b
  }
  lv <- c(group, "All other markets")
  out <- rbind(
    tibble::tibble(date = j$date, series = group, ma3 = j$ma3_g, index = idx(j$ma3_g)),
    tibble::tibble(date = j$date, series = "All other markets", ma3 = j$ma3_o, index = idx(j$ma3_o))
  )
  out$series <- factor(out$series, levels = lv)
  out
}

markets_frontload <- function(td, flow, group = "United States", from = "2022-01-01") {
  d <- markets_frontload_data(td, flow, group)
  d <- d[!is.na(d$index), ]
  if (nrow(d) == 0) return(tm_empty_plot("No data for this selection"))
  pal <- stats::setNames(
    c(TM_PARTNER_GROUP_COLOURS[[group]], "#5c6270"), c(group, "All other markets"))
  tm_chart_lines(d, y = "index", colour_by = "series", palette = pal,
                 y_title = "Index, 2024 average = 100") |>
    tm_add_events() |>
    plotly::layout(xaxis = list(range = list(as.character(from), as.character(td$latest_date))))
}

#' Contributions of partner groups to growth in the flow (last 36 months).
#' @return tibble(flow, date, partner_group, level, delta, contrib_pp, total_growth_pct).
markets_contrib_data <- function(td, flow, basis = "3m", months = 36) {
  pgm <- td$partner_group_monthly
  pgm <- pgm[pgm$flow == flow, ]
  ct <- tm_contributions(pgm, component = "partner_group", basis = basis, by = "flow")
  ct <- ct[!is.na(ct$contrib_pp) & !is.na(ct$total_growth_pct) & ct$date <= td$latest_date, ]
  ct[ct$date > markets_shift(td$latest_date, -months), ]
}

markets_contrib <- function(td, flow, basis = "3m", months = 36) {
  d <- markets_contrib_data(td, flow, basis, months)
  if (nrow(d) == 0) return(tm_empty_plot("No data for this selection"))
  tm_chart_contrib(d, component = "partner_group", palette = TM_PARTNER_GROUP_COLOURS,
                   total_name = "Total growth (%)", y_title = "Percentage points")
}

#' Top-15 partner countries by 12-month value, 3-month y/y over 24 months.
#' @return tibble(partner, country [factor, rank order], date, yoy3_pct).
markets_heatmap_data <- function(td, flow, n = 15, months = 24) {
  ct <- utils::head(markets_country_table(td, flow), n)
  pt <- markets_partner_tf(td)
  d <- pt[pt$flow == flow & pt$partner %in% ct$partner & pt$date <= td$latest_date &
            pt$date > markets_shift(td$latest_date, -months), c("partner", "date", "yoy3_pct")]
  d$country <- factor(ct$country[match(d$partner, ct$partner)], levels = ct$country)
  d[order(d$country, d$date), c("partner", "country", "date", "yoy3_pct")]
}

markets_heatmap <- function(td, flow, n = 15, months = 24, cap = 50) {
  d <- markets_heatmap_data(td, flow, n, months)
  if (nrow(d) == 0) return(tm_empty_plot("No data"))
  tm_chart_heatmap(d, x = "date", y = "country", z = "yoy3_pct", cap = cap)
}

#' Data for the size-versus-momentum bubbles (top 20 countries).
#' @return tibble(partner, country, share, value12, yoy3, y_plot).
markets_scatter_data <- function(td, flow, n = 20, clip = 60) {
  ct <- utils::head(markets_country_table(td, flow), n)
  ct <- ct[!is.na(ct$yoy3) & ct$share > 0, c("partner", "country", "share", "value12", "yoy3")]
  ct$y_plot <- pmin(pmax(ct$yoy3, -clip), clip)
  ct
}

markets_scatter <- function(td, flow, n = 20, clip = 60) {
  d <- markets_scatter_data(td, flow, n, clip)
  if (nrow(d) == 0) return(tm_empty_plot("No data"))
  fl <- tolower(markets_flow_name(flow))
  p <- tm_chart_scatter_bubble(
    d, x = "share", y = "y_plot", size = "value12", label = "country",
    colour = TM_COL$exports, x_title = paste("Share of", fl, "(log scale)"),
    y_title = "3-month y/y growth", x_suffix = "%", y_suffix = "%")
  # True (unclipped) growth in the hover.
  hov <- paste0("<b>", d$country, "</b><br>Share of ", fl, ": ", tm_fmt_share(d$share, 1),
                "<br>12-month value: ", tm_fmt_bn(d$value12),
                "<br>3-month y/y: ", tm_fmt_pct(d$yoy3, 1), "<extra></extra>")
  p <- plotly::plotly_build(p)
  p$x$data[[1]]$hovertemplate <- hov
  cand <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 40, 60)
  lo <- min(d$share) / 1.5
  hi <- max(d$share) * 3.4  # room on the right for the largest market's label
  tv <- cand[cand >= lo & cand <= hi]
  plotly::layout(
    p,
    xaxis = list(type = "log", range = log10(c(lo, hi)), tickvals = tv,
                 ticktext = paste0(tv, "%"), ticksuffix = "", showgrid = TRUE,
                 gridcolor = TM_COL$grid),
    yaxis = list(range = c(-clip * 1.12, clip * 1.12))
  )
}

#' Bilateral goods balances (exports minus imports, latest 12 months): the 8
#' largest surpluses and 8 largest deficits among countries.
#' @return tibble(partner, country, balance12) ordered largest surplus first.
markets_balance_data <- function(td, n = 8) {
  # Countries appearing in either flow.
  pt <- markets_partner_tf(td)
  im <- pt[pt$flow == "1" & pt$date == td$latest_date & markets_is_country(pt$partner), c("partner", "sum12")]
  ex <- pt[pt$flow == "2" & pt$date == td$latest_date & markets_is_country(pt$partner), c("partner", "sum12")]
  all_p <- union(ex$partner, im$partner)
  b <- tibble::tibble(
    partner = all_p,
    balance12 = ifelse(is.na(ex$sum12[match(all_p, ex$partner)]), 0, ex$sum12[match(all_p, ex$partner)]) -
      ifelse(is.na(im$sum12[match(all_p, im$partner)]), 0, im$sum12[match(all_p, im$partner)])
  )
  b$country <- markets_name(td, b$partner)
  b <- b[order(-b$balance12), ]
  pos <- utils::head(b[b$balance12 > 0, ], n)
  neg <- utils::tail(b[b$balance12 < 0, ], n)
  rbind(pos, neg)[, c("partner", "country", "balance12")]
}

markets_balances <- function(td, n = 8) {
  d <- markets_balance_data(td, n)
  tm_chart_hbar(d, label = "country", value = "balance12", scale = 1e9, diverging = TRUE)
}

# ---------------------------------------------------------------------------
# Country profile
# ---------------------------------------------------------------------------

#' Countries available in monthly_partner_sitc1 for the flow, largest first.
#' @return named character vector c(<country name> = <code>).
markets_profile_choices <- function(td, flow) {
  codes <- unique(td$monthly_partner_sitc1$partner[td$monthly_partner_sitc1$flow == flow])
  pt <- markets_partner_tf(td)
  cur <- pt[pt$flow == flow & pt$date == td$latest_date & pt$partner %in% codes, c("partner", "sum12")]
  cur <- cur[order(-cur$sum12), ]
  codes <- c(cur$partner, setdiff(codes, cur$partner))
  stats::setNames(codes, markets_name(td, codes))
}

#' 3-month-average monthly values by SITC section for one partner, sections
#' below `min_share` of the partner's latest 12-month total lumped to
#' "Other sections".
#' @return tibble(date, section [factor], value) in EUR.
markets_profile_sitc_data <- function(td, flow, partner, min_share = 0.02) {
  s1 <- td$monthly_partner_sitc1
  d <- s1[s1$flow == flow & s1$partner == partner, c("flow", "sitc1", "date", "value_eur")]
  empty <- tibble::tibble(date = as.Date(character()), section = factor(character()), value = numeric())
  if (nrow(d) == 0) return(empty)
  ld <- td$latest_date
  # Pad to the full sample range so the 12-month window at the latest date exists.
  pad <- tibble::tibble(flow = flow, sitc1 = d$sitc1[1],
                        date = c(min(td$monthly_total$date), ld), value_eur = 0)
  d <- rbind(d, pad)
  cm <- tm_complete_monthly(d, by = c("flow", "sitc1"), value_cols = "value_eur")
  tf <- tm_add_transforms(cm, by = c("flow", "sitc1"))
  tf <- tf[tf$date <= ld, ]
  cur <- tf[tf$date == ld, ]
  keep <- cur$sitc1[cur$sum12 > 0 & cur$sum12 / sum(cur$sum12) >= min_share]
  sec <- unname(TM_SITC1_LABELS[tf$sitc1])
  sec[!(tf$sitc1 %in% keep)] <- "Other sections"
  out <- tibble::tibble(date = tf$date, section = sec, value = tf$ma3)
  out <- out[!is.na(out$value), ]
  out <- stats::aggregate(value ~ date + section, data = out, FUN = sum)
  out$section <- factor(out$section, levels = c(unname(TM_SITC1_LABELS), "Other sections"))
  tibble::as_tibble(out[order(out$section, out$date), ])
}

markets_profile_sitc <- function(td, flow, partner, years = 5) {
  d <- markets_profile_sitc_data(td, flow, partner)
  if (nrow(d) == 0) return(tm_empty_plot("No product detail for this country"))
  cols <- c(TM_SITC1_COLOURS, "Other sections" = "#c8cbd0")
  p <- plotly::plot_ly()
  for (s in levels(droplevels(d$section))) {
    x <- d[d$section == s, ]
    p <- plotly::add_trace(
      p, type = "bar", x = x$date, y = x$value / 1e9, name = s,
      marker = list(color = unname(cols[[s]])),
      hovertemplate = paste0(.TM_EURO, "%{y:,.2f}bn<extra>", s, "</extra>"))
  }
  ld <- td$latest_date
  p |>
    tm_layout(y_title = "EUR bn per month (3-month average)", range_buttons = TRUE) |>
    plotly::layout(barmode = "stack", bargap = 0.1, xaxis = list(hoverformat = "%b %Y")) |>
    tm_add_events() |>
    plotly::layout(xaxis = list(range = list(as.character(markets_shift(ld, -12 * years)),
                                             as.character(markets_shift(ld, 1)))))
}

markets_profile_bilateral <- function(td, partner) {
  b <- markets_bilateral_series(td, partner)
  if (nrow(b) == 0) return(tm_empty_plot("No data for this country"))
  p <- plotly::plot_ly() |>
    plotly::add_trace(type = "bar", x = b$date, y = b$balance / 1e9, name = "Balance",
                      marker = list(color = "rgba(82,96,115,0.45)"),
                      hovertemplate = paste0(.TM_EURO, "%{y:,.1f}bn<extra>Balance</extra>")) |>
    plotly::add_trace(type = "scatter", mode = "lines", x = b$date, y = b$exports / 1e9,
                      name = "Exports to", line = list(color = TM_COL$exports, width = 2.6),
                      connectgaps = FALSE,
                      hovertemplate = paste0(.TM_EURO, "%{y:,.1f}bn<extra>Exports to</extra>")) |>
    plotly::add_trace(type = "scatter", mode = "lines", x = b$date, y = b$imports / 1e9,
                      name = "Imports from", line = list(color = TM_COL$imports, width = 2.6),
                      connectgaps = FALSE,
                      hovertemplate = paste0(.TM_EURO, "%{y:,.1f}bn<extra>Imports from</extra>"))
  p |>
    tm_layout(y_title = "EUR bn, 12-month total", zero_line = TRUE, range_buttons = TRUE) |>
    plotly::layout(xaxis = list(hoverformat = "%b %Y"), bargap = 0.1) |>
    tm_add_events()
}

#' One-line country summary for the selected flow.
markets_profile_summary <- function(td, flow, partner) {
  ct <- markets_country_table(td, flow, countries_only = TRUE)
  nm <- markets_name(td, partner)
  ld <- td$latest_date
  i <- match(partner, ct$partner)
  fl <- tolower(markets_flow_name(flow))
  noun <- if (flow == "2") "export market" else "source of imports"
  rank_txt <- if (is.na(i)) paste0(nm, " is not ranked among country partners") else
    paste0(nm, " is the ", if (i == 1) "" else paste0(markets_ordinal(i), " "), "largest ", noun, ", ",
           tm_fmt_share(ct$share[i]), " of ", fl)
  s1 <- td$monthly_partner_sitc1
  s1 <- s1[s1$flow == flow & s1$partner == partner &
             s1$date > markets_shift(ld, -12) & s1$date <= ld, ]
  chem <- if (nrow(s1) > 0 && sum(s1$value_eur) > 0)
    100 * sum(s1$value_eur[s1$sitc1 == "5"]) / sum(s1$value_eur) else NA_real_
  b <- markets_bilateral_series(td, partner)
  bal <- b$balance[match(ld, b$date)]
  bal_txt <- if (is.na(bal)) "n/a" else
    paste0(if (bal >= 0) "surplus of " else "deficit of ", tm_fmt_bn(abs(bal)))
  paste0(rank_txt, " (12 months to ", tm_fmt_month(ld), "). Chemicals & pharma are ",
         tm_fmt_share(chem), " of ", fl, " ", if (flow == "2") "to" else "from", " this country. Bilateral goods balance: ",
         bal_txt, ".")
}

# ---------------------------------------------------------------------------
# Top-25 table
# ---------------------------------------------------------------------------

#' @return tibble(rank, country, value_bn, share, share_5y, growth12, yoy3, balance_bn).
markets_top_table <- function(td, flow, n = 25) {
  ct <- utils::head(markets_country_table(td, flow), n)
  tibble::tibble(
    rank = seq_len(nrow(ct)), country = ct$country, value_bn = ct$value12 / 1e9,
    share = ct$share, share_5y = ct$share_5y, growth12 = ct$growth12, yoy3 = ct$yoy3,
    balance_bn = ct$balance12 / 1e9
  )
}

markets_dt <- function(td, flow) {
  d <- markets_top_table(td, flow)
  fl <- markets_flow_name(flow)
  dt <- DT::datatable(
    d, class = "trade-dt row-border hover compact", rownames = FALSE,
    colnames = c("Rank", "Country", paste0(fl, " 12m (EUR bn)"), "Share (%)",
                 "Share 5 years earlier (%)", "Growth vs previous 12m (%)",
                 "3-month y/y (%)", "Balance 12m (EUR bn)"),
    options = list(dom = "ft", pageLength = 25, ordering = TRUE,
                   columnDefs = list(list(className = "dt-right", targets = 2:7)))
  )
  DT::formatRound(dt, c("value_bn", "balance_bn"), digits = 1) |>
    DT::formatRound(c("share", "share_5y", "growth12", "yoy3"), digits = 1)
}

# ---------------------------------------------------------------------------
# Module
# ---------------------------------------------------------------------------

mod_markets_ui <- function(id) {
  ns <- shiny::NS(id)
  ph <- function(out, h = "360px") plotly::plotlyOutput(ns(out), height = h)
  txt <- function(out) shiny::textOutput(ns(out), inline = TRUE)
  ctl <- function(...) htmltools::div(style = "padding: 0 0.4rem;", ...)
  kpi_w <- bslib::breakpoints(sm = c(12, 12, 12, 12), md = c(6, 6, 6, 6), xl = c(3, 3, 3, 3))
  half <- bslib::breakpoints(sm = c(12, 12), lg = c(6, 6))
  htmltools::tagList(
    tm_page_css(),
    bslib::layout_columns(
      col_widths = kpi_w, fill = FALSE,
      shiny::uiOutput(ns("kpi_us")), shiny::uiOutput(ns("kpi_ea")),
      shiny::uiOutput(ns("kpi_gb")), shiny::uiOutput(ns("kpi_bal"))
    ),
    bslib::layout_columns(
      col_widths = half, fill = FALSE,
      tm_chart_card(txt("t_share"), "Share of the 12-month total by partner group",
                    ph("p_share")),
      tm_chart_card("Front-loading and payback", "3-month average, 2024 = 100",
                    ctl(shiny::selectInput(ns("front_group"), NULL,
                                           choices = TM_PARTNER_GROUP_LEVELS,
                                           selected = "United States", width = "260px")),
                    ph("p_front", "320px"))
    ),
    bslib::layout_columns(
      col_widths = half, fill = FALSE,
      tm_chart_card(txt("t_contrib"), txt("s_contrib"),
                    ctl(shiny::radioButtons(ns("basis"), NULL, choices = MARKETS_BASIS_CHOICES,
                                            selected = "3m", inline = TRUE)),
                    ph("p_contrib", "340px")),
      tm_chart_card("Market momentum heatmap", txt("s_heat"), ph("p_heat", "400px"))
    ),
    bslib::layout_columns(
      col_widths = half, fill = FALSE,
      tm_chart_card("Size versus momentum", txt("s_scatter"), ph("p_scatter", "400px")),
      tm_chart_card("Bilateral goods balances",
                    "Exports minus imports, 12 months, EUR bn. Ignores the Exports/Imports switch.",
                    ph("p_balances", "400px"))
    ),
    bslib::layout_columns(
      col_widths = 12, fill = FALSE,
      tm_chart_card(
        "Country profile", NULL,
        ctl(shiny::selectInput(ns("country"), NULL, choices = c("United States" = "US"),
                               selected = "US", width = "260px"),
            htmltools::div(class = "tm-subtitle", style = "margin-bottom: 0.5rem;",
                           shiny::uiOutput(ns("profile_summary")))),
        bslib::layout_columns(
          col_widths = half, fill = FALSE,
          htmltools::div(htmltools::div(class = "tm-subtitle", txt("s_prof_sitc")),
                         ph("p_prof_sitc", "380px")),
          htmltools::div(htmltools::div(class = "tm-subtitle",
                                        "Bilateral picture: exports to, imports from and balance, 12-month totals (both flows)"),
                         ph("p_prof_bil", "380px"))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = 12, fill = FALSE,
      tm_chart_card(txt("t_table"), "Partner countries ranked by 12-month value; click a header to sort",
                    DT::DTOutput(ns("tbl")))
    )
  )
}

mod_markets_server <- function(id, td, flow) {
  shiny::moduleServer(id, function(input, output, session) {
    fl <- shiny::reactive({
      f <- as.character(flow())
      if (length(f) != 1 || !f %in% c("1", "2")) "2" else f
    })
    fname <- shiny::reactive(markets_flow_name(fl()))
    markets_partner_tf(td)  # warm the cache once

    # KPI tiles
    output$kpi_us <- shiny::renderUI(markets_kpi_ui(td, fl(), NULL, "us"))
    output$kpi_ea <- shiny::renderUI(markets_kpi_ui(td, fl(), NULL, "ea"))
    output$kpi_gb <- shiny::renderUI(markets_kpi_ui(td, fl(), NULL, "gb"))
    output$kpi_bal <- shiny::renderUI(markets_kpi_ui(td, fl(), NULL, "bal"))

    # Dynamic titles
    output$t_share <- shiny::renderText(
      if (fl() == "2") "Where it goes" else "Where it comes from")
    output$t_contrib <- shiny::renderText(
      paste("Who is driving the latest move in", tolower(fname())))
    basis_label <- shiny::reactive({
      b <- input$basis
      if (is.null(b) || !b %in% MARKETS_BASIS_CHOICES) b <- "3m"
      names(MARKETS_BASIS_CHOICES)[match(b, MARKETS_BASIS_CHOICES)]
    })
    output$s_contrib <- shiny::renderText(
      paste0("Contribution of partner groups to ", tolower(fname()),
             " growth, percentage points (", basis_label(), "); line = total growth"))
    output$s_heat <- shiny::renderText(
      paste0("Top 15 partners by 12-month ", tolower(fname()),
             "; colour = 3-month y/y %, capped at +/-50"))
    output$s_scatter <- shiny::renderText(
      paste0("Top 20 partners by ", tolower(fname()),
             ": share of the flow (x, log) vs 3-month y/y growth (y, clipped at +/-60%); bubble = 12-month value"))
    output$t_table <- shiny::renderText(paste("Top 25 markets:", tolower(fname())))

    # Charts
    output$p_share <- plotly::renderPlotly(markets_area_share(td, fl()))
    front_group <- shiny::reactive({
      g <- input$front_group
      if (is.null(g) || !g %in% TM_PARTNER_GROUP_LEVELS) "United States" else g
    })
    output$p_front <- plotly::renderPlotly(markets_frontload(td, fl(), front_group()))
    basis <- shiny::reactive({
      b <- input$basis
      if (is.null(b) || !b %in% MARKETS_BASIS_CHOICES) "3m" else b
    })
    output$p_contrib <- plotly::renderPlotly(markets_contrib(td, fl(), basis()))
    output$p_heat <- plotly::renderPlotly(markets_heatmap(td, fl()))
    output$p_scatter <- plotly::renderPlotly(markets_scatter(td, fl()))
    output$p_balances <- plotly::renderPlotly(markets_balances(td))

    # Country profile
    choices <- shiny::reactive(markets_profile_choices(td, fl()))
    shiny::observeEvent(choices(), {
      ch <- choices()
      cur <- shiny::isolate(input$country)
      sel <- if (!is.null(cur) && cur %in% ch) cur else if ("US" %in% ch) "US" else ch[[1]]
      shiny::updateSelectInput(session, "country", choices = ch, selected = sel)
    })
    country <- shiny::reactive({
      ch <- choices()
      cc <- input$country
      if (is.null(cc) || !cc %in% ch) { if ("US" %in% ch) "US" else ch[[1]] } else cc
    })
    # renderUI, not renderText: the summary contains a euro sign, which renderText
    # mangles when R runs in a non-UTF-8 locale.
    output$profile_summary <- shiny::renderUI(
      htmltools::span(markets_profile_summary(td, fl(), country())))
    output$s_prof_sitc <- shiny::renderText(
      paste0(fname(), ": 3-month average by SITC section, EUR bn per month; sections under 2% of the total grouped"))
    output$p_prof_sitc <- plotly::renderPlotly(markets_profile_sitc(td, fl(), country()))
    output$p_prof_bil <- plotly::renderPlotly(markets_profile_bilateral(td, country()))

    # Table
    output$tbl <- DT::renderDT(markets_dt(td, fl()))
  })
}
