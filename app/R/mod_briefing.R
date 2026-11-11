# Briefing page module ("the month in one page") for the Irish Trade Monitor.
#
# Contract
#   mod_briefing_ui(id)                 -> tagList for the page body
#   mod_briefing_server(id, td, flow)   -> td is the analytics bundle (plain list),
#                                          flow a reactive returning "1" or "2".
#
# Every chart / table / text block is built by a pure briefing_* function that
# takes the bundle, so it can be tested without Shiny. Top-level code only
# defines functions; everything inside is namespace-qualified (except |>).

# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------

BRIEFING_COL <- list(
  total = "#0b0c0c", ex_pharma = "#2e7d6f", ex_chem = "#9a7b2f", ex_aircraft = "#2c55a2",
  bars = "rgba(154,165,177,0.45)"
)

# First-of-month Date n months before `date`.
briefing_months_back <- function(date, n) {
  date <- as.Date(date)
  lt <- as.POSIXlt(date, tz = "UTC")
  idx <- (lt$year + 1900L) * 12L + lt$mon - as.integer(n)
  as.Date(sprintf("%04d-%02d-01", as.integer(idx %/% 12L), as.integer(idx %% 12L) + 1L))
}

# Value of `col` at `date` (NA if the row is absent).
briefing_pick <- function(df, date, col) {
  v <- df[[col]][df$date == date]
  if (length(v) != 1L) NA_real_ else as.numeric(v)
}

briefing_flow_name <- function(flow) {
  if (identical(as.character(flow), "1")) "Imports" else "Exports"
}

briefing_lc <- function(x) paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x)))

briefing_ordinal <- function(n) {
  n <- as.integer(n)
  suf <- if (n %% 100L %in% 11:13) "th" else switch(as.character(n %% 10L),
    "1" = "st", "2" = "nd", "3" = "rd", "th")
  paste0(n, suf)
}

# Plain number with fixed decimals, no sign.
briefing_num <- function(x, digits = 1) {
  formatC(abs(as.numeric(x)), format = "f", digits = digits, big.mark = ",")
}

# "up 3.4%" / "down 2.0%" / "unchanged" (percent input).
briefing_move <- function(x, digits = 1, up = "up", down = "down") {
  if (length(x) != 1L || is.na(x)) return("not available")
  r <- round(x, digits)
  if (r > 0) paste0(up, " ", briefing_num(r, digits), "%")
  else if (r < 0) paste0(down, " ", briefing_num(r, digits), "%")
  else "unchanged"
}

# Window label for the 3 months ending at `date`: "Jun-Aug 2026".
briefing_window_label <- function(date) {
  d <- seq(briefing_months_back(date, 2), as.Date(date), by = "month")
  y <- as.integer(format(d, "%Y"))
  m <- month.abb[as.integer(format(d, "%m"))]
  if (y[1] == y[3]) paste0(m[1], "-", m[3], " ", y[3])
  else paste0(m[1], " ", y[1], "-", m[3], " ", y[3])
}

# Dates of the 3-month window ending `lag` months before `date`.
briefing_window_dates <- function(date, lag = 0) {
  end <- briefing_months_back(date, lag)
  seq(briefing_months_back(end, 2), end, by = "month")
}

briefing_dateline <- function(td) {
  paste0("Data to ", td$latest_label,
         ". The latest two months are provisional and are routinely revised.")
}

# ---------------------------------------------------------------------------
# Metrics (single source of numbers for KPI tiles and key points)
# ---------------------------------------------------------------------------

briefing_flow_metrics <- function(df, date, n_spark = 36) {
  df <- df[order(df$date), , drop = FALSE]
  list(
    value = briefing_pick(df, date, "value_eur"),
    yoy = briefing_pick(df, date, "yoy_pct"),
    mom3 = briefing_pick(df, date, "mom3_pct"),
    yoy3 = briefing_pick(df, date, "yoy3_pct"),
    sum3 = briefing_pick(df, date, "sum3"),
    sum12 = briefing_pick(df, date, "sum12"),
    spark = utils::tail(df$ma3[df$date <= date], n_spark)
  )
}

#' All headline numbers for the page, computed once from the bundle.
#' @param td analytics bundle.
#' @return named list (date, exp, imp, bal, und_chem, und_pharma, imp_ex_aircraft, us).
briefing_metrics <- function(td) {
  d <- td$latest_date
  tt <- td$total_tf
  ex <- tt[tt$flow == "2", , drop = FALSE]
  im <- tt[tt$flow == "1", , drop = FALSE]
  und <- td$underlying
  und_m <- function(meas, fl) {
    u <- und[und$flow == fl & as.character(und$measure) == meas, , drop = FALSE]
    briefing_flow_metrics(u, d)
  }
  b <- td$balance[order(td$balance$date), , drop = FALSE]
  d12 <- briefing_months_back(d, 12)
  bal <- list(
    value = briefing_pick(b, d, "balance"),
    bal12 = briefing_pick(b, d, "sum12"),
    bal12_prev = briefing_pick(b, d12, "sum12"),
    spark = utils::tail(b$sum12[b$date <= d], 36)
  )
  bal$bal12_chg <- bal$bal12 - bal$bal12_prev

  pg <- td$partner_group_tf
  us <- pg[pg$flow == "2" & as.character(pg$partner_group) == "United States", , drop = FALSE]
  us <- us[order(us$date), , drop = FALSE]
  ex <- ex[order(ex$date), , drop = FALSE]
  share <- 100 * us$sum12[match(ex$date, us$date)] / ex$sum12
  sh <- data.frame(date = ex$date, share = share)
  us_m <- list(
    share12 = briefing_pick(sh, d, "share"),
    share12_prev = briefing_pick(sh, d12, "share"),
    sum3 = briefing_pick(us, d, "sum3"),
    yoy3 = briefing_pick(us, d, "yoy3_pct"),
    spark = utils::tail(sh$share[sh$date <= d], 36)
  )
  us_m$share_chg <- us_m$share12 - us_m$share12_prev

  list(
    date = d,
    exp = briefing_flow_metrics(ex, d),
    imp = briefing_flow_metrics(im, d),
    bal = bal,
    und_chem = und_m("ex_chem", "2"),
    und_pharma = und_m("ex_pharma", "2"),
    imp_ex_aircraft = und_m("ex_aircraft", "1"),
    us = us_m
  )
}

# ---------------------------------------------------------------------------
# KPI row
# ---------------------------------------------------------------------------

briefing_kpi_row <- function(td, met = NULL) {
  m <- if (is.null(met)) briefing_metrics(td) else met
  mon <- tm_fmt_month(m$date)
  flow_card <- function(title, x, colour) {
    tm_kpi_card(
      title = paste0(title, ", ", mon), value = tm_fmt_bn(x$value),
      lines = list(
        tm_delta_span(x$yoy, "%", 1, "y/y"),
        tm_delta_span(x$mom3, "%", 1, "3m on 3m"),
        tm_delta_span(x$yoy3, "%", 1, "3m y/y"),
        htmltools::tags$span("12-month total ", htmltools::tags$strong(tm_fmt_bn(x$sum12)))
      ),
      spark = tm_sparkline(x$spark, colour), accent = colour
    )
  }
  bal <- m$bal
  bal_card <- tm_kpi_card(
    title = paste0("Trade balance, ", mon), value = tm_fmt_bn(bal$value),
    lines = list(
      htmltools::tags$span("12-month balance ", htmltools::tags$strong(tm_fmt_bn(bal$bal12))),
      tm_delta_span(bal$bal12_chg / 1e9, "bn", 1, "EUR vs a year earlier")
    ),
    spark = tm_sparkline(bal$spark, TM_COL$balance), accent = TM_COL$balance
  )
  und <- m$und_chem
  und_card <- tm_kpi_card(
    title = paste0("Underlying exports, ", mon), value = tm_fmt_bn(und$value),
    lines = list(
      tm_delta_span(und$yoy, "%", 1, "y/y"),
      tm_delta_span(und$yoy3, "%", 1, "3m y/y")
    ),
    spark = tm_sparkline(und$spark, "#2e7d6f"), accent = "#2e7d6f",
    footnote = "Excluding chemicals & pharma"
  )
  us <- m$us
  us_card <- tm_kpi_card(
    title = "US share of exports", value = tm_fmt_share(us$share12),
    lines = list(
      tm_delta_span(us$share_chg, "pp", 1, "vs 12 months earlier"),
      htmltools::tags$span("Share of 12-month exports")
    ),
    spark = tm_sparkline(us$spark, TM_PARTNER_GROUP_COLOURS[["United States"]]),
    accent = TM_PARTNER_GROUP_COLOURS[["United States"]]
  )
  bslib::layout_column_wrap(
    width = 1 / 5, class = "tm-briefing-kpis", fill = FALSE, heights_equal = "row",
    flow_card("Exports", m$exp, TM_COL$exports),
    flow_card("Imports", m$imp, TM_COL$imports),
    bal_card, und_card, us_card
  )
}

# ---------------------------------------------------------------------------
# Key points (plain text)
# ---------------------------------------------------------------------------

briefing_contrib <- function(df, component, flow, date) {
  d <- df[df$flow == flow, , drop = FALSE]
  out <- tm_contributions(d, component, basis = "3m", by = "flow")
  out <- out[!is.na(out$contrib_pp) & out$date == date, , drop = FALSE]
  out[order(-out$contrib_pp), , drop = FALSE]
}

briefing_rank_phrase <- function(rank_hi, n) {
  rank_lo <- n - rank_hi + 1L
  if (rank_hi == 1L) "the highest"
  else if (rank_lo == 1L) "the lowest"
  else if (rank_hi <= rank_lo) paste0("the ", briefing_ordinal(rank_hi), " highest")
  else paste0("the ", briefing_ordinal(rank_lo), " lowest")
}

#' Auto-generated key points for the latest month (plain text, one string each).
#' @param td analytics bundle. @param met optional precomputed briefing_metrics(td).
#' @return character vector of 7 bullets.
briefing_keypoints <- function(td, met = NULL) {
  m <- if (is.null(met)) briefing_metrics(td) else met
  d <- m$date
  mlong <- tm_fmt_month_long(d)
  mname <- month.name[as.integer(format(d, "%m"))]
  ex <- m$exp
  im <- m$imp
  out <- character()

  # a. headline
  gap <- ex$yoy - ex$yoy3
  flag <- ""
  if (!is.na(gap)) {
    opposite <- sign(round(ex$yoy, 1)) * sign(round(ex$yoy3, 1)) < 0
    if (opposite || abs(gap) > 10) {
      flag <- paste0(" The monthly figure ", if (gap > 0) "overstates" else "understates",
                     " the underlying trend.")
    } else {
      flag <- " The monthly and three-month readings are broadly consistent."
    }
  }
  out <- c(out, paste0(
    "Goods exports in ", mlong, " were ", tm_fmt_bn(ex$value), ", ",
    briefing_move(ex$yoy), " on a year earlier. Over the three months to ", mname,
    " exports were ", briefing_move(ex$yoy3), " year on year (", tm_fmt_bn(ex$sum3), ").", flag))

  # b. drivers
  cg <- briefing_contrib(td$group_monthly, "group", "2", d)
  if (nrow(cg) > 0) {
    top <- cg[1, ]
    bot <- cg[nrow(cg), ]
    s_up <- if (top$contrib_pp > 0)
      paste0("The largest positive contribution came from ", briefing_lc(as.character(top$group)),
             " (", tm_fmt_pp(top$contrib_pp), ")")
    else "No product group made a positive contribution"
    s_dn <- if (bot$contrib_pp < 0)
      paste0(", while ", briefing_lc(as.character(bot$group)), " (", tm_fmt_pp(bot$contrib_pp),
             ") was the largest drag.")
    else ", and no group subtracted from growth."
    out <- c(out, paste0("Contributions to three-month export growth of ", tm_fmt_pct(ex$yoy3),
                         " by product group: ", briefing_lc(s_up), s_dn))
  } else {
    out <- c(out, "Product-group contributions to three-month export growth are not available for this month.")
  }

  # c. underlying
  hp <- m$und_pharma$yoy3
  hc <- m$und_chem$yoy3
  rel <- if (is.na(hc) || is.na(ex$yoy3)) ""
         else if (ex$yoy3 - hc > 5) " Chemicals and pharmaceuticals are lifting the headline rate."
         else if (ex$yoy3 - hc < -5) " Chemicals and pharmaceuticals are weighing on the headline rate."
         else " The headline rate is close to the underlying rate."
  out <- c(out, paste0(
    "Excluding pharmaceuticals, three-month export growth was ", tm_fmt_pct(hp),
    " year on year, and excluding all chemicals and pharmaceuticals ", tm_fmt_pct(hc),
    ", compared with ", tm_fmt_pct(ex$yoy3), " for total exports.", rel))

  # d. markets
  us <- m$us
  cp <- briefing_contrib(td$partner_group_monthly, "partner_group", "2", d)
  other <- ""
  if (nrow(cp) > 0 && cp$contrib_pp[1] > 0 && as.character(cp$partner_group[1]) != "United States") {
    other <- paste0(" The largest positive contribution to three-month export growth came from ",
                    cp$partner_group[1], " (", tm_fmt_pp(cp$contrib_pp[1]), ").")
  }
  out <- c(out, paste0(
    "Exports to the United States over the three months to ", mname, " were ", tm_fmt_bn(us$sum3),
    ", ", briefing_move(us$yoy3), " year on year. The US accounted for ", tm_fmt_share(us$share12),
    " of exports over the past 12 months, compared with ", tm_fmt_share(us$share12_prev),
    " a year earlier (", tm_fmt_pp(us$share_chg), ").", other))

  # e. imports
  out <- c(out, paste0(
    "Imports in ", mlong, " were ", tm_fmt_bn(im$value), ", ", briefing_move(im$yoy),
    " on a year earlier; over three months they were ", briefing_move(im$yoy3),
    " year on year. Excluding aircraft, three-month import growth was ",
    tm_fmt_pct(m$imp_ex_aircraft$yoy3), "."))

  # f. balance
  b <- m$bal
  noun <- if (!is.na(b$bal12) && b$bal12 < 0) "deficit" else "surplus"
  chg_word <- if (is.na(b$bal12_chg)) "changed" else if (round(b$bal12_chg / 1e9, 1) > 0) "higher"
              else if (round(b$bal12_chg / 1e9, 1) < 0) "lower" else "unchanged"
  chg_txt <- if (chg_word == "unchanged") "unchanged on a year earlier"
             else if (chg_word == "changed") "not comparable with a year earlier"
             else paste0(tm_fmt_bn(abs(b$bal12_chg)), " ", chg_word, " than a year earlier (",
                         tm_fmt_bn(b$bal12_prev), ")")
  out <- c(out, paste0(
    "The 12-month rolling goods trade ", noun, " to ", mname, " was ",
    tm_fmt_bn(abs(b$bal12)), ", ", chg_txt, "."))

  # g. how unusual
  tf <- td$total_tf
  ex_all <- tf[tf$flow == "2" & tf$date <= d, , drop = FALSE]
  ex_all <- ex_all[order(ex_all$date), , drop = FALSE]
  last36 <- utils::tail(ex_all$value_eur, 36)
  n <- length(last36)
  rank_hi <- sum(last36 > ex$value) + 1L
  rank_lo <- n - rank_hi + 1L
  tail_note <- if (min(rank_hi, rank_lo) <= 3L)
    " Readings in the tails of the recent range should be interpreted with caution." else ""
  out <- c(out, paste0(
    "At ", tm_fmt_bn(ex$value), ", exports in ", mlong, " were ", briefing_rank_phrase(rank_hi, n),
    " of the past ", n, " months (median ", tm_fmt_bn(stats::median(last36)), ").", tail_note))
  out
}

# ---------------------------------------------------------------------------
# Charts
# ---------------------------------------------------------------------------

#' Exports and imports: faint monthly lines, bold 3-month averages, balance bars.
briefing_chart_trade <- function(td) {
  d <- td$latest_date
  tt <- td$total_tf
  ex <- tt[tt$flow == "2", , drop = FALSE]; ex <- ex[order(ex$date), ]
  im <- tt[tt$flow == "1", , drop = FALSE]; im <- im[order(im$date), ]
  b <- td$balance[order(td$balance$date), , drop = FALSE]
  hov <- function(nm) paste0(.TM_EURO, "%{y:,.1f}bn<extra>", nm, "</extra>")
  p <- plotly::plot_ly() |>
    plotly::add_trace(type = "bar", x = b$date, y = b$balance / 1e9, name = "Monthly balance",
                      marker = list(color = BRIEFING_COL$bars), hovertemplate = hov("Monthly balance")) |>
    plotly::add_trace(type = "scatter", mode = "lines", x = ex$date, y = ex$value_eur / 1e9,
                      name = "Exports, monthly", line = list(color = .tm_rgba(TM_COL$exports, 0.4), width = 1),
                      hovertemplate = hov("Exports, monthly")) |>
    plotly::add_trace(type = "scatter", mode = "lines", x = im$date, y = im$value_eur / 1e9,
                      name = "Imports, monthly", line = list(color = .tm_rgba(TM_COL$imports, 0.4), width = 1),
                      hovertemplate = hov("Imports, monthly")) |>
    plotly::add_trace(type = "scatter", mode = "lines", x = ex$date, y = ex$ma3 / 1e9,
                      name = "Exports, 3-month average", connectgaps = FALSE,
                      line = list(color = TM_COL$exports, width = 3),
                      hovertemplate = hov("Exports, 3-month average")) |>
    plotly::add_trace(type = "scatter", mode = "lines", x = im$date, y = im$ma3 / 1e9,
                      name = "Imports, 3-month average", connectgaps = FALSE,
                      line = list(color = TM_COL$imports, width = 3),
                      hovertemplate = hov("Imports, 3-month average"))
  x0 <- briefing_months_back(d, 60)
  x1 <- as.Date(d) + 20
  p <- p |>
    tm_layout(y_title = "EUR bn", range_buttons = TRUE, x_range = c(x0 - 15, x1)) |>
    plotly::layout(xaxis = list(hoverformat = "%b %Y"), bargap = 0.15)
  tm_add_events(p, events = td$events, x_min = x0, x_max = d)
}

# Measures and labels shown in the headline-vs-underlying chart.
briefing_growth_measures <- function(flow) {
  if (identical(as.character(flow), "1")) {
    c(total = "Total imports", ex_aircraft = "Excluding aircraft", ex_chem = "Excluding chemicals & pharma")
  } else {
    c(total = "Total exports", ex_pharma = "Excluding pharmaceuticals", ex_chem = "Excluding chemicals & pharma")
  }
}

#' Headline versus underlying 3-month y/y growth for one flow.
briefing_chart_growth <- function(td, flow) {
  d <- td$latest_date
  meas <- briefing_growth_measures(flow)
  u <- td$underlying
  u <- u[u$flow == flow & as.character(u$measure) %in% names(meas), , drop = FALSE]
  x0 <- briefing_months_back(d, 60)
  p <- plotly::plot_ly()
  vals <- numeric()
  for (k in names(meas)) {
    s <- u[as.character(u$measure) == k, , drop = FALSE]
    s <- s[order(s$date), , drop = FALSE]
    vis <- s[s$date >= x0, ]
    vals <- c(vals, vis$yoy3_pct)
    is_total <- k == "total"
    p <- plotly::add_trace(
      p, type = "scatter", mode = "lines", x = s$date, y = s$yoy3_pct, name = meas[[k]],
      connectgaps = FALSE,
      line = list(color = BRIEFING_COL[[k]], width = if (is_total) 3.2 else 1.9),
      hovertemplate = paste0("%{y:+.1f}%<extra>", meas[[k]], "</extra>"))
  }
  vals <- vals[is.finite(vals)]
  p <- tm_layout(p, y_suffix = "%", zero_line = TRUE, range_buttons = TRUE,
                 x_range = c(x0 - 15, as.Date(d) + 20))
  if (length(vals) >= 5) {
    q <- stats::quantile(vals, c(0.02, 0.98), names = FALSE)
    last_tot <- u$yoy3_pct[as.character(u$measure) == "total" & u$date == d]
    lo <- min(q[1], 0, last_tot, na.rm = TRUE)
    hi <- max(q[2], 0, last_tot, na.rm = TRUE)
    pad <- 0.12 * (hi - lo)
    p <- plotly::layout(p, yaxis = list(range = c(lo - pad, hi + pad)))
  }
  p |>
    plotly::layout(xaxis = list(hoverformat = "%b %Y")) |>
    plotly::config(doubleClick = "reset+autosize")
}

#' Contributions of the ten analytical groups to 3-month y/y growth, last 36 months.
briefing_chart_drivers <- function(td, flow) {
  d <- td$latest_date
  gm <- td$group_monthly[td$group_monthly$flow == flow, , drop = FALSE]
  cc <- tm_contributions(gm, "group", basis = "3m", by = "flow")
  cc <- cc[!is.na(cc$contrib_pp) & cc$date >= briefing_months_back(d, 35) & cc$date <= d, , drop = FALSE]
  if (nrow(cc) == 0) return(tm_empty_plot("No contribution data"))
  tm_chart_contrib(cc, "group", palette = TM_GROUP_COLOURS, total_name = "Total growth (%)",
                   y_title = "Percentage points")
}

#' Biggest movers: change in the latest 3-month total vs the same months a year earlier.
#' @param kind "product" (SITC 3-digit) or "market" (partner countries).
#' @return tibble(code, label, cur, prev, delta) with up to n increases and n decreases.
briefing_movers <- function(td, flow, kind = c("product", "market"), n = 6) {
  kind <- match.arg(kind)
  d <- td$latest_date
  cur_d <- briefing_window_dates(d, 0)
  prev_d <- briefing_window_dates(d, 12)
  src <- if (kind == "product") td$monthly_sitc5 else td$monthly_partner_total
  keep <- as.character(src$flow) == flow & src$date %in% c(cur_d, prev_d)
  src <- src[keep, , drop = FALSE]
  code <- if (kind == "product") substr(src$product_sitc5, 1, 3) else as.character(src$partner)
  v <- src$value_eur
  v[is.na(v)] <- 0
  agg <- tibble::tibble(
    code = code,
    cur = ifelse(src$date %in% cur_d, v, 0),
    prev = ifelse(src$date %in% prev_d, v, 0)
  ) |>
    dplyr::group_by(code) |>
    dplyr::summarise(cur = sum(cur), prev = sum(prev), .groups = "drop")
  agg$delta <- agg$cur - agg$prev
  agg <- agg[order(-agg$delta), , drop = FALSE]
  up <- utils::head(agg[agg$delta > 0, , drop = FALSE], n)
  dn <- utils::tail(agg[agg$delta < 0, , drop = FALSE], n)
  out <- rbind(up, dn)
  out$label <- if (kind == "product") tm_sitc_label(out$code, td$sitc_labels, 34)
               else tm_partner_label(out$code, td$partner_labels)
  dup <- duplicated(out$label) | duplicated(out$label, fromLast = TRUE)
  out$label[dup] <- paste0(out$label[dup], " (", out$code[dup], ")")
  out[, c("code", "label", "cur", "prev", "delta")]
}

briefing_chart_movers <- function(td, flow, kind = c("product", "market")) {
  kind <- match.arg(kind)
  mv <- briefing_movers(td, flow, kind)
  if (nrow(mv) == 0) return(tm_empty_plot("No data"))
  tm_chart_hbar(mv, label = "label", value = "delta", scale = 1e9, diverging = TRUE)
}

briefing_movers_subtitle <- function(td, flow, kind = c("product", "market")) {
  kind <- match.arg(kind)
  d <- td$latest_date
  paste0(briefing_window_label(d), " vs ", briefing_window_label(briefing_months_back(d, 12)),
         ", EUR bn change in 3-month ", tolower(briefing_flow_name(flow)),
         if (kind == "product") " (SITC 3-digit groups)" else " (partner countries)")
}

# ---------------------------------------------------------------------------
# Key points card
# ---------------------------------------------------------------------------

briefing_copy_js <- function(list_id) {
  paste0(
    "var b=this,u=document.getElementById('", list_id, "'),",
    "t=Array.prototype.map.call(u.querySelectorAll('li'),function(l){return '- '+l.textContent;}).join('\\n');",
    "function done(){var o=b.getAttribute('data-label');b.textContent='Copied';",
    "setTimeout(function(){b.textContent=o;},1500);}",
    "function fb(){var a=document.createElement('textarea');a.value=t;document.body.appendChild(a);",
    "a.select();try{document.execCommand('copy');}catch(e){}document.body.removeChild(a);done();}",
    "if(navigator.clipboard&&navigator.clipboard.writeText){navigator.clipboard.writeText(t).then(done,fb);}else{fb();}"
  )
}

briefing_keypoints_card <- function(points, ns = function(x) x) {
  list_id <- ns("keypoints_list")
  bslib::card(
    class = "tm-card border-0 shadow-sm",
    bslib::card_header(
      class = "tm-card-header d-flex justify-content-between align-items-start",
      htmltools::div(
        htmltools::div(class = "tm-card-title", "Key points"),
        htmltools::div(class = "tm-subtitle", "Auto-generated from the latest release; figures are nominal EUR.")
      ),
      htmltools::tags$button(
        type = "button", class = "btn btn-sm btn-outline-secondary tm-copy-btn",
        `data-label` = "Copy text", onclick = briefing_copy_js(list_id), "Copy text"
      )
    ),
    bslib::card_body(
      class = "tm-card-body",
      htmltools::tags$ul(id = list_id, class = "tm-keypoints",
                         lapply(points, function(p) htmltools::tags$li(p)))
    )
  )
}

# ---------------------------------------------------------------------------
# UI and server
# ---------------------------------------------------------------------------

briefing_css <- function() {
  htmltools::tags$style(htmltools::HTML("
.tm-briefing .tm-dateline { color: #5c6270; font-size: 0.82rem; margin: 0.2rem 0 0.7rem 0; }
.tm-briefing .tm-briefing-kpis { grid-template-columns: repeat(auto-fit, minmax(11.5rem, 1fr)) !important; margin-bottom: 1rem; }
.tm-briefing .tm-keypoints { box-shadow: none; padding: 0.2rem 0 0.2rem 1.4rem; margin: 0; border-left: 0; font-size: 0.95rem; }
.tm-briefing .tm-keypoints li { margin-bottom: 0.35rem; }
.tm-briefing .tm-copy-btn { font-size: 0.75rem; padding: 0.1rem 0.5rem; }
.tm-briefing .tm-card { margin-bottom: 1rem; }
"))
}

mod_briefing_ui <- function(id) {
  ns <- shiny::NS(id)
  plot_out <- function(x, h) {
    plotly::plotlyOutput(ns(x), height = h)
  }
  htmltools::tagList(
    tm_page_css(),
    briefing_css(),
    htmltools::div(
      class = "tm-briefing",
      shiny::uiOutput(ns("dateline")),
      shiny::uiOutput(ns("kpis")),
      shiny::uiOutput(ns("keypoints")),
      tm_chart_card(
        "Exports and imports",
        "Monthly values (faint lines) and 3-month averages (bold), EUR bn; grey bars show the monthly trade balance (exports minus imports).",
        plot_out("chart_trade", "380px")
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        tm_chart_card(shiny::uiOutput(ns("t_growth")), shiny::uiOutput(ns("s_growth")),
                      plot_out("chart_growth", "340px")),
        tm_chart_card(shiny::uiOutput(ns("t_drivers")), shiny::uiOutput(ns("s_drivers")),
                      plot_out("chart_drivers", "340px"))
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        tm_chart_card(shiny::uiOutput(ns("t_products")), shiny::uiOutput(ns("s_products")),
                      plot_out("chart_products", "340px")),
        tm_chart_card(shiny::uiOutput(ns("t_markets")), shiny::uiOutput(ns("s_markets")),
                      plot_out("chart_markets", "340px"))
      )
    )
  )
}

mod_briefing_server <- function(id, td, flow) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Computed once, independent of the flow switch.
    met <- briefing_metrics(td)
    points <- briefing_keypoints(td, met)
    trade_plot <- briefing_chart_trade(td)

    fl <- shiny::reactive({
      f <- as.character(flow())
      if (f %in% c("1", "2")) f else "2"
    })
    fname <- shiny::reactive(briefing_flow_name(fl()))

    output$dateline <- shiny::renderUI(htmltools::div(class = "tm-dateline", briefing_dateline(td)))
    output$kpis <- shiny::renderUI(briefing_kpi_row(td, met))
    output$keypoints <- shiny::renderUI(briefing_keypoints_card(points, ns))
    output$chart_trade <- plotly::renderPlotly(trade_plot)

    output$t_growth <- shiny::renderUI(paste0(fname(), ": headline versus underlying growth"))
    output$s_growth <- shiny::renderUI({
      ex <- if (fl() == "1") "excluding aircraft and excluding chemicals & pharma"
            else "excluding pharmaceuticals and excluding chemicals & pharma"
      paste0("3-month total vs the same 3 months a year earlier, % (total, ", ex,
             "). Axis clipped to the 2nd-98th percentile range; double-click to autoscale.")
    })
    output$chart_growth <- plotly::renderPlotly(briefing_chart_growth(td, fl()))

    output$t_drivers <- shiny::renderUI(paste0(fname(), ": what is driving growth"))
    output$s_drivers <- shiny::renderUI(paste0(
      "Contribution of each product group to 3-month y/y ", tolower(fname()),
      " growth, percentage points; line = total growth, %. Last 36 months."))
    output$chart_drivers <- plotly::renderPlotly(briefing_chart_drivers(td, fl()))

    output$t_products <- shiny::renderUI(paste0(fname(), ": biggest product movers"))
    output$s_products <- shiny::renderUI(briefing_movers_subtitle(td, fl(), "product"))
    output$chart_products <- plotly::renderPlotly(briefing_chart_movers(td, fl(), "product"))

    output$t_markets <- shiny::renderUI(paste0(fname(), ": biggest market movers"))
    output$s_markets <- shiny::renderUI(briefing_movers_subtitle(td, fl(), "market"))
    output$chart_markets <- plotly::renderPlotly(briefing_chart_movers(td, fl(), "market"))
  })
}
