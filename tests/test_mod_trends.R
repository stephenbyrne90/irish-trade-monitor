# Tests for app/R/mod_trends.R. Run from the project root:
#   Rscript tests/test_mod_trends.R
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(dplyr); library(ggplot2); library(plotly)
  library(DT); library(scales); library(arrow); library(tidyr); library(readr)
})

args <- commandArgs(trailingOnly = FALSE)
f <- sub("^--file=", "", args[grepl("^--file=", args)])
root <- if (length(f)) normalizePath(file.path(dirname(f), "..")) else getwd()
app <- file.path(root, "app")
source(file.path(app, "R", "trade_utils.R"))
source(file.path(app, "R", "analytics.R"))
source(file.path(app, "R", "charts.R"))
source(file.path(app, "R", "mod_trends.R"))

ok <- function(cond, msg) {
  if (!isTRUE(cond)) stop("TEST FAILED: ", msg, call. = FALSE)
  cat("  ok -", msg, "\n")
}

cat("Source file is ASCII\n")
src <- readLines(file.path(app, "R", "mod_trends.R"), warn = FALSE)
ok(all(!is.na(iconv(src, "UTF-8", "ASCII"))), "mod_trends.R is ASCII-only")

cat("Loading data and building bundle\n")
td <- tm_build_bundle(read_trade_data(app))
flows <- c("1", "2")
build <- function(p) plotly::plotly_build(p)

cat("trends_series\n")
ids <- trends_series_ids()
ok(length(ids) == 4 + 10 + 10, "24 series choices")
tf_cols <- c("date", "value_eur", "sum3", "sum12", "ma3", "ma12", "yoy_pct", "yoy3_pct",
             "yoy12_pct", "mom3_pct", "ytd", "ytd_yoy_pct", "series_label")
for (fl in flows) for (id in ids) {
  s <- trends_series(td, fl, id)
  lat <- max(td$total_tf$date[td$total_tf$flow == fl])
  stopifnot(nrow(s) > 100, all(tf_cols %in% names(s)), max(s$date) == lat,
            !is.unsorted(s$date), !anyDuplicated(s$date), length(unique(s$series_label)) == 1)
}
ok(TRUE, "every series x flow: non-empty, transform columns, latest date, sorted")
ok(trends_series(td, "2", "s:5")$series_label[1] == "5 - Chemicals & pharma", "section label style")
ok(isTRUE(all.equal(trends_series(td, "2", "u:total")$value_eur,
                    td$total_tf$value_eur[td$total_tf$flow == "2"])), "headline total matches total_tf")
ok(inherits(try(trends_series(td, "2", "x:foo"), silent = TRUE), "try-error"), "unknown id errors")
for (fl in flows) ok(nzchar(trends_summary(td, fl, "u:total")), paste("summary text flow", fl))
cat("  ", trends_summary(td, "2", "u:total"), "\n")

cat("Plot builders\n")
for (fl in flows) {
  for (id in ids) {
    build(trends_chart_level(td, fl, id))
    build(trends_chart_growth(td, fl, id))
    for (cu in c(FALSE, TRUE)) build(trends_chart_seasonal(td, fl, id, cu))
  }
  ok(TRUE, paste("level / growth / seasonal for all series, flow", fl))
  for (b in names(TRENDS_GROWTH_COLS)) build(trends_chart_heatmap(td, fl, b))
  ok(TRUE, paste("heatmap all bases, flow", fl))
  for (b in names(TRENDS_BASIS_LABELS)) for (k in names(TRENDS_BREAKDOWNS))
    build(trends_chart_contrib(td, fl, b, k))
  ok(TRUE, paste("contributions all bases x breakdowns, flow", fl))
  build(trends_chart_breadth(td, fl))
  ok(TRUE, paste("breadth chart, flow", fl))
}
hd <- trends_heatmap_data(td, "2", "3m")
ok(nlevels(hd$row) == 11 && levels(hd$row)[1] == "Total goods", "heatmap has Total + 10 groups, Total on top")
ok(length(unique(hd$date)) == 36 && max(hd$date) == td$latest_date, "heatmap spans last 36 months")
g <- build(trends_chart_growth(td, "2", "u:total"))
yr <- unlist(g$x$layout$yaxis$range)
ok(length(yr) == 2 && yr[2] < 400, "growth y range set from smoothed series")
lv <- build(trends_chart_level(td, "2", "u:total"))
ok(length(lv$x$layout$shapes) >= 1, "level chart has event shapes")

cat("Breadth index\n")
for (fl in flows) {
  tm <- system.time(b <- trends_breadth(td, fl))[["elapsed"]]
  cat(sprintf("  flow %s: %.2fs, %d months, latest %s: unweighted %.1f%%, weighted %.1f%% (%d groups)\n",
              fl, tm, nrow(b), format(max(b$date)), b$share_unweighted[nrow(b)],
              b$share_weighted[nrow(b)], b$n_groups[nrow(b)]))
  ok(tm < 3, paste("breadth computed in < 3s, flow", fl))
  ok(all(b$share_unweighted >= 0 & b$share_unweighted <= 100 &
           b$share_weighted >= 0 & b$share_weighted <= 100), paste("breadth in [0,100], flow", fl))
  ok(max(b$date) == max(td$total_tf$date[td$total_tf$flow == fl]), paste("breadth latest date, flow", fl))
  cache <- new.env()
  trends_breadth(td, fl, cache = cache)
  tm2 <- system.time(b2 <- trends_breadth(td, fl, cache = cache))[["elapsed"]]
  ok(identical(b, b2) && tm2 < 0.2, paste("cache hit is instant, flow", fl))
  # brute force at the latest and one earlier date
  for (L in c(max(b$date), trends_shift_months(max(b$date), -7))) {
    L <- as.Date(L)
    s5 <- td$monthly_sitc5[td$monthly_sitc5$flow == fl, ]
    s5$code <- substr(s5$product_sitc5, 1, 3)
    win_cur <- trends_shift_months(L, -2:0)
    win_prev <- trends_shift_months(L, -14:-12)
    cur <- tapply(s5$value_eur[s5$date %in% win_cur], s5$code[s5$date %in% win_cur], sum)
    prv <- tapply(s5$value_eur[s5$date %in% win_prev], s5$code[s5$date %in% win_prev], sum)
    codes <- names(prv)[prv > 1e6]
    cu <- cur[codes]; cu[is.na(cu)] <- 0
    pv <- prv[codes]
    up <- cu > pv
    row <- b[b$date == L, ]
    ok(abs(100 * mean(up) - row$share_unweighted) < 1e-9 && row$n_groups == length(codes),
       paste("brute-force unweighted share matches at", format(L), "flow", fl))
    ok(abs(100 * sum(pv[up]) / sum(pv) - row$share_weighted) < 1e-9,
       paste("brute-force weighted share matches at", format(L), "flow", fl))
  }
}

cat("Contributions sum to total\n")
for (fl in flows) for (k in names(TRENDS_BREAKDOWNS)) for (b in names(TRENDS_BASIS_LABELS)) {
  ct <- trends_contrib_data(td, fl, b, k)
  stopifnot(nrow(ct) > 0, !anyNA(ct$contrib_pp))
  s <- tapply(ct$contrib_pp, ct$date, sum)
  t <- tapply(ct$total_growth_pct, ct$date, function(x) x[1])
  stopifnot(max(abs(s - t)) < 1e-6)
}
ok(TRUE, "sum of contributions == total growth line, all flows x breakdowns x bases")
ct <- trends_contrib_data(td, "2", "3m", "group")
lat <- max(ct$date)
tot_yoy3 <- td$total_tf$yoy3_pct[td$total_tf$flow == "2" & td$total_tf$date == lat]
ok(abs(ct$total_growth_pct[ct$date == lat][1] - tot_yoy3) < 1e-4, "group contributions total equals total 3m y/y")

cat("UI renders\n")
html <- as.character(mod_trends_ui("x"))
ok(nchar(html) > 1000, "mod_trends_ui renders")
for (nm in c("series", "seasonal_mode", "heat_basis", "contrib_basis", "contrib_breakdown"))
  ok(grepl(paste0("x-", nm), html, fixed = TRUE), paste("UI has input", nm))
for (nm in c("level", "growth", "seasonal", "heatmap", "contrib", "breadth", "summary"))
  ok(grepl(paste0("x-", nm), html, fixed = TRUE), paste("UI has output", nm))

cat("Server (testServer)\n")
outs <- c("summary", "t_level", "t_growth", "t_seasonal", "sub_seasonal", "t_heatmap", "t_contrib",
          "t_breadth", "level", "growth", "seasonal", "heatmap", "contrib", "breadth")
for (fl in flows) {
  shiny::testServer(mod_trends_server, args = list(td = td, flow = shiny::reactive(fl)), {
    touch <- function() for (o in outs) {
      v <- output[[o]]
      stopifnot(!is.null(v))
    }
    touch()
    for (s in trends_series_ids()) { session$setInputs(series = s); touch() }
    session$setInputs(series = "u:total")
    for (v in c("monthly", "cumulative")) { session$setInputs(seasonal_mode = v); touch() }
    for (v in c("3m", "m", "12m")) { session$setInputs(heat_basis = v); touch() }
    for (b in c("3m", "m", "12m", "ytd")) for (k in c("group", "sitc", "partner")) {
      session$setInputs(contrib_basis = b, contrib_breakdown = k); touch()
    }
    session$setInputs(series = "bogus"); touch()
  })
  ok(TRUE, paste("testServer all inputs x outputs, flow", fl))
}
# flow reactive switching
rv <- shiny::reactiveVal("2")
shiny::testServer(mod_trends_server, args = list(td = td, flow = rv), {
  # summary is a renderUI (span) so read the rendered HTML
  a <- as.character(output$summary$html)
  rv("1"); session$flushReact()
  b <- as.character(output$summary$html)
  stopifnot(grepl("<span>Exports of ", a, fixed = TRUE), grepl("<span>Imports of ", b, fixed = TRUE))
})
ok(TRUE, "outputs react to the global flow switch")

cat("ALL TRENDS TESTS PASSED\n")
