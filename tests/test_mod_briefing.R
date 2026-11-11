# Tests for app/R/mod_briefing.R.
# Run from the project root:  Rscript tests/test_mod_briefing.R
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
source(file.path(app, "R", "mod_briefing.R"))

ok <- function(cond, msg) {
  if (!isTRUE(cond)) stop("TEST FAILED: ", msg, call. = FALSE)
  cat("  ok -", msg, "\n")
}
near <- function(a, b, tol = 1e-6) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol))

cat("Loading data and building bundle\n")
trd <- read_trade_data(app)
td <- tm_build_bundle(trd)
d <- td$latest_date

cat("Source file is ASCII\n")
src <- readLines(file.path(app, "R", "mod_briefing.R"), warn = FALSE, encoding = "bytes")
ok(!any(grepl("[^\x01-\x7F]", src, perl = TRUE, useBytes = TRUE)), "mod_briefing.R is ASCII-only")

cat("Plot builders (both flows)\n")
build <- function(p) plotly::plotly_build(p)
ok(inherits(build(briefing_chart_trade(td)), "plotly"), "trade chart builds")
for (fl in c("1", "2")) {
  ok(inherits(build(briefing_chart_growth(td, fl)), "plotly"), paste("growth chart builds, flow", fl))
  ok(inherits(build(briefing_chart_drivers(td, fl)), "plotly"), paste("drivers chart builds, flow", fl))
  for (k in c("product", "market")) {
    ok(inherits(build(briefing_chart_movers(td, fl, k)), "plotly"), paste(k, "movers chart builds, flow", fl))
    mv <- briefing_movers(td, fl, k)
    ok(nrow(mv) >= 2 && nrow(mv) <= 12 && all(is.finite(mv$delta)), paste(k, "movers table sane, flow", fl))
    ok(!anyDuplicated(mv$label), paste(k, "mover labels unique"))
    sub <- briefing_movers_subtitle(td, fl, k)
    ok(grepl("Jun-Aug 2026 vs Jun-Aug 2025", sub, fixed = TRUE), paste("subtitle windows:", sub))
  }
}
g <- build(briefing_chart_growth(td, "2"))
ok(length(g$x$data) == 3, "growth chart has three lines")
ok(length(build(briefing_chart_trade(td))$x$layout$shapes) > 0, "trade chart has event shapes")

cat("Movers brute force\n")
mp <- td$monthly_partner_total
cur <- mp$flow == "2" & mp$date %in% as.Date(c("2026-06-01", "2026-07-01", "2026-08-01"))
prv <- mp$flow == "2" & mp$date %in% as.Date(c("2025-06-01", "2025-07-01", "2025-08-01"))
bf <- tapply(mp$value_eur[cur], mp$partner[cur], sum)
bp <- tapply(mp$value_eur[prv], mp$partner[prv], sum)
all_p <- union(names(bf), names(bp))
delta <- (ifelse(is.na(bf[all_p]), 0, bf[all_p])) - (ifelse(is.na(bp[all_p]), 0, bp[all_p]))
mvm <- briefing_movers(td, "2", "market")
ok(near(max(delta), max(mvm$delta)) && near(min(delta), min(mvm$delta)), "market movers extremes match brute force")

cat("Brute-force numbers vs metrics\n")
mt <- td$monthly_total
ex <- mt[mt$flow == "2", ]; ex <- ex[order(ex$date), ]
im <- mt[mt$flow == "1", ]; im <- im[order(im$date), ]
v <- ex$value_eur; n <- length(v)
yoy_bf <- 100 * (v[n] / v[n - 12] - 1)
yoy3_bf <- 100 * (sum(v[(n - 2):n]) / sum(v[(n - 14):(n - 12)]) - 1)
bal12_bf <- sum(v[(n - 11):n]) - sum(im$value_eur[(nrow(im) - 11):nrow(im)])
bal12_prev_bf <- sum(v[(n - 23):(n - 12)]) - sum(im$value_eur[(nrow(im) - 23):(nrow(im) - 12)])
mp_us <- mp[mp$flow == "2" & mp$partner == "US", ]
us12 <- function(end) {
  dts <- seq(end, by = "-1 month", length.out = 12)
  sum(mp_us$value_eur[mp_us$date %in% dts]) / sum(ex$value_eur[ex$date %in% dts])
}
d12 <- seq(d, by = "-12 months", length.out = 2)[2]
met <- briefing_metrics(td)
ok(near(met$exp$yoy, yoy_bf), "exports y/y matches brute force")
ok(near(met$exp$yoy3, yoy3_bf), "exports 3m y/y matches brute force")
ok(near(met$bal$bal12, bal12_bf), "12m balance matches brute force")
ok(near(met$bal$bal12_chg, bal12_bf - bal12_prev_bf), "12m balance change matches brute force")
ok(near(met$us$share12, 100 * us12(d)), "US 12m export share matches brute force")
ok(near(met$us$share_chg, 100 * (us12(d) - us12(d12))), "US share change matches brute force")
ok(near(met$exp$value, v[n]), "latest exports value matches")
kp <- briefing_keypoints(td, met)
ok(grepl(tm_fmt_pct(yoy_bf), paste(kp, collapse = " "), fixed = FALSE) ||
   grepl(sprintf("%.1f%%", abs(round(yoy_bf, 1))), kp[1], fixed = TRUE), "keypoint a quotes brute-force y/y")
ok(grepl(sprintf("%.1f%%", abs(round(yoy3_bf, 1))), kp[1], fixed = TRUE), "keypoint a quotes brute-force 3m y/y")
ok(grepl(sprintf("%.1f", abs(bal12_bf) / 1e9), kp[6], fixed = TRUE), "keypoint f quotes brute-force 12m balance")
ok(grepl(sprintf("%.1f%%", 100 * us12(d)), kp[4], fixed = TRUE), "keypoint d quotes brute-force US share")

cat("Key points\n")
bad <- "\\bNA\\b|NaN|Inf|\\+-|--"
check_kp <- function(k, label) {
  ok(is.character(k) && length(k) >= 5 && length(k) <= 7, paste(label, ": 5-7 bullets"))
  ok(all(nzchar(trimws(k))), paste(label, ": bullets non-empty"))
  ok(!any(grepl(bad, k)), paste(label, ": no NA/NaN/Inf/+-/--"))
}
check_kp(kp, "latest")
cat(paste0("   * ", kp, "\n"), sep = "")

trunc_date <- as.Date("2025-03-01")
trd2 <- trd
for (nm in c("monthly_total", "monthly_sitc1", "monthly_sitc5", "monthly_partner_total", "monthly_partner_sitc1")) {
  trd2[[nm]] <- trd[[nm]][trd[[nm]]$date <= trunc_date, ]
}
td2 <- tm_build_bundle(trd2)
ok(td2$latest_date == trunc_date, "truncated bundle ends 2025-03")
kp2 <- briefing_keypoints(td2)
check_kp(kp2, "truncated")
cat(paste0("   * ", kp2, "\n"), sep = "")
ok(grepl("up ", kp2[1]) && grepl("highest", kp2[7]), "truncated wording adapts (up / highest)")
ok(!identical(kp, kp2), "wording differs between months")
ok(inherits(build(briefing_chart_growth(td2, "2")), "plotly"), "charts build on truncated bundle")
ok(inherits(build(briefing_chart_movers(td2, "1", "product")), "plotly"), "movers build on truncated bundle")

cat("Wording helpers\n")
ok(briefing_move(-0.04) == "unchanged" && briefing_move(2.345) == "up 2.3%" && briefing_move(-5) == "down 5.0%", "briefing_move")
ok(briefing_ordinal(11) == "11th" && briefing_ordinal(22) == "22nd" && briefing_ordinal(3) == "3rd", "ordinals")
ok(briefing_window_label(as.Date("2026-01-01")) == "Nov 2025-Jan 2026", "window label across years")

cat("KPI row and UI\n")
kpis <- as.character(briefing_kpi_row(td, met))
ok(length(gregexpr("tm-kpi-title", kpis, fixed = TRUE)[[1]]) == 5, "five KPI tiles")
ui <- as.character(mod_briefing_ui("x"))
ok(nzchar(ui) && grepl("x-chart_trade", ui, fixed = TRUE), "UI renders")
card <- as.character(briefing_keypoints_card(kp, shiny::NS("x")))
ok(grepl("tm-keypoints", card, fixed = TRUE) && grepl("navigator.clipboard.writeText", card, fixed = TRUE) &&
   grepl("Copy text", card, fixed = TRUE), "key points card has list and copy button")

cat("Server (testServer)\n")
outs <- c("dateline", "kpis", "keypoints", "chart_trade", "t_growth", "s_growth", "chart_growth",
          "t_drivers", "s_drivers", "chart_drivers", "t_products", "s_products", "chart_products",
          "t_markets", "s_markets", "chart_markets")
for (fl in c("2", "1")) {
  assign(".briefing_test_flow", fl, envir = globalenv())
  shiny::testServer(mod_briefing_server, args = list(td = td, flow = shiny::reactive(fl)), {
    for (o in outs) {
      val <- output[[o]]
      stopifnot(!is.null(val))
    }
    ttl <- output$t_growth
    stopifnot(grepl(if (get(".briefing_test_flow", envir = globalenv()) == "2") "Exports" else "Imports", paste(unlist(ttl), collapse = " ")))
    stopifnot(grepl("Jun-Aug 2026 vs Jun-Aug 2025", paste(unlist(output$s_markets), collapse = " "), fixed = TRUE))
  })
  ok(TRUE, paste("all outputs render, flow", fl))
}

cat("ALL BRIEFING TESTS PASSED\n")
