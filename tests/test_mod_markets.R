# Tests for app/R/mod_markets.R. Run from the project root:
#   Rscript tests/test_mod_markets.R
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
source(file.path(app, "R", "mod_markets.R"))

ok <- function(cond, msg) {
  if (!isTRUE(cond)) stop("TEST FAILED: ", msg, call. = FALSE)
  cat("  ok -", msg, "\n")
}
near <- function(a, b, tol = 1e-9) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol))
build <- function(p) plotly::plotly_build(p)
is_plot <- function(p) inherits(p, "plotly") && length(build(p)$x$data) >= 0

cat("Loading data and building bundle\n")
td <- tm_build_bundle(read_trade_data(app))
ld <- td$latest_date
w12 <- function(d) d[d$date > markets_shift(ld, -12) & d$date <= ld, ]

cat("Source file hygiene\n")
src <- readLines(file.path(app, "R", "mod_markets.R"), warn = FALSE)
ok(!any(grepl("[^\\x01-\\x7F]", src, perl = TRUE)), "mod_markets.R is ASCII")
ok(!any(grepl("library\\(|require\\(", src)), "no library()/require() calls in the module")

cat("Cached partner-level step\n")
assign("pt", NULL, envir = .markets_cache)
tm_cold <- system.time(pt <- markets_partner_tf(td))[["elapsed"]]
tm_warm <- system.time(markets_partner_tf(td))[["elapsed"]]
cat(sprintf("  partner transforms: cold %.2fs, cached %.3fs, %d rows\n", tm_cold, tm_warm, nrow(pt)))
ok(tm_cold < 2, "partner-level step < 2s")
ok(tm_warm < 0.1, "cached call is instant")

cat("Brute-force checks against raw tables\n")
mp <- td$monthly_partner_total
mt <- td$monthly_total
exp12 <- w12(mp[mp$flow == "2", ])
imp12 <- w12(mp[mp$flow == "1", ])
tot_exp12 <- sum(w12(mt[mt$flow == "2", ])$value_eur)
us_share <- 100 * sum(exp12$value_eur[exp12$partner == "US"]) / tot_exp12
kp <- markets_kpi_data(td, "2")
ok(near(kp$us$value, us_share, 1e-6), sprintf("US share of 12m exports = %.4f%%", us_share))
us_bal <- sum(exp12$value_eur[exp12$partner == "US"]) - sum(imp12$value_eur[imp12$partner == "US"])
ok(near(kp$bal$value, us_bal, 1e-8), sprintf("12m US goods balance = %.3f bn", us_bal / 1e9))
# change vs 12 months earlier
prev <- function(d) d[d$date > markets_shift(ld, -24) & d$date <= markets_shift(ld, -12), ]
us_bal_prev <- sum(prev(mp[mp$flow == "2" & mp$partner == "US", ])$value_eur) -
  sum(prev(mp[mp$flow == "1" & mp$partner == "US", ])$value_eur)
ok(near(kp$bal$change, us_bal - us_bal_prev, 1e-8), "US balance change vs 12m earlier")
ok(length(kp$us$spark) == 36 && all(is.finite(kp$us$spark)), "sparkline series of 36 points")
ea <- exp12$value_eur[exp12$partner %in% TM_PARTNER_GROUPS$partner[TM_PARTNER_GROUPS$partner_group == "Euro area"]]
ok(near(kp$ea$value, 100 * sum(ea) / tot_exp12, 1e-6), "euro area share of exports")

by_p <- exp12 |> group_by(partner) |> summarise(v = sum(value_eur), .groups = "drop") |>
  filter(markets_is_country(partner)) |> arrange(desc(v))
top5 <- head(by_p, 5)
ct <- markets_country_table(td, "2")
ok(identical(ct$partner[1:5], top5$partner), paste("top-5 export markets:", paste(top5$partner, collapse = ",")))
ok(near(ct$value12[1:5], top5$v, 1e-9), "top-5 12m values")
ok(near(ct$share[1:5], 100 * top5$v / tot_exp12, 1e-6), "top-5 shares")
tb <- markets_top_table(td, "2")
ok(nrow(tb) == 25 && identical(tb$rank, 1:25), "top table has 25 ranked rows")
ok(all(vapply(tb[, -2], is.numeric, logical(1))), "top table columns numeric")
ok(near(tb$value_bn[1], top5$v[1] / 1e9), "top table value in EUR bn")
# share five years earlier
old <- mp[mp$flow == "2" & mp$date > markets_shift(ld, -72) & mp$date <= markets_shift(ld, -60), ]
tot_old <- sum(mt$value_eur[mt$flow == "2" & mt$date > markets_shift(ld, -72) & mt$date <= markets_shift(ld, -60)])
ok(near(tb$share_5y[1], 100 * sum(old$value_eur[old$partner == top5$partner[1]]) / tot_old, 1e-6),
   "share five years earlier (rank 1)")
# growth vs previous 12 months
gr <- 100 * (top5$v[1] / sum(prev(mp[mp$flow == "2" & mp$partner == top5$partner[1], ])$value_eur) - 1)
ok(near(tb$growth12[1], gr, 1e-6), "growth vs previous 12 months (rank 1)")

bal <- markets_balance_data(td)
b_all <- bind_rows(exp12, imp12) |> filter(markets_is_country(partner)) |>
  group_by(partner) |>
  summarise(b = sum(value_eur[flow == "2"]) - sum(value_eur[flow == "1"]), .groups = "drop") |>
  arrange(desc(b))
ok(bal$partner[1] == b_all$partner[1] && near(bal$balance12[1], b_all$b[1]),
   paste("largest surplus:", b_all$partner[1]))
ok(bal$partner[nrow(bal)] == b_all$partner[nrow(b_all)] && near(bal$balance12[nrow(bal)], b_all$b[nrow(b_all)]),
   paste("largest deficit:", b_all$partner[nrow(b_all)]))
ok(nrow(bal) == 16 && sum(bal$balance12 > 0) == 8 && !any(grepl("^[QX]", bal$partner) & bal$partner != "QA"),
   "8 surpluses + 8 deficits, no non-country codes")

cat("Indexed series\n")
for (fl in c("1", "2")) for (g in TM_PARTNER_GROUP_LEVELS) {
  d <- markets_frontload_data(td, fl, g)
  for (s in levels(d$series)) {
    x <- d$index[d$series == s & format(d$date, "%Y") == "2024"]
    ok(length(x) == 12 && near(mean(x), 100, 1e-9),
       sprintf("flow %s %s / %s averages 100 over 2024", fl, g, s))
  }
}
d <- markets_frontload_data(td, "2", "United States")
tot3 <- td$total_tf[td$total_tf$flow == "2", ]
oth <- d[d$series == "All other markets", ]
us3 <- d[d$series == "United States", ]
ok(near(oth$ma3 + us3$ma3, tot3$ma3[match(oth$date, tot3$date)], 1e-9), "group + other = total (3m average)")

cat("Contributions sum to total growth\n")
for (fl in c("1", "2")) for (b in c("3m", "m", "12m", "ytd")) {
  cd <- markets_contrib_data(td, fl, b)
  s <- cd |> group_by(date) |> summarise(s = sum(contrib_pp), t = first(total_growth_pct), .groups = "drop")
  ok(nrow(s) > 12 && !anyNA(cd$contrib_pp) && max(abs(s$s - s$t)) < 1e-8,
     sprintf("flow %s basis %s: contributions sum to total growth (%d months)", fl, b, nrow(s)))
}

cat("Chart builders (plotly_build) for both flows\n")
countries_both <- intersect(markets_profile_choices(td, "1"), markets_profile_choices(td, "2"))
for (fl in c("1", "2")) {
  ok(is_plot(markets_area_share(td, fl)), paste("area share, flow", fl))
  for (g in TM_PARTNER_GROUP_LEVELS) ok(is_plot(markets_frontload(td, fl, g)), paste("front-loading", g, "flow", fl))
  for (b in c("3m", "m", "12m", "ytd")) ok(is_plot(markets_contrib(td, fl, b)), paste("contrib", b, "flow", fl))
  hb <- build(markets_heatmap(td, fl))
  hd <- markets_heatmap_data(td, fl)
  ok(nlevels(hd$country) == 15 && length(unique(hd$date)) == 24, paste("heatmap 15 x 24, flow", fl))
  ok(identical(as.character(hd$partner[1]), markets_country_table(td, fl)$partner[1]), "heatmap largest at top")
  sb <- build(markets_scatter(td, fl))
  sd <- markets_scatter_data(td, fl)
  ok(nrow(sd) <= 20 && nrow(sd) >= 15 && all(abs(sd$y_plot) <= 60), paste("scatter data clipped, flow", fl))
  ok(identical(sb$x$layout$xaxis$type, "log"), "scatter log x axis")
  ok(length(sb$x$data[[1]]$hovertemplate) == nrow(sd) && all(grepl("3-month y/y: [+-]", sb$x$data[[1]]$hovertemplate)), "scatter hover per point with true growth")
  ok(identical(sb$x$data[[1]]$hovertemplate[1], paste0("<b>", sd$country[1], "</b><br>Share of ", tolower(markets_flow_name(fl)), ": ", tm_fmt_share(sd$share[1], 1), "<br>12-month value: ", tm_fmt_bn(sd$value12[1]), "<br>3-month y/y: ", tm_fmt_pct(sd$yoy3[1], 1), "<extra></extra>")), "scatter hover text")
  for (cc in c("US", "GB", "DE", "BE", "NL", "CH", "CN", head(markets_profile_choices(td, fl), 3))) {
    if (!cc %in% markets_profile_choices(td, fl)) next
    ok(is_plot(markets_profile_sitc(td, fl, cc)), paste("profile sitc", cc, "flow", fl))
    ok(is_plot(markets_profile_bilateral(td, cc)), paste("profile bilateral", cc, "flow", fl))
    ok(nchar(markets_profile_summary(td, fl, cc)) > 40, paste("summary", cc, "flow", fl))
  }
  dt <- markets_dt(td, fl)
  ok(inherits(dt, "datatables"), paste("datatable, flow", fl))
}
ok(is_plot(markets_balances(td)), "balances chart")
cat("  e.g. ", markets_profile_summary(td, "2", "US"), "\n")
cat("  e.g. ", markets_profile_summary(td, "1", "GB"), "\n")

cat("Profile details\n")
ps <- markets_profile_sitc_data(td, "2", "US")
ok(all(c("Chemicals & pharma") %in% as.character(ps$section)), "US profile has chemicals & pharma")
ok(all(as.character(ps$section) %in% c(unname(TM_SITC1_LABELS), "Other sections")), "section names valid")
# 3m average sum over sections equals the partner's 3m average total
us3m <- mp[mp$flow == "2" & mp$partner == "US", ]
tot_ps <- ps |> group_by(date) |> summarise(v = sum(value), .groups = "drop")
ref <- pt[pt$flow == "2" & pt$partner == "US", ]
m <- inner_join(tot_ps, ref[, c("date", "ma3")], by = "date")
ok(nrow(m) > 100 && max(abs(m$v - m$ma3)) / max(m$ma3) < 0.01, "sitc stack totals match partner 3m average (within 1%)")
cur <- ps[ps$date == ld, ]
ok(all(cur$value > 0), "latest stacked values positive")
ok(identical(unname(markets_profile_choices(td, "2")[[1]]), "US"), "US is the first (largest) export country choice")
ok(!is.na(match("US", markets_profile_choices(td, "1"))), "US available for imports")
b <- markets_bilateral_series(td, "US")
ok(near(b$balance[b$date == ld], us_bal), "bilateral series ends at brute-force balance")
ok(!any(markets_is_country(c("QV", "QW", "XS", "XC", "XK", "XL", "QP", "QR", "QS", "QZ"))) &&
     all(markets_is_country(c("US", "QA", "DE"))), "non-country classifier")

cat("UI renders\n")
html <- as.character(mod_markets_ui("x"))
ok(nchar(html) > 2000, "mod_markets_ui renders")
for (id in c("x-kpi_us", "x-kpi_bal", "x-p_share", "x-front_group", "x-basis", "x-p_heat", "x-p_scatter",
             "x-p_balances", "x-country", "x-p_prof_sitc", "x-p_prof_bil", "x-tbl", "x-profile_summary"))
  ok(grepl(id, html, fixed = TRUE), paste("UI has id", id))
ok(grepl("tm-card", html, fixed = TRUE) && grepl("<style>", html, fixed = TRUE), "uses tm_chart_card and page css")

cat("testServer\n")
outs <- c("kpi_us", "kpi_ea", "kpi_gb", "kpi_bal", "t_share", "t_contrib", "s_contrib", "s_heat", "s_scatter",
          "t_table", "p_share", "p_front", "p_contrib", "p_heat", "p_scatter", "p_balances",
          "profile_summary", "s_prof_sitc", "p_prof_sitc", "p_prof_bil", "tbl")
for (flw in c("2", "1")) {
  shiny::testServer(mod_markets_server, args = list(td = td, flow = shiny::reactive(flw)), {
    touch <- function() for (o in outs) {
      v <- output[[o]]
      stopifnot(!is.null(v))
    }
    session$flushReact()
    touch()
    for (g in c("United States", "Euro area", "Rest of world", "China")) {
      session$setInputs(front_group = g)
      stopifnot(!is.null(output$p_front))
    }
    for (b in c("3m", "m", "12m", "ytd")) {
      session$setInputs(basis = b)
      stopifnot(!is.null(output$p_contrib), grepl("percentage points", output$s_contrib, fixed = TRUE))
    }
    for (cc in c("US", "GB", "DE", "CH")) {
      session$setInputs(country = cc)
      stopifnot(!is.null(output$p_prof_sitc), !is.null(output$p_prof_bil),
                grepl(markets_name(td, cc), as.character(output$profile_summary$html), fixed = TRUE))
    }
    # an invalid / missing country falls back safely
    session$setInputs(country = "ZZ")
    stopifnot(!is.null(output$p_prof_sitc))
    touch()
    stopifnot(grepl(if (flw == "2") "Where it goes" else "Where it comes from", output$t_share))
    stopifnot(grepl("United States share", as.character(output$kpi_us$html), fixed = TRUE))
  })
  cat("  ok - testServer flow", flw, "\n")
}
# flow switch inside one session
rv <- shiny::reactiveVal("2")
shiny::testServer(mod_markets_server, args = list(td = td, flow = rv), {
  session$setInputs(country = "GB")
  session$flushReact()
  a <- output$t_share
  rv("1"); session$flushReact()
  stopifnot(!identical(a, output$t_share), !is.null(output$p_prof_sitc), !is.null(output$tbl))
})
cat("  ok - flow switch within a session\n")

cat("ALL MARKETS TESTS PASSED\n")
