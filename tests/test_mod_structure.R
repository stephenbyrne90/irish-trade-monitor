# Tests for app/R/mod_structure.R (page "Pharma & concentration").
# Run from the project root:  Rscript tests/test_mod_structure.R
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
source(file.path(app, "R", "mod_structure.R"))

ok <- function(cond, msg) {
  if (!isTRUE(cond)) stop("TEST FAILED: ", msg, call. = FALSE)
  cat("  ok -", msg, "\n")
}
near <- function(a, b, tol = 1e-9) isTRUE(all.equal(a, b, tolerance = tol, check.attributes = FALSE))
build <- function(p) plotly::plotly_build(p)

cat("Loading data and building bundle\n")
trd <- read_trade_data(app)
td <- tm_build_bundle(trd)
L <- td$latest_date
cat("  latest date:", format(L), "\n")

# --- source hygiene ---------------------------------------------------------
src <- readLines(file.path(app, "R", "mod_structure.R"), warn = FALSE)
ok(!any(grepl("[^\x01-\x7F]", src, perl = TRUE, useBytes = TRUE)), "module source is ASCII only")
ok(!any(grepl("^\\s*(library|require)\\(", src)), "no library()/require() calls in module")

# --- cached heavy step ------------------------------------------------------
cat("Cached heavy step (per flow)\n")
prep <- list()
for (fc in c("2", "1")) {
  tt <- system.time(prep[[fc]] <- structure_prep(td, fc))[["elapsed"]]
  cat(sprintf("  structure_prep(flow %s): %.2fs\n", fc, tt))
  ok(tt < 2, paste0("structure_prep < 2s for flow ", fc))
}

# --- brute-force checks (exports, latest date) ------------------------------
cat("Brute-force checks, exports at the latest date\n")
win <- seq(structure_shift_months(L, -11), L, by = "month")
ex5 <- td$monthly_sitc5 |> filter(flow == "2", date %in% win) |>
  group_by(product_sitc5) |> summarise(v = sum(value_eur), .groups = "drop") |> arrange(desc(v))
tot12 <- sum(td$monthly_total$value_eur[td$monthly_total$flow == "2" & td$monthly_total$date %in% win])
ok(near(tot12, td$total_tf$sum12[td$total_tf$flow == "2" & td$total_tf$date == L]), "12m total matches total_tf")
bf_top3 <- 100 * sum(ex5$v[1:3]) / tot12
bf_top1 <- 100 * ex5$v[1] / tot12
bf_hhi <- sum((100 * ex5$v / tot12)^2)
chem_codes <- tm_assign_group(ex5$product_sitc5)
gm_win <- td$group_monthly |> filter(flow == "2", date %in% win)
bf_chem <- 100 * sum(gm_win$value_eur[gm_win$group %in% TM_CHEM_GROUPS]) / tot12
k <- structure_kpis(td, "2", prep[["2"]])
ok(near(k$top3$share, bf_top3, 1e-9), sprintf("top-3 share %.2f%% matches brute force", bf_top3))
ok(identical(k$largest$code, ex5$product_sitc5[1]), paste0("largest product is ", ex5$product_sitc5[1]))
ok(near(k$largest$share, bf_top1, 1e-9), sprintf("largest share %.2f%% matches brute force", bf_top1))
ok(near(k$largest$value, ex5$v[1], 1e-9), "largest product 12m value matches")
ok(near(k$neff$hhi, bf_hhi, 1e-9), sprintf("HHI %.0f matches brute force", bf_hhi))
ok(near(k$neff$neff, 10000 / bf_hhi, 1e-9), "effective number = 10000 / HHI")
ok(near(k$chem$share, bf_chem, 1e-9), sprintf("chemicals & pharma share %.2f%% matches brute force", bf_chem))
ok(identical(k$top3$codes, ex5$product_sitc5[1:3]), "top-3 codes match")
# year-earlier comparisons
w0 <- seq(structure_shift_months(L, -23), structure_shift_months(L, -12), by = "month")
e0 <- td$monthly_sitc5 |> filter(flow == "2", date %in% w0) |>
  group_by(product_sitc5) |> summarise(v = sum(value_eur), .groups = "drop") |> arrange(desc(v))
t0 <- sum(td$monthly_total$value_eur[td$monthly_total$flow == "2" & td$monthly_total$date %in% w0])
ok(near(k$top3$d12, bf_top3 - 100 * sum(e0$v[1:3]) / t0, 1e-9), "top-3 change vs 12 months earlier")
cat(sprintf("  HEADLINE exports %s: chem&pharma %.1f%%, top3 %.1f%%, top1 %s %.1f%%, HHI %.0f, N_eff %.1f\n",
            format(L), bf_chem, bf_top3, ex5$product_sitc5[1], bf_top1, bf_hhi, 10000 / bf_hhi))
for (fc in c("1", "2")) {
  kk <- structure_kpis(td, fc, prep[[fc]])
  ok(all(is.finite(c(kk$chem$share, kk$chem$d12, kk$chem$d60, kk$top3$share, kk$neff$neff, kk$largest$share))),
     paste0("KPIs finite for flow ", fc))
}

# --- concentration series ---------------------------------------------------
cat("Concentration series\n")
for (fc in c("2", "1")) {
  cs <- structure_concentration(td, fc, prep[[fc]])
  ok(min(cs$date) >= as.Date("2015-01-01") && max(cs$date) == L, paste0("quarterly series spans 2015..latest, flow ", fc))
  ok(all(cs$top1 <= cs$top3 + 1e-9 & cs$top3 <= cs$top10 + 1e-9 & cs$top10 <= 100), "top1 <= top3 <= top10 <= 100")
  ok(all(!is.na(cs$hhi_products)) && all(!is.na(cs$hhi_partners)), "HHI series complete")
  ok(all(cs$hhi_products > 0 & cs$hhi_products < 10000 & cs$hhi_partners > 0 & cs$hhi_partners < 10000), "HHI in range")
}
# random historical date vs brute force
d_chk <- as.Date("2019-06-01")
wc <- seq(structure_shift_months(d_chk, -11), d_chk, by = "month")
e1 <- td$monthly_sitc5 |> filter(flow == "2", date %in% wc) |>
  group_by(product_sitc5) |> summarise(v = sum(value_eur), .groups = "drop") |> arrange(desc(v))
t1 <- sum(td$monthly_total$value_eur[td$monthly_total$flow == "2" & td$monthly_total$date %in% wc])
cs2 <- structure_concentration(td, "2", prep[["2"]])
ok(near(cs2$top10[cs2$date == d_chk], 100 * sum(e1$v[1:10]) / t1, 1e-9), "top-10 share at 2019-06 matches brute force")
pw <- td$monthly_partner_total |> filter(flow == "2", date %in% wc) |>
  group_by(partner) |> summarise(v = sum(value_eur), .groups = "drop")
ok(near(cs2$hhi_partners[cs2$date == d_chk], sum((100 * pw$v / sum(pw$v))^2), 1e-9), "partner HHI at 2019-06 matches brute force")

# --- treemap ----------------------------------------------------------------
cat("Treemap\n")
for (fc in c("2", "1")) {
  tr <- structure_treemap_data(td, fc, prep[[fc]])
  T12 <- td$total_tf$sum12[td$total_tf$flow == fc & td$total_tf$date == L]
  leaves <- tr[tr$kind %in% c("leaf", "other", "confidential"), ]
  ok(abs(sum(leaves$value) - T12) <= max(1, 1e-9 * T12), paste0("treemap leaves sum to 12m total, flow ", fc))
  ok(abs(tr$value[tr$kind == "root"] - T12) <= max(1, 1e-9 * T12), "root equals 12m total")
  gt <- td$group_monthly |> filter(flow == fc, date %in% seq(structure_shift_months(L, -11), L, by = "month")) |>
    group_by(group) |> summarise(v = sum(value_eur), .groups = "drop")
  grp_nodes <- tr[tr$kind == "group", ]
  ok(all(sapply(seq_len(nrow(grp_nodes)), function(i) {
    abs(grp_nodes$value[i] - gt$v[as.character(gt$group) == grp_nodes$group[i]]) <= 1 &&
      abs(sum(leaves$value[leaves$group == grp_nodes$group[i]]) - grp_nodes$value[i]) <= 1
  })), paste0("each group branch equals group_monthly 12m total and its leaves, flow ", fc))
  ok(!anyDuplicated(tr$id) && all(tr$parent[tr$parent != ""] %in% tr$id), "treemap ids unique, parents exist")
  ok(all(tr$value > 0), "treemap values positive")
}

# --- top-25 table -----------------------------------------------------------
cat("Top 25 table\n")
for (fc in c("2", "1")) {
  tb <- structure_top_table(td, fc, 25, prep[[fc]])
  ok(nrow(tb) == 25, "25 rows")
  ok(all(diff(tb$cum_share) > 0) && all(diff(tb$value_bn) <= 0), "cumulative share strictly increases; value falls")
  ok(near(tb$cum_share[25], sum(tb$share)), "cumulative share equals sum of shares")
  ok(is.numeric(tb$value_bn) && is.numeric(tb$share) && is.numeric(tb$growth) && is.numeric(tb$share_5y), "numeric columns")
}
tb <- structure_top_table(td, "2", 25, prep[["2"]])
ok(tb$code[1] == ex5$product_sitc5[1] && near(tb$share[1], bf_top1) && near(tb$cum_share[3], bf_top3),
   "first row = largest product; cumulative at row 3 = top-3 share")
ok(near(tb$value_bn[1] * 1e9, ex5$v[1]), "first-row value matches")
ok(near(tb$growth[1], 100 * (ex5$v[1] / e0$v[e0$product_sitc5 == tb$code[1]] - 1), 1e-9), "growth vs prior 12m matches brute force")
w5 <- seq(structure_shift_months(L, -71), structure_shift_months(L, -60), by = "month")
v5 <- sum(td$monthly_sitc5$value_eur[td$monthly_sitc5$flow == "2" & td$monthly_sitc5$date %in% w5 &
                                      td$monthly_sitc5$product_sitc5 == tb$code[1]])
t5 <- sum(td$monthly_total$value_eur[td$monthly_total$flow == "2" & td$monthly_total$date %in% w5])
ok(near(tb$share_5y[1], 100 * v5 / t5, 1e-9), "share five years earlier matches brute force")
w <- structure_table_widget(td, "2", prep[["2"]])
ok(inherits(w, "datatables"), "DT widget built")

# --- contribution chart -----------------------------------------------------
cat("One product against everything else\n")
for (fc in c("2", "1")) {
  tp <- structure_top_codes(td, fc, 25, prep[[fc]])
  ok(nrow(tp) == 25 && !is.unsorted(rev(tp$value)), "25 top codes sorted")
  for (cd in unique(c(tp$product_sitc5[1], "54154", tp$product_sitc5[10]))) {
    cc <- structure_product_contrib(td, fc, cd, prep[[fc]])
    ok(nlevels(cc$component) == 2 && nrow(cc) > 100, paste0("two components, flow ", fc, " product ", cd))
    s <- cc |> group_by(date) |> summarise(s = sum(contrib_pp), g = first(total_growth_pct), .groups = "drop")
    ref <- td$total_tf[td$total_tf$flow == fc, c("date", "yoy3_pct")]
    s <- merge(s, ref, by = "date")
    ok(all(abs(s$s - s$yoy3_pct) < 1e-8) && all(abs(s$g - s$yoy3_pct) < 1e-8),
       paste0("contributions sum to total 3m y/y growth, flow ", fc, " product ", cd))
  }
}

# --- charts: every builder, both flows, forced through plotly_build ---------
cat("Chart builders\n")
n_charts <- 0
for (fc in c("2", "1")) {
  pr <- prep[[fc]]
  chk <- function(p, nm) {
    b <- build(p)
    n_charts <<- n_charts + 1
    stopifnot(inherits(b, "plotly"), length(b$x$data) >= 1 || nm == "empty")
    invisible(b)
  }
  chk(structure_chart_area(td, fc, pr), "area")
  chk(structure_chart_treemap(td, fc, pr), "treemap")
  chk(structure_chart_conc(td, fc, pr), "conc")
  hh <- chk(structure_chart_hhi(td, fc, pr), "hhi")
  ok(length(hh$x$layout$shapes) == 2, "HHI chart has the two reference lines")
  ok(identical(hh$x$layout$yaxis$title$text, "HHI (0-10,000)"), "HHI axis title")
  for (g in TM_GROUP_LEVELS) {
    gb <- chk(structure_chart_group(td, fc, g, pr), "group")
    chk(structure_chart_uv(td, fc, g, pr), "uv")
    sub <- structure_uv_subtitle(td, fc, g, pr)
    stopifnot(grepl("not a price index", sub, fixed = TRUE))
    gs <- structure_group_series(td, fc, g, 6, pr)
    gtf <- td$group_tf[td$group_tf$flow == fc & as.character(td$group_tf$group) == g, ]
    chk_tot <- gs |> group_by(date) |> summarise(s = sum(value_eur), .groups = "drop")
    m <- merge(chk_tot, gtf[, c("date", "ma3")], by = "date")
    stopifnot(all(abs(m$s - m$ma3) < 1e-3 * pmax(1, abs(m$ma3)) + 1))
  }
  tp <- structure_top_codes(td, fc, 25, pr)
  for (cd in tp$product_sitc5[c(1, 2, 7, 25)]) chk(structure_chart_product(td, fc, cd, pr), "product")
  ok(TRUE, paste0("all builders built for flow ", fc))
}
cat("  built", n_charts, "plots\n")
a <- build(structure_chart_area(td, "2", prep[["2"]]))
sh <- structure_area_data(td, "2")
ok(all(abs(tapply(sh$share, sh$date, sum) - 100) < 1e-6), "area shares sum to 100 at every date")
gp <- build(structure_chart_group(td, "2", "Pharmaceuticals", prep[["2"]]))
ok(identical(gp$x$layout$barmode, "stack"), "group chart is stacked")
ok(length(gp$x$data) == 7, "group chart: 6 products + Other")
sh_cols <- structure_shades("#0f4c5c", 6)
ok(length(unique(sh_cols)) == 6 && length(structure_shades("#0f4c5c", 1)) == 1, "structure_shades gives distinct colours")
ok(length(structure_shades("#b8bec6", 3)) == 3, "structure_shades works on a pale colour")

# --- Shiny module via testServer -------------------------------------------
cat("testServer\n")
for (fc in c("2", "1")) {
  shiny::testServer(mod_structure_server, args = list(td = td, flow = shiny::reactive(fc)), {
    session$setInputs(group = "Pharmaceuticals", product = "54154")
    stopifnot(!is.null(output$kpis), !is.null(output$area), !is.null(output$treemap),
              !is.null(output$conc), !is.null(output$hhi), !is.null(output$group_chart),
              !is.null(output$prod_chart), !is.null(output$uv_chart), !is.null(output$table),
              !is.null(output$t_area), !is.null(output$sub_uv))
    for (g in c("Aircraft", "Computers & electronics", "Food, drink & tobacco")) {
      session$setInputs(group = g)
      stopifnot(!is.null(output$group_chart), !is.null(output$uv_chart), !is.null(output$t_group),
                !is.null(output$sub_uv))
    }
    session$setInputs(product = structure_top_codes(td, fc, 25)$product_sitc5[3])
    stopifnot(!is.null(output$prod_chart), !is.null(output$t_prod))
    session$setInputs(product = "99999")   # not in list: falls back to default
    stopifnot(!is.null(output$prod_chart))
    stopifnot(!is.null(output$t_tree), !is.null(output$t_conc), !is.null(output$t_hhi),
              !is.null(output$t_uv), !is.null(output$t_table))
  })
  ok(TRUE, paste0("testServer outputs evaluated for flow ", fc))
}
# reactive flow switch
flow_rv <- shiny::reactiveVal("2")
shiny::testServer(mod_structure_server, args = list(td = td, flow = flow_rv), {
  session$setInputs(group = "Pharmaceuticals", product = "54154")
  stopifnot(!is.null(output$kpis))
  flow_rv("1")
  session$flushReact()
  stopifnot(!is.null(output$kpis), !is.null(output$treemap), !is.null(output$table))
})
ok(TRUE, "flow switch inside one session works")

# --- UI ---------------------------------------------------------------------
cat("UI\n")
html <- as.character(mod_structure_ui("x"))
ok(nchar(html) > 1000, "mod_structure_ui renders")
for (id in c("x-kpis", "x-area", "x-treemap", "x-conc", "x-hhi", "x-group", "x-group_chart",
             "x-product", "x-prod_chart", "x-uv_chart", "x-table")) {
  ok(grepl(paste0('id="', id, '"'), html, fixed = TRUE), paste0("UI contains #", id))
}
ok(length(gregexpr("class=\"tm-card-title\"", html, fixed = TRUE)[[1]]) == 8, "eight chart cards")
ok(grepl(".tm-kpi", html, fixed = TRUE), "tm_page_css included")
kc <- as.character(structure_kpi_cards(td, "2", prep[["2"]]))
ok(grepl("Effective number of products", kc, fixed = TRUE) && grepl("spread evenly", kc, fixed = TRUE), "KPI tiles render with footnote")

cat("ALL STRUCTURE TESTS PASSED\n")
