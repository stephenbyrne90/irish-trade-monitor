# Foundation tests for app/R/analytics.R and app/R/charts.R.
# Run from the project root:  Rscript tests/test_foundation.R
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
EUR <- intToUtf8(0x20AC)

ok <- function(cond, msg) {
  if (!isTRUE(cond)) stop("TEST FAILED: ", msg, call. = FALSE)
  cat("  ok -", msg, "\n")
}
near <- function(a, b, tol = 1e-9) isTRUE(all.equal(a, b, tolerance = tol, check.attributes = FALSE))

cat("Loading data and building bundle\n")
t_read <- system.time(trd <- read_trade_data(app))[["elapsed"]]
t_bundle <- system.time(td <- tm_build_bundle(trd))[["elapsed"]]
cat(sprintf("  read_trade_data: %.2fs, tm_build_bundle: %.2fs\n", t_read, t_bundle))
ok(t_bundle < 5, "bundle builds in < 5 seconds")
need <- c("monthly_total", "monthly_sitc1", "monthly_sitc5", "monthly_partner_total", "monthly_partner_sitc1",
          "sitc_labels", "partner_labels", "group_monthly", "partner_group_monthly", "total_tf", "group_tf",
          "sitc1_tf", "partner_group_tf", "underlying", "balance", "latest_date", "latest_label",
          "complete_year", "events")
ok(all(need %in% names(td)), "bundle has all required elements")
ok(all(c("section", "section_short") %in% names(td$monthly_sitc1)), "monthly_sitc1 has section labels")
ok(td$latest_label == tm_fmt_month_long(td$latest_date), "latest_label matches latest_date")

cat("Constants\n")
ok(nrow(TM_GROUPS) == 10 && identical(TM_GROUP_LEVELS, TM_GROUPS$group), "ten groups in order")
ok(identical(TM_GROUPS$colour[1], "#004d44"), "Pharmaceuticals colour (Department of Finance green)")
ok(length(TM_GROUP_COLOURS) == 10 && !anyDuplicated(TM_GROUP_COLOURS), "distinct group colours")
ok(nrow(TM_EVENTS) == 5, "five events")

cat("tm_assign_group\n")
chk <- c("54154" = "Pharmaceuticals", "77423" = "Medical devices & instruments", "79240" = "Aircraft",
         "89961" = "Medical devices & instruments", "77642" = "Computers & electronics",
         "01110" = "Food, drink & tobacco", "33400" = "Fuels", "51579" = "Organic chemicals",
         "55141" = "Other chemicals", "78120" = "Other machinery & transport", "89399" = "Other goods",
         "11211" = "Food, drink & tobacco", "87111" = "Medical devices & instruments")
ok(identical(unname(tm_assign_group(names(chk))), unname(chk)), "spot checks")
ok(all(td$monthly_sitc5$product_sitc5 |> tm_assign_group() |> unique() %in% TM_GROUP_LEVELS), "all real codes map to a group")
ok(system.time(tm_assign_group(td$monthly_sitc5$product_sitc5))[["elapsed"]] < 2, "assign_group fast on 750k codes")

cat("Group / partner-group sums\n")
tot <- td$monthly_total |> select(flow, date, total = value_eur)
chk_sum <- function(x, grp) {
  j <- x |> group_by(flow, date) |> summarise(s = sum(value_eur), .groups = "drop") |>
    inner_join(tot, by = c("flow", "date"))
  list(n = nrow(j), n_tot = nrow(tot), maxdiff = max(abs(j$s - j$total)),
       maxrel = max(abs(j$s - j$total) / j$total))
}
g <- chk_sum(td$group_monthly)
ok(g$n == g$n_tot, "group_monthly covers every flow-date")
ok(g$maxdiff < 1 && g$maxrel < 1e-9, sprintf("group_monthly sums to total (max diff %.4g EUR)", g$maxdiff))
pg <- chk_sum(td$partner_group_monthly)
ok(pg$n == pg$n_tot, "partner_group_monthly covers every flow-date")
ok(pg$maxdiff < 1 && pg$maxrel < 1e-9, sprintf("partner groups sum to total (max diff %.4g EUR)", pg$maxdiff))
ok(is.factor(td$group_monthly$group) && identical(levels(td$group_monthly$group), TM_GROUP_LEVELS), "group factor levels")
ok(is.factor(td$partner_group_monthly$partner_group), "partner_group factor")
s1 <- td$monthly_sitc1 |> group_by(flow, date) |> summarise(s = sum(value_eur), .groups = "drop")
ok(all(abs(s1$s - tot$total[match(paste(s1$flow, s1$date), paste(tot$flow, tot$date))]) < 2000),
   "sitc1 sums to total within source rounding (< EUR 2k)")
gs1 <- td$group_monthly |> group_by(flow, date) |> summarise(g = sum(value_eur), .groups = "drop") |>
  inner_join(s1, by = c("flow", "date"))
ok(max(abs(gs1$g - gs1$s)) < 2000, "groups match sitc1 sections within source rounding (< EUR 2k)")
gm_sec <- td$group_monthly |>
  mutate(sec = c("Food, drink & tobacco" = "01", "Fuels" = "3", "Pharmaceuticals" = "5", "Organic chemicals" = "5",
                 "Other chemicals" = "5")[as.character(group)]) |>
  filter(!is.na(sec), sec == "5") |> group_by(flow, date) |> summarise(v = sum(value_eur), .groups = "drop")
s5ref <- td$monthly_sitc1 |> filter(sitc1 == "5") |> select(flow, date, ref = value_eur)
jj <- inner_join(gm_sec, s5ref, by = c("flow", "date"))
ok(max(abs(jj$v - jj$ref)) < 1, "section 5 groups sum to SITC 5 section total")
ok(!anyNA(td$group_monthly$value_eur), "no NA in group_monthly")

cat("tm_complete_monthly\n")
toy <- tibble(flow = c("2", "2", "2"), g = "a", date = as.Date(c("2020-01-01", "2020-04-01", "2020-05-01")),
              value_eur = c(1, 2, 3), quantity_kg = c(1, 1, 1))
cm <- tm_complete_monthly(toy, c("flow", "g"))
ok(nrow(cm) == 5 && identical(cm$month, 1:5) && cm$value_eur[2] == 0 && all(cm$year == 2020), "toy completion")

cat("Transforms vs brute force (exports total, latest date)\n")
ex <- td$monthly_total |> filter(flow == "2") |> arrange(date)
ld <- td$latest_date
i <- which(ex$date == ld)
v <- ex$value_eur
tf <- td$total_tf |> filter(flow == "2", date == ld)
ok(near(tf$sum3, sum(v[(i - 2):i])), "sum3")
ok(near(tf$ma3, sum(v[(i - 2):i]) / 3), "ma3")
ok(near(tf$sum12, sum(v[(i - 11):i])), "sum12")
ok(near(tf$ma12, sum(v[(i - 11):i]) / 12), "ma12")
ok(near(tf$yoy_pct, 100 * (v[i] / v[i - 12] - 1)), "yoy_pct")
ok(near(tf$yoy3_pct, 100 * (sum(v[(i - 2):i]) / sum(v[(i - 14):(i - 12)]) - 1)), "yoy3_pct")
ok(near(tf$yoy12_pct, 100 * (sum(v[(i - 11):i]) / sum(v[(i - 23):(i - 12)]) - 1)), "yoy12_pct")
ok(near(tf$mom3_pct, 100 * (sum(v[(i - 2):i]) / sum(v[(i - 5):(i - 3)]) - 1)), "mom3_pct")
yr <- as.integer(format(ld, "%Y")); mo <- as.integer(format(ld, "%m"))
ytd_now <- sum(v[ex$date >= as.Date(paste0(yr, "-01-01")) & ex$date <= ld])
ytd_prev <- sum(v[ex$date >= as.Date(paste0(yr - 1, "-01-01")) & ex$date <= seq(ld, by = "-1 year", length.out = 2)[2]])
ok(near(tf$ytd, ytd_now), "ytd")
ok(near(tf$ytd_yoy_pct, 100 * (ytd_now / ytd_prev - 1)), "ytd_yoy_pct")
shuf <- td$monthly_total |> tm_complete_monthly("flow") |> slice_sample(prop = 1, replace = FALSE)
tf2 <- tm_add_transforms(shuf, "flow") |> filter(flow == "2", date == ld)
ok(near(tf2$sum12, tf$sum12), "transforms do not rely on row order")
first <- td$total_tf |> filter(flow == "2") |> arrange(date)
ok(all(is.na(first$sum3[1:2])) && !is.na(first$sum3[3]) && all(is.na(first$yoy_pct[1:12])), "NA for incomplete windows")
ok(tm_add_transforms(tibble(date = as.Date(c("2020-01-01", "2020-02-01")), value_eur = c(0, 5)), character(0))$yoy_pct |>
     is.na() |> all(), "no-by transforms work")

cat("tm_contributions\n")
for (b in c("3m", "m", "12m", "ytd")) {
  cc <- tm_contributions(td$group_monthly, "group", basis = b)
  s <- cc |> filter(!is.na(contrib_pp)) |> group_by(flow, date) |>
    summarise(s = sum(contrib_pp), tg = first(total_growth_pct), .groups = "drop")
  ok(nrow(s) > 100 && max(abs(s$s - s$tg)) < 1e-8, sprintf("contributions sum to total growth (%s)", b))
  col <- c("3m" = "yoy3_pct", m = "yoy_pct", `12m` = "yoy12_pct", ytd = "ytd_yoy_pct")[[b]]
  j <- inner_join(s, td$total_tf |> select(flow, date, ref = all_of(col)), by = c("flow", "date")) |>
    filter(!is.na(ref))
  ok(nrow(j) > 100 && max(abs(j$tg - j$ref)) < 1e-8, sprintf("total_growth_pct matches total transform (%s)", b))
}
cp <- tm_contributions(td$partner_group_monthly, "partner_group", "12m")
ok(all(c("flow", "date", "partner_group", "level", "delta", "contrib_pp", "total_growth_pct") %in% names(cp)),
   "contribution columns")

cat("tm_balance / tm_concentration\n")
bl <- tm_balance(td$monthly_total, "date")
ok(near(bl$balance, bl$exports - bl$imports) && nrow(bl) == length(unique(td$monthly_total$date)), "balance")
ok(identical(names(td$balance)[1:4], c("date", "exports", "imports", "balance")) && "ma3" %in% names(td$balance) &&
     "sum12" %in% names(td$balance), "bundle balance columns")
toyc <- tibble(k = "x", it = c("a", "b", "c"), value_eur = c(10, 10, 0))
tc <- tm_concentration(toyc, "it", "k")
ok(near(tc$hhi, 5000) && near(tc$top1_share, 50) && near(tc$n_effective, 2) && near(tc$top3_share, 100),
   "concentration toy (hhi 5000, top1 50)")
cr <- tm_concentration(td$group_monthly, "group", c("flow", "date"))
ok(all(cr$hhi > 0 & cr$hhi <= 10000), "concentration range on real data")

cat("tm_rolling_item_sums\n")
sub5 <- td$monthly_sitc5 |> filter(product_sitc5 == "54154", flow == "2") |> arrange(date)
full <- tibble(date = sort(unique(td$monthly_total$date))) |>
  left_join(sub5 |> select(date, value_eur), by = "date") |> mutate(value_eur = coalesce(value_eur, 0))
d3 <- as.Date(c("2019-12-01", "2023-06-01", format(ld, "%Y-%m-01")))
rs <- tm_rolling_item_sums(td$monthly_sitc5, "product_sitc5", "flow", 12, dates = d3) |>
  filter(product_sitc5 == "54154", flow == "2")
for (dd in as.list(d3)) {
  dd <- as.Date(dd, origin = "1970-01-01")
  k <- which(full$date == dd)
  bf <- sum(full$value_eur[(k - 11):k])
  ok(near(rs$value[rs$date == dd], bf), paste("rolling 12m for 54154 at", dd))
}
dates50 <- tail(sort(unique(td$monthly_total$date)), 50)
t_roll <- system.time(r50 <- tm_rolling_item_sums(td$monthly_sitc5, "product_sitc5", "flow", 12, dates = dates50))[["elapsed"]]
cat(sprintf("  rolling sums on monthly_sitc5, 50 dates: %.2fs (%d rows)\n", t_roll, nrow(r50)))
ok(t_roll < 3, "rolling item sums fast on 750k rows")
ok(all(c("flow", "product_sitc5", "date", "value") %in% names(r50)), "rolling columns")
tot_roll <- r50 |> group_by(flow, date) |> summarise(s = sum(value), .groups = "drop")
ok(all(tot_roll$s <= (td$total_tf |> filter(date %in% dates50) |> inner_join(tot_roll, by = c("flow", "date")) |>
                       pull(sum12)) * (1 + 1e-9)), "5-digit rolling sums never exceed the total")

cat("tm_unit_values\n")
gu <- td$group_monthly |> filter(group == "Pharmaceuticals")
uv <- tm_unit_values(gu, c("flow", "group"), 12, 2019)
b <- uv |> filter(format(date, "%Y") == "2019") |> group_by(flow) |>
  summarise(v = mean(value_index), k = mean(kg_index, na.rm = TRUE), u = mean(uv_index, na.rm = TRUE), .groups = "drop")
ok(all(abs(b$v - 100) < 1e-8) && all(abs(b$k - 100) < 1e-8) && all(abs(b$u - 100) < 1e-8), "base-year average = 100")
ok(all(c("value", "kg", "unit_value", "value_index", "kg_index", "uv_index") %in% names(uv)), "unit value columns")

cat("Misc data helpers\n")
ok(td$latest_date == tm_latest_date(td$monthly_total), "latest date")
l3 <- tm_sitc_level_monthly(td$monthly_sitc5, 3)
ok(all(nchar(l3$code) == 3) && near(sum(l3$value_eur), sum(td$monthly_sitc5$value_eur)), "sitc level aggregation")
ok(tm_sitc_label("54154", td$sitc_labels) != "54154" && tm_sitc_label("99999x", td$sitc_labels) == "99999x", "sitc label")
ok(nchar(tm_sitc_label("54154", td$sitc_labels, 20)) <= 20, "sitc label truncation")
ok(!any(grepl("\\)$", tm_partner_label(td$partner_labels$partner, td$partner_labels))) &&
     tm_partner_label("ZZ", td$partner_labels) == "ZZ", "partner labels")
ok(td$complete_year == 2025, "complete year is 2025")
ok(all(c("flow", "date", "measure", "value_eur", "yoy_pct") %in% names(td$underlying)) &&
     setequal(levels(td$underlying$measure), c("total", "ex_pharma", "ex_chem", "ex_aircraft")), "underlying structure")
u1 <- td$underlying |> filter(flow == "2", date == ld)
ok(u1$value_eur[u1$measure == "ex_chem"] < u1$value_eur[u1$measure == "ex_pharma"], "ex_chem < ex_pharma")

cat("Formatters\n")
ok(tm_fmt_bn(12.345e9) == paste0(EUR, "12.3bn") && tm_fmt_bn(-1.2e9) == paste0("-", EUR, "1.2bn") && tm_fmt_bn(NA) == "n/a" &&
     tm_fmt_bn(0) == paste0(EUR, "0.0bn"), "tm_fmt_bn")
ok(identical(tm_fmt_eur(c(2.5e9, 3.4e6, -1.2e9, NA, 0)), c(paste0(EUR, "2.5bn"), paste0(EUR, "3.4m"), paste0("-", EUR, "1.2bn"), "n/a", paste0(EUR, "0"))), "tm_fmt_eur")
ok(identical(tm_fmt_pct(c(3.44, -2, 0, NA)), c("+3.4%", "-2.0%", "0.0%", "n/a")), "tm_fmt_pct")
ok(tm_fmt_pp(1.23) == "+1.2pp" && tm_fmt_pp(-0.5) == "-0.5pp" && tm_fmt_pp(NA) == "n/a", "tm_fmt_pp")
ok(tm_fmt_share(43.14) == "43.1%" && tm_fmt_share(NA) == "n/a", "tm_fmt_share")
ok(tm_fmt_month(as.Date("2026-08-01")) == "Aug 2026" && tm_fmt_month_long(as.Date("2026-08-01")) == "August 2026" &&
     tm_fmt_month(as.Date(NA)) == "n/a", "month formats")

cat("Charts (plotly_build forces evaluation)\n")
is_plotly <- function(p, nm) {
  ok(inherits(p, "plotly"), paste(nm, "returns plotly"))
  b <- plotly::plotly_build(p)
  ok(length(b$x$data) >= 0, paste(nm, "builds"))
  invisible(b)
}
tex <- td$total_tf |> filter(flow == "2")
p <- tm_chart_trend(tex); b <- is_plotly(p, "tm_chart_trend")
ok(length(b$x$data) == 2, "trend has raw + smooth traces")
is_plotly(tm_chart_trend(tex, raw_as = "line", name = "Exports"), "tm_chart_trend line")
pe <- tm_add_events(p); be <- is_plotly(pe, "tm_add_events")
ok(length(be$x$layout$shapes) >= 4 && length(be$x$layout$annotations) >= 4, "events added inside data range")
pe2 <- tm_add_events(tm_add_events(p, TM_EVENTS[1, ]), TM_EVENTS[2, ])
ok(length(plotly::plotly_build(pe2)$x$layout$shapes) == 2, "tm_add_events preserves existing shapes")
ok(length(plotly::plotly_build(tm_add_events(p, TM_EVENTS, x_min = as.Date("2024-01-01")))$x$layout$shapes) == 2,
   "events filtered by x_min")
is_plotly(tm_layout(plotly::plot_ly(x = 1:3, y = 1:3), legend = "right", zero_line = TRUE, x_range = c(0, 4)), "tm_layout")
ok(isFALSE(plotly::plotly_build(p)$x$config$displaylogo), "modebar config applied")

tg <- td$group_tf |> filter(flow == "2", date >= as.Date("2022-01-01"))
is_plotly(tm_chart_lines(tg, "date", "ma3", "group", palette = TM_GROUP_COLOURS, scale = 1e9, y_title = "EUR bn"), "tm_chart_lines")
is_plotly(tm_chart_lines(tg, "date", "yoy3_pct", "group", palette = TM_GROUP_COLOURS,
                         highlight = c("Pharmaceuticals", "Aircraft"), y_suffix = "%"), "tm_chart_lines highlight")
is_plotly(tm_chart_lines(td$total_tf, "date", "ma3", "flow_label", dash_by = "flow", scale = 1e9), "tm_chart_lines dash")
cc <- tm_contributions(td$group_monthly, "group", "3m") |> filter(flow == "2", !is.na(contrib_pp))
b <- is_plotly(tm_chart_contrib(cc, "group", palette = TM_GROUP_COLOURS), "tm_chart_contrib")
ok(identical(b$x$layout$barmode, "relative"), "contrib barmode relative")
hm <- td$group_tf |> filter(flow == "2", !is.na(yoy3_pct))
b <- is_plotly(tm_chart_heatmap(hm, "date", "group", "yoy3_pct"), "tm_chart_heatmap")
is_plotly(tm_chart_heatmap(hm, "date", "group", "yoy3_pct", text_values = TRUE), "tm_chart_heatmap text")
tex_all <- td$total_tf |> filter(flow == "2")
is_plotly(tm_chart_seasonal(tex_all), "tm_chart_seasonal")
is_plotly(tm_chart_seasonal(tex_all, cumulative = TRUE, highlight_year = 2024), "tm_chart_seasonal cumulative")
sh <- td$partner_group_monthly |> filter(flow == "2") |> group_by(date) |>
  mutate(share = 100 * value_eur / sum(value_eur)) |> ungroup()
is_plotly(tm_chart_area_share(sh, "date", "share", "partner_group", TM_PARTNER_GROUP_COLOURS), "tm_chart_area_share")
hb <- td$group_tf |> filter(flow == "2", date == ld)
is_plotly(tm_chart_hbar(hb, "group", "value_eur", diverging = FALSE), "tm_chart_hbar positive")
hb2 <- tibble(g = c("A", "B", "C", "D"), v = c(3e9, -1.2e9, 0.4e9, -2e9))
b <- is_plotly(tm_chart_hbar(hb2, "g", "v", top_n = 3), "tm_chart_hbar negative")
ok(b$x$data[[1]]$text[1] == paste0(EUR, "3.0bn") && grepl("^-", b$x$data[[1]]$text[3]), "hbar labels signed and ranked")
is_plotly(tm_chart_hbar(hb2, "g", "v", scale = 1, value_prefix = "", unit = "", value_suffix = "%", digits = 1), "tm_chart_hbar pct")
lab <- c("Total", as.character(TM_GROUPS$group))
val <- c(sum(hb$value_eur), hb$value_eur[match(TM_GROUPS$group, hb$group)])
is_plotly(tm_chart_treemap(lab, c("", rep("Total", 10)), val, colours = c("#ffffff", TM_GROUPS$colour)), "tm_chart_treemap")
bub <- tibble(l = c("a", "b", "c"), x = c(1, 5, 9), y = c(-2, 3, 8), s = c(1, 5, 9))
is_plotly(tm_chart_scatter_bubble(bub, "x", "y", "s", "l", x_title = "X", y_title = "Y"), "tm_chart_scatter_bubble")
is_plotly(tm_empty_plot("No data for this selection"), "tm_empty_plot")

cat("UI helpers\n")
sp <- tm_sparkline(c(1, 3, NA, 2, 5)); ok(grepl("<svg", as.character(sp)) && grepl("polyline", as.character(sp)), "sparkline")
ok(nzchar(as.character(tm_sparkline(c(2, 2, 2)))) && nzchar(as.character(tm_sparkline(NA_real_))) &&
     nzchar(as.character(tm_sparkline(numeric(0)))), "sparkline constant / NA / empty")
ok(grepl("tm-delta-up", as.character(tm_delta_span(3.4))) && grepl("tm-delta-down", as.character(tm_delta_span(-2))) &&
     grepl("n/a", as.character(tm_delta_span(NA))) && grepl("y/y", as.character(tm_delta_span(1, label = "y/y"))), "delta span")
kc <- tm_kpi_card("Exports", paste0(EUR, "12.3bn"), lines = list(tm_delta_span(3.4, label = "y/y"), "plain text"), spark = sp, footnote = "note")
ok(grepl("tm-kpi", as.character(kc)) && grepl("Exports", as.character(kc)), "kpi card renders")
cd <- tm_chart_card("Title", "Sub", plotly::plot_ly(x = 1, y = 1))
ok(grepl("tm-card", as.character(cd)) && grepl("Eurostat Comext", as.character(cd)) && grepl("Sub", as.character(cd)), "chart card renders")
ok(nzchar(as.character(tm_page_css())), "page css")
ok(!is.null(as.character(htmltools::as.tags(tm_chart_card("t", NULL, "x", footer = NULL)))), "chart card without subtitle/footer")

cat("\nFresh-session sourcing with no packages attached\n")
rs <- system2(file.path(R.home("bin"), "Rscript"),
              c("-e", shQuote(sprintf("source('%s'); source('%s'); invisible(TM_COL)",
                                      file.path(app, "R", "analytics.R"), file.path(app, "R", "charts.R")))),
              stdout = TRUE, stderr = TRUE)
ok(is.null(attr(rs, "status")), "sourcing analytics.R and charts.R in a fresh session raises no error")

cat("\nALL FOUNDATION TESTS PASSED\n")
