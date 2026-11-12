# Tests for the PDF briefing: report/briefing_charts.R and the download link in
# app/R/mod_briefing.R. The full render (pandoc + XeLaTeX) is exercised by
# scripts/render_briefing.R, not here.
# Run from the project root:  Rscript tests/test_briefing_pdf.R
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(dplyr); library(ggplot2); library(plotly)
  library(DT); library(scales); library(arrow); library(tidyr); library(readr)
})

args <- commandArgs(trailingOnly = FALSE)
f <- sub("^--file=", "", args[grepl("^--file=", args)])
root <- if (length(f)) normalizePath(file.path(dirname(f), "..")) else getwd()
app <- file.path(root, "app")
for (s in c("trade_utils.R", "analytics.R", "charts.R", "mod_briefing.R", "mod_structure.R", "mod_markets.R")) {
  source(file.path(app, "R", s))
}
source(file.path(root, "report", "briefing_charts.R"))

ok <- function(cond, msg) {
  if (!isTRUE(cond)) stop("TEST FAILED: ", msg, call. = FALSE)
  cat("  ok -", msg, "\n")
}

cat("Loading data and building bundle\n")
td <- tm_build_bundle(read_trade_data(app))
d <- td$latest_date
met <- briefing_metrics(td)

cat("Source files are ASCII\n")
for (p in c(file.path("report", "briefing_charts.R"), file.path("report", "briefing.Rmd"),
            file.path("report", "preamble.tex"), file.path("scripts", "render_briefing.R"))) {
  src <- readLines(file.path(root, p), warn = FALSE, encoding = "bytes")
  ok(!any(grepl("[^\x01-\x7F]", src, perl = TRUE, useBytes = TRUE)), paste(p, "is ASCII-only"))
}

cat("Shared chart data\n")
for (fl in c("1", "2")) {
  g <- briefing_growth_data(td, fl)
  ok(setequal(unique(as.character(g$series$measure)), names(g$measures)), paste("growth measures, flow", fl))
  tot <- g$series$yoy3_pct[as.character(g$series$measure) == "total" & g$series$date == d]
  ok(isTRUE(all.equal(tot, briefing_metrics(td)[[if (fl == "2") "exp" else "imp"]]$yoy3)),
     paste("growth chart total equals the headline 3m y/y, flow", fl))
  ok(is.null(g$yrange) || (g$yrange[1] <= min(0, tot) && g$yrange[2] >= max(0, tot)),
     paste("clipped range keeps zero and the latest total, flow", fl))
  cc <- briefing_drivers_data(td, fl)
  last <- cc[cc$date == d, ]
  ok(nrow(last) == length(TM_GROUP_LEVELS), paste("drivers has every group in the latest month, flow", fl))
  ok(isTRUE(all.equal(sum(last$contrib_pp), last$total_growth_pct[1])),
     paste("driver contributions sum to total growth, flow", fl))
  ok(max(cc$date) == d && length(unique(cc$date)) <= 36, paste("drivers window is the last 36 months, flow", fl))
}

cat("Static charts build (both flows)\n")
builds <- function(p) inherits(ggplot2::ggplot_build(p), "ggplot_built")
ok(builds(pdf_chart_trade(td)), "trade chart builds")
for (fl in c("1", "2")) {
  ok(builds(pdf_chart_growth(td, fl)), paste("growth chart builds, flow", fl))
  ok(builds(pdf_chart_drivers(td, fl)), paste("drivers chart builds, flow", fl))
  for (k in c("product", "market")) {
    ok(builds(pdf_chart_movers(td, fl, k)), paste(k, "movers chart builds, flow", fl))
  }
}
out <- file.path(tempfile("figs_"))
path <- pdf_save(pdf_chart_trade(td), "trade", 17.8, 9.3, dir = out)
ok(file.exists(path) && file.size(path) > 1000, "pdf_save writes a PDF")
unlink(out, recursive = TRUE)

cat("LaTeX helpers\n")
ok(pdf_tex("50% of R&D_x #1 {a} $5") == "50\\% of R\\&D\\_x \\#1 \\{a\\} \\$5", "pdf_tex escapes specials")
ok(pdf_tex("a\\b~c^d") == "a\\textbackslash b\\textasciitilde{}c\\textasciicircum{}d", "pdf_tex escapes backslash, tilde, caret")
ok(pdf_delta(3.44, "%", 1, "y/y") == "\\tmup{+3.4\\%} \\tmlab{y/y}", "positive delta")
ok(pdf_delta(-2, "pp") == "\\tmdown{-2.0pp}", "negative delta")
ok(pdf_delta(0.01, "%") == "\\tmflat{0.0\\%}", "zero delta has no sign")
ok(pdf_delta(NA_real_, "%", 1, "y/y") == "\\tmflat{n/a} \\tmlab{y/y}", "missing delta")
ok(pdf_kpi("#004d44", "Exports", "x", c("a", "b")) == "\\tmkpi{004D44}{Exports}{x}{a\\\\ b}", "kpi tile")

tiles <- pdf_briefing_tiles(met)
ok(length(tiles) == 5 && all(grepl("^\\\\tmkpi\\{[0-9A-F]{6}\\}", tiles)), "five headline tiles")
ok(grepl(pdf_tex(tm_fmt_bn(met$exp$value)), tiles[1], fixed = TRUE), "exports tile shows the headline value")
ok(grepl(pdf_tex(tm_fmt_share(met$us$share12)), tiles[5], fixed = TRUE), "US tile shows the 12-month share")
ex_tiles <- pdf_exposure_tiles(td, "2")
ok(length(ex_tiles) == 5 && all(grepl("^\\\\tmkpi\\{", ex_tiles)), "five exposure tiles")
ok(grepl(pdf_tex(tm_fmt_share(structure_kpis(td, "2")$chem$share)), ex_tiles[1], fixed = TRUE),
   "chemicals & pharma tile matches the Concentration page")

cat("Download link\n")
ok(briefing_pdf_file(as.Date("2026-08-01")) == "irish-trade-briefing-2026-08.pdf", "file name carries the data month")
tmp_app <- tempfile("app_")
dir.create(file.path(tmp_app, "www", "briefing"), recursive = TRUE)
ok(is.null(briefing_pdf_link(td, tmp_app)), "no link when the PDF for the latest month is missing")
invisible(file.create(file.path(tmp_app, "www", "briefing", briefing_pdf_file(as.Date("2000-01-01")))))
ok(is.null(briefing_pdf_link(td, tmp_app)), "no link for a PDF of another month")
invisible(file.create(file.path(tmp_app, "www", "briefing", briefing_pdf_file(d))))
link <- briefing_pdf_link(td, tmp_app)
ok(inherits(link, "shiny.tag") && link$attribs$href == paste0("briefing/", briefing_pdf_file(d)),
   "link points at the PDF for the latest month")
unlink(tmp_app, recursive = TRUE)

cat("\nAll PDF briefing tests passed.\n")
