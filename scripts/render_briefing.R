# Render the PDF briefing (report/briefing.Rmd) from the bundled app data.
#
#   Rscript scripts/render_briefing.R             # two pages: overview + exports
#   Rscript scripts/render_briefing.R --imports   # adds a third page on imports
#
# The PDF is written to app/www/briefing/irish-trade-briefing-YYYY-MM.pdf, named
# after the latest month in the data. The app's Briefing page shows a
# "Download PDF briefing" link when the file for its latest month exists, so
# the hosted app needs neither pandoc nor LaTeX.
#
# Run after scripts/update_trade_data.R (the refresh calls it automatically).
# Needs the rmarkdown, knitr and ggplot2 packages, pandoc (the copy bundled
# with RStudio is found automatically) and XeLaTeX (MacTeX / TeX Live /
# TinyTeX). showtext is optional: with it the charts use Lato.

suppressWarnings(suppressMessages({
  library(dplyr)
  library(arrow)
  library(lubridate)
}))

required_packages <- c("rmarkdown", "knitr", "ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required packages: ", paste(missing_packages, collapse = ", "),
       "\nInstall them before running this script.", call. = FALSE)
}

# Locate the project root (this file lives in scripts/).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
this_file <- if (length(file_arg)) normalizePath(sub("^--file=", "", file_arg[[1]])) else "scripts/render_briefing.R"
project_root <- normalizePath(file.path(dirname(this_file), ".."))
app_dir <- file.path(project_root, "app")
report_dir <- file.path(project_root, "report")
imports_page <- "--imports" %in% commandArgs(trailingOnly = TRUE)

# ---- Toolchain --------------------------------------------------------------

# pandoc is on the PATH inside RStudio but usually not for a terminal Rscript,
# so fall back to the copies bundled with RStudio / Positron / Quarto / Homebrew.
ensure_pandoc <- function() {
  if (rmarkdown::pandoc_available("2.0")) return(invisible(TRUE))
  candidates <- c(
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64",
    "/Applications/RStudio.app/Contents/MacOS/quarto/bin/tools",
    "/Applications/Positron.app/Contents/Resources/app/quarto/bin/tools",
    "/Applications/quarto/bin/tools",
    "/opt/homebrew/bin", "/usr/local/bin", "/usr/lib/rstudio/bin/quarto/bin/tools"
  )
  for (dir in candidates[file.exists(file.path(candidates, "pandoc"))]) {
    rmarkdown::find_pandoc(cache = FALSE, dir = dir)
    if (rmarkdown::pandoc_available("2.0")) return(invisible(TRUE))
  }
  stop("pandoc was not found. Install it (for example `brew install pandoc`) ",
       "or set the RSTUDIO_PANDOC environment variable to its folder.", call. = FALSE)
}

ensure_xelatex <- function() {
  if (nzchar(Sys.which("xelatex"))) return(invisible(TRUE))
  for (dir in c("/Library/TeX/texbin", "/usr/local/texlive/bin", "~/Library/TinyTeX/bin/universal-darwin")) {
    if (file.exists(file.path(path.expand(dir), "xelatex"))) {
      Sys.setenv(PATH = paste(path.expand(dir), Sys.getenv("PATH"), sep = .Platform$path.sep))
      return(invisible(TRUE))
    }
  }
  stop("XeLaTeX was not found. Install MacTeX / TeX Live, or run tinytex::install_tinytex().", call. = FALSE)
}

ensure_pandoc()
ensure_xelatex()

# ---- Output name: taken from the latest month in the data ---------------------

# local = TRUE: when the data refresh runs this script, nothing leaks into it.
source(file.path(app_dir, "R", "analytics.R"), local = TRUE)
source(file.path(app_dir, "R", "mod_briefing.R"), local = TRUE)
monthly_total <- arrow::read_parquet(file.path(app_dir, "data", "monthly_total.parquet"))
latest_date <- tm_latest_date(monthly_total)
out_dir <- file.path(app_dir, "www", "briefing")
out_file <- file.path(out_dir, briefing_pdf_file(latest_date))

# ---- Render -------------------------------------------------------------------

# Work in a temporary folder: LaTeX intermediates stay out of the project (and
# out of iCloud), and paths with spaces or "~" never reach LaTeX.
work_dir <- tempfile("briefing_")
dir.create(work_dir)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

message("Rendering the PDF briefing for ", tm_fmt_month_long(latest_date), "...")
tryCatch({
  sources <- c("briefing.Rmd", "briefing_charts.R", "preamble.tex")
  if (!all(file.copy(file.path(report_dir, sources), work_dir))) {
    stop("Could not copy the report sources from ", report_dir, call. = FALSE)
  }
  rendered <- rmarkdown::render(
    file.path(work_dir, "briefing.Rmd"),
    output_file = "briefing.pdf",
    params = list(app_dir = app_dir, imports_page = imports_page),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  if (!file.copy(rendered, out_file, overwrite = TRUE)) {
    stop("Could not write ", out_file, call. = FALSE)
  }
}, finally = unlink(work_dir, recursive = TRUE))
message("Wrote ", out_file)
