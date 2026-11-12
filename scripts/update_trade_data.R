# Refresh the processed data for the Irish Trade Monitor entirely from the
# Eurostat Comext monthly API (dataset DS-059331). No local archives, no CN8.
#
# What it produces (partner = World where relevant; imports and exports; EUR + kg),
# written to app/data:
#   monthly_total.parquet          headline monthly goods trade by flow
#   monthly_sitc1.parquet          monthly trade by SITC 1-digit section and flow
#   monthly_sitc5.parquet          monthly trade by SITC 5-digit product and flow
#   monthly_partner_total.parquet  monthly trade by individual partner and flow
#   monthly_partner_sitc1.parquet  monthly trade by partner, SITC 1-digit and flow
#                                  for the top 20 partners in 2025 for each flow
#   annual_total.parquet           annual totals by flow (summed from monthly)
#   annual_sitc1.parquet           annual by SITC 1-digit (summed from monthly)
#   annual_sitc5.parquet           annual by SITC 5-digit (summed from monthly)
#   annual_partner_total.parquet   annual by partner and flow (summed from monthly)
#   annual_partner_sitc1.parquet   annual by partner, SITC 1-digit and flow
#                                  for the top 20 partners in 2025 for each flow
#   sitc5_labels.csv               5-digit SITC descriptions (best-effort fetch)
#   partner_labels.csv             partner code labels (best-effort fetch)
#
# Design choices the user asked for:
#   * Annual series are AGGREGATED FROM THE MONTHLY API, not from any local
#     download. This makes them match what Eurostat shows live.
#   * The detail level is 5-digit SITC (e.g. 54154), not CN8.

required_packages <- c(
  "arrow", "dplyr", "lubridate", "readr", "stringr", "tidyr", "tibble"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ", paste(missing_packages, collapse = ", "),
    "\nInstall them before running this script.",
    call. = FALSE
  )
}

# Larger Comext extracts can take longer than R's default 60-second URL
# timeout, especially over consumer connections. Bump it here so the refresh
# completes instead of failing before any parquet files are written.
options(timeout = max(600, getOption("timeout")))

find_this_file <- function() {
  decode_rscript_path <- function(path) {
    # Rscript encodes spaces in --file= arguments as "~+~" on some setups.
    gsub("~\\+~", " ", path)
  }

  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(
      decode_rscript_path(sub("^--file=", "", file_arg[[1]])),
      winslash = "/",
      mustWork = TRUE
    ))
  }

  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile)) {
      return(normalizePath(frame$ofile, winslash = "/", mustWork = TRUE))
    }
  }

  # Use the file open in RStudio, not a working directory from another project.
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    editor_path <- tryCatch(
      rstudioapi::getSourceEditorContext()$path,
      error = function(e) ""
    )
    if (nzchar(editor_path) && basename(editor_path) == "update_trade_data.R") {
      return(normalizePath(editor_path, winslash = "/", mustWork = TRUE))
    }
  }

  cwd_script <- file.path(getwd(), "scripts", "update_trade_data.R")
  if (file.exists(cwd_script)) {
    return(normalizePath(cwd_script, winslash = "/", mustWork = TRUE))
  }

  NULL
}

script_path <- find_this_file()
if (is.null(script_path)) {
  stop(
    "Cannot locate scripts/update_trade_data.R. Use RStudio's Source button, or run Rscript scripts/update_trade_data.R from the project folder.",
    call. = FALSE
  )
}
project_root <- dirname(dirname(script_path))
monthly_start_period <- "2014-01"
message("Project root: ", project_root)

# ---------------------------------------------------------------------------
# Comext API helpers
# ---------------------------------------------------------------------------
# Key order for DS-059331 is FREQ.REPORTER.PARTNER.PRODUCT.FLOW.INDICATORS.
# Flow 1 = imports (arrivals), flow 2 = exports (dispatches). Leaving a dimension blank returns every code
# in it. We deliberately PIN partner = WORLD for the value series (so we never
# sum WORLD together with the INT_/EXT_ EU and euro-area aggregates, which would
# multiply-count trade ~6x for Ireland), and blank the PRODUCT slot to retrieve
# every SITC level (TOTAL, 1-digit, ... 5-digit) in one call.
trade_flows <- tibble::tibble(
  flow = c("1", "2"),
  flow_label = c("Imports", "Exports")
)

download_comext_csv <- function(key, start_period = monthly_start_period) {
  url <- paste0(
    "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/2.1/data/DS-059331/",
    key,
    "?startPeriod=", start_period,
    "&format=SDMX-CSV"
  )
  message("Downloading ", url)

  first_response <- paste(readLines(url, warn = FALSE), collapse = "\n")

  if (!grepl("<queued>", first_response, fixed = TRUE)) {
    return(readr::read_csv(url, show_col_types = FALSE, progress = FALSE))
  }

  request_id <- stringr::str_match(first_response, "<id>([^<]+)</id>")[, 2]
  if (is.na(request_id) || !nzchar(request_id)) {
    stop("Eurostat returned an asynchronous response without a request id.", call. = FALSE)
  }

  status_url <- paste0(
    "https://ec.europa.eu/eurostat/api/comext/dissemination/1.0/async/status/",
    request_id
  )
  data_url <- paste0(
    "https://ec.europa.eu/eurostat/api/comext/dissemination/1.0/async/data/",
    request_id
  )

  for (poll in seq_len(60)) {
    status_response <- paste(readLines(status_url, warn = FALSE), collapse = "\n")
    status_value <- stringr::str_match(status_response, "<ns1:status>([^<]+)</ns1:status>")[, 2]

    if (identical(status_value, "AVAILABLE")) {
      return(readr::read_csv(data_url, show_col_types = FALSE, progress = FALSE))
    }
    if (identical(status_value, "ERROR")) {
      stop("Eurostat asynchronous extraction failed for request id ", request_id, ".", call. = FALSE)
    }
    if (identical(status_value, "EXPIRED")) {
      stop("Eurostat asynchronous extraction expired for request id ", request_id, ".", call. = FALSE)
    }

    Sys.sleep(2)
  }

  stop("Timed out waiting for Eurostat asynchronous extraction ", request_id, ".", call. = FALSE)
}

parse_monthly_comext <- function(data) {
  data |>
    dplyr::mutate(
      partner = as.character(partner),
      product = as.character(product),
      indicators = as.character(indicators)
    ) |>
    dplyr::transmute(
      partner,
      product,
      indicator = indicators,
      value = OBS_VALUE,
      period = TIME_PERIOD
    ) |>
    dplyr::mutate(
      date = as.Date(paste0(period, "-01")),
      year = lubridate::year(date),
      month = lubridate::month(date)
    ) |>
    dplyr::select(year, month, date, partner, product, indicator, value) |>
    tidyr::pivot_wider(
      names_from = indicator,
      values_from = value,
      values_fill = 0
    ) |>
    dplyr::rename(value_eur = VALUE_EUR, quantity_kg = QUANTITY_KG)
}

# (1) World, all product levels -> total / SITC1 / SITC5 for each flow.
world_products <- lapply(seq_len(nrow(trade_flows)), function(i) {
  flow_code <- trade_flows$flow[[i]]
  flow_label <- trade_flows$flow_label[[i]]
  download_comext_csv(paste0("M.IE.WORLD..", flow_code, ".VALUE_EUR+QUANTITY_KG")) |>
    parse_monthly_comext() |>
    dplyr::mutate(flow = flow_code, flow_label = flow_label)
}) |>
  dplyr::bind_rows()

# (2) All partners, TOTAL product -> partner ranking for each flow.
partner_total_raw <- lapply(seq_len(nrow(trade_flows)), function(i) {
  flow_code <- trade_flows$flow[[i]]
  flow_label <- trade_flows$flow_label[[i]]
  download_comext_csv(paste0("M.IE..TOTAL.", flow_code, ".VALUE_EUR+QUANTITY_KG")) |>
    parse_monthly_comext() |>
    dplyr::mutate(flow = flow_code, flow_label = flow_label)
}) |>
  dplyr::bind_rows()

sum_value <- function(df, keys) {
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      value_eur = sum(value_eur, na.rm = TRUE),
      quantity_kg = sum(quantity_kg, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------
# Monthly series (World basis)
# ---------------------------------------------------------------------------
monthly_total <- world_products |>
  dplyr::filter(product == "TOTAL") |>
  sum_value(c("flow", "flow_label", "year", "month", "date")) |>
  dplyr::arrange(flow, date)

monthly_sitc1 <- world_products |>
  dplyr::filter(stringr::str_detect(product, "^[0-9]$")) |>
  dplyr::rename(sitc1 = product) |>
  sum_value(c("flow", "flow_label", "year", "month", "date", "sitc1")) |>
  dplyr::arrange(flow, date, sitc1)

monthly_sitc5 <- world_products |>
  dplyr::filter(stringr::str_detect(product, "^[0-9]{5}$")) |>
  dplyr::mutate(sitc1 = stringr::str_sub(product, 1, 1)) |>
  dplyr::rename(product_sitc5 = product) |>
  sum_value(c("flow", "flow_label", "year", "month", "date", "sitc1", "product_sitc5")) |>
  dplyr::arrange(flow, date, dplyr::desc(value_eur))

# Individual reporting partners only (2-letter ISO); aggregates removed.
partner_aggregate_codes <- c(
  "WORLD",
  "INT_EU", "EXT_EU", "INT_EU27_2020", "EXT_EU27_2020", "INT_EU28", "EXT_EU28",
  "INT_EA", "EXT_EA", "INT_EA21", "EXT_EA21", "EU27_2020", "EA21"
)
monthly_partner_total <- partner_total_raw |>
  dplyr::filter(
    !partner %in% partner_aggregate_codes,
    stringr::str_detect(partner, "^[A-Z]{2}$")
  ) |>
  sum_value(c("flow", "flow_label", "year", "month", "date", "partner")) |>
  dplyr::arrange(flow, date, dplyr::desc(value_eur))

# ---------------------------------------------------------------------------
# Annual series: aggregated from the monthly observations above.
# ---------------------------------------------------------------------------
annual_total <- monthly_total |>
  sum_value(c("flow", "flow_label", "year")) |>
  dplyr::arrange(flow, year)

annual_sitc1 <- monthly_sitc1 |>
  sum_value(c("flow", "flow_label", "year", "sitc1")) |>
  dplyr::arrange(flow, year, sitc1)

annual_sitc5 <- monthly_sitc5 |>
  sum_value(c("flow", "flow_label", "year", "sitc1", "product_sitc5")) |>
  dplyr::arrange(flow, year, dplyr::desc(value_eur))

annual_partner_total <- monthly_partner_total |>
  sum_value(c("flow", "flow_label", "year", "partner")) |>
  dplyr::arrange(flow, year, dplyr::desc(value_eur))

destination_choice_year <- if (2025L %in% annual_partner_total$year) 2025L else max(annual_partner_total$year)
top_destination_requests <- annual_partner_total |>
  dplyr::filter(year == destination_choice_year) |>
  dplyr::group_by(flow, flow_label) |>
  dplyr::slice_max(order_by = value_eur, n = 20, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::distinct(flow, flow_label, partner)

download_partner_sitc1 <- function(flow_code, flow_label, partner_code) {
  key <- paste0("M.IE.", partner_code, ".0+1+2+3+4+5+6+7+8+9.", flow_code, ".VALUE_EUR+QUANTITY_KG")
  download_comext_csv(key) |>
    parse_monthly_comext() |>
    dplyr::filter(stringr::str_detect(product, "^[0-9]$")) |>
    dplyr::rename(sitc1 = product) |>
    dplyr::mutate(flow = flow_code, flow_label = flow_label, partner = partner_code) |>
    sum_value(c("flow", "flow_label", "year", "month", "date", "partner", "sitc1"))
}

monthly_partner_sitc1 <- lapply(seq_len(nrow(top_destination_requests)), function(i) {
  download_partner_sitc1(
    flow_code = top_destination_requests$flow[[i]],
    flow_label = top_destination_requests$flow_label[[i]],
    partner_code = top_destination_requests$partner[[i]]
  )
}) |>
  dplyr::bind_rows() |>
  dplyr::arrange(flow, date, partner, sitc1)

annual_partner_sitc1 <- monthly_partner_sitc1 |>
  sum_value(c("flow", "flow_label", "year", "partner", "sitc1")) |>
  dplyr::arrange(flow, year, partner, sitc1)

# ---------------------------------------------------------------------------
# 5-digit SITC labels (best effort). Pulls the product codelist from the DSD
# and keeps the 5-digit numeric codes. Any failure is non-fatal: the app shows
# bare codes if the CSV is absent. Once fetched the CSV is committed, so the
# labels travel with the app ("fetch once, bundle thereafter").
# ---------------------------------------------------------------------------
fetch_ds059331_labels <- function() {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    message("xml2 not installed; skipping best-effort label fetches.")
    return(NULL)
  }
  url <- paste0(
    "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/2.1/",
    "datastructure/ESTAT/DS-059331?references=children"
  )
  tryCatch({
    doc <- xml2::read_xml(url)
    codes <- xml2::xml_find_all(doc, ".//*[local-name()='Code']")
    if (length(codes) == 0) return(NULL)
    ids <- xml2::xml_attr(codes, "id")
    descs <- vapply(codes, function(node) {
      nm <- xml2::xml_find_first(node, ".//*[local-name()='Name']")
      if (length(nm) == 0) NA_character_ else xml2::xml_text(nm)
    }, character(1))
    tibble::tibble(code = ids, label = descs) |>
      dplyr::distinct(code, .keep_all = TRUE)
  }, error = function(e) {
    message("Could not fetch labels (", conditionMessage(e), "); continuing without descriptions.")
    NULL
  })
}

all_labels <- fetch_ds059331_labels()

sitc5_labels <- if (is.null(all_labels)) {
  NULL
} else {
  all_labels |>
    dplyr::filter(stringr::str_detect(code, "^[0-9]{5}$")) |>
    dplyr::transmute(product_sitc5 = code, sitc5_desc = label) |>
    dplyr::arrange(product_sitc5)
}

# Full SITC hierarchy labels (1- to 5-digit) for the in-app product finder,
# which lets users browse any level of granularity. The codelist endpoint above
# already returns every level, so we simply keep all numeric codes here rather
# than only the 5-digit ones.
sitc_labels <- if (is.null(all_labels)) {
  NULL
} else {
  all_labels |>
    dplyr::filter(stringr::str_detect(code, "^[0-9]{1,5}$")) |>
    dplyr::transmute(
      sitc_code = code,
      level = nchar(code),
      sitc_desc = stringr::str_squish(label)
    ) |>
    dplyr::arrange(level, sitc_code)
}

partner_labels <- if (is.null(all_labels)) {
  NULL
} else {
  all_labels |>
    dplyr::filter(stringr::str_detect(code, "^[A-Z]{2}$")) |>
    dplyr::transmute(partner = code, partner_label = label) |>
    dplyr::arrange(partner)
}

# ---------------------------------------------------------------------------
# Write outputs
# ---------------------------------------------------------------------------
output_dir <- file.path(project_root, "app", "data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

arrow::write_parquet(monthly_total, file.path(output_dir, "monthly_total.parquet"))
arrow::write_parquet(monthly_sitc1, file.path(output_dir, "monthly_sitc1.parquet"))
arrow::write_parquet(monthly_sitc5, file.path(output_dir, "monthly_sitc5.parquet"))
arrow::write_parquet(monthly_partner_total, file.path(output_dir, "monthly_partner_total.parquet"))
arrow::write_parquet(monthly_partner_sitc1, file.path(output_dir, "monthly_partner_sitc1.parquet"))
arrow::write_parquet(annual_total, file.path(output_dir, "annual_total.parquet"))
arrow::write_parquet(annual_sitc1, file.path(output_dir, "annual_sitc1.parquet"))
arrow::write_parquet(annual_sitc5, file.path(output_dir, "annual_sitc5.parquet"))
arrow::write_parquet(annual_partner_total, file.path(output_dir, "annual_partner_total.parquet"))
arrow::write_parquet(annual_partner_sitc1, file.path(output_dir, "annual_partner_sitc1.parquet"))

if (!is.null(sitc5_labels) && nrow(sitc5_labels) > 0) {
  readr::write_csv(sitc5_labels, file.path(output_dir, "sitc5_labels.csv"))
  message("Wrote ", nrow(sitc5_labels), " SITC 5-digit labels.")
} else {
  message("No SITC labels written; drop a CSV with columns product_sitc5,sitc5_desc into app/data to add descriptions.")
}

if (!is.null(sitc_labels) && nrow(sitc_labels) > 0) {
  readr::write_csv(sitc_labels, file.path(output_dir, "sitc_labels.csv"))
  message("Wrote ", nrow(sitc_labels), " SITC labels across levels 1-5 (product finder).")
} else {
  message("No multi-level SITC labels written; the product finder will show bare codes at 2-4 digit levels.")
}

if (!is.null(partner_labels) && nrow(partner_labels) > 0) {
  readr::write_csv(partner_labels, file.path(output_dir, "partner_labels.csv"))
  message("Wrote ", nrow(partner_labels), " partner labels.")
}

message("Wrote processed data to ", output_dir)

# Pre-compute the Monthly monitoring "drivers" decompositions so the hosted app
# serves the common cases instantly without live calls. Best-effort: any failure
# here must not invalidate the core refresh above.
tryCatch({
  drivers_script <- file.path(project_root, "scripts", "precompute_drivers.R")
  if (file.exists(drivers_script)) {
    message("Pre-computing move drivers (this makes several Eurostat calls)...")
    sys.source(drivers_script, envir = new.env())
  }
}, error = function(e) message("Driver pre-compute skipped: ", conditionMessage(e)))

# Re-render the PDF briefing so the app's "Download PDF briefing" link matches
# the refreshed data. Best-effort, like the step above: it needs pandoc and
# XeLaTeX, and the refresh is still valid without it.
tryCatch({
  briefing_script <- file.path(project_root, "scripts", "render_briefing.R")
  if (file.exists(briefing_script)) {
    sys.source(briefing_script, envir = new.env())
  }
}, error = function(e) message("PDF briefing skipped: ", conditionMessage(e)))

message("Years covered: ", paste(sort(unique(monthly_total$year)), collapse = ", "))
message("Latest month: ", format(max(monthly_total$date), "%Y-%m"))
message("Distinct SITC5 products: ", dplyr::n_distinct(monthly_sitc5$product_sitc5))
latest_complete_year <- max(annual_sitc5$year[annual_sitc5$year < lubridate::year(Sys.Date())], na.rm = TRUE)
top5 <- annual_sitc5 |>
  dplyr::filter(flow == "2", year == latest_complete_year) |>
  dplyr::slice_max(value_eur, n = 5)
if (nrow(top5) > 0) {
  message("Top export SITC5 in latest complete year:")
  print(top5[, c("flow_label", "year", "product_sitc5", "value_eur")])
}
