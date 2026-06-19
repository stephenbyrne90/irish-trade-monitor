# Shared data-loading, labelling and YTD-contribution helpers for the
# Irish Trade Monitor app and the companion Quarto site.
#
# Classification note. Everything in this project is SITC (Standard
# International Trade Classification, Rev. 4):
#   - SITC 1-digit (0-9)  : 10 broad sections (the contribution chart).
#   - SITC 5-digit        : the detailed product level used for drill-down.
# CN8 (8-digit customs) is intentionally NOT used anywhere; the detail level
# is 5-digit SITC throughout.

sitc1_reference <- tibble::tibble(
  sitc1 = as.character(0:9),
  sitc1_label = c(
    "0 Food and live animals",
    "1 Beverages and tobacco",
    "2 Crude materials, inedible, except fuels",
    "3 Mineral fuels, lubricants and related materials",
    "4 Animal and vegetable oils, fats and waxes",
    "5 Chemicals and related products",
    "6 Manufactured goods classified chiefly by material",
    "7 Machinery and transport equipment",
    "8 Miscellaneous manufactured articles",
    "9 Commodities and transactions n.e.s."
  )
)

trade_flow_reference <- tibble::tibble(
  flow = c("1", "2"),
  flow_label = c("Imports", "Exports"),
  partner_role = c("Origin country", "Destination country")
)

# Source note reused across charts and pages so provenance is always visible.
# Everything now comes from the monthly Comext API on a partner = World basis;
# annual figures are aggregated from those monthly observations.
EUROSTAT_SOURCE_NOTE <- paste(
  "Source: Eurostat Comext, monthly API dataset DS-059331",
  "(reporter = Ireland, partner = World). Annual figures are summed",
  "from the monthly series. SITC Rev. 4 classification."
)

# Read a single processed parquet and coerce the period keys to integers.
# Parquet may store year/month as doubles; integers keep dropdown labels and
# year-on-year arithmetic clean throughout the app.
.read_processed <- function(path) {
  df <- arrow::read_parquet(path) |> dplyr::collect()
  if ("flow" %in% names(df)) {
    df$flow <- as.character(df$flow)
  } else {
    df$flow <- "2"
  }
  if ("flow_label" %in% names(df)) {
    df$flow_label <- as.character(df$flow_label)
  } else {
    df$flow_label <- flow_label_value(df$flow)
  }
  for (col in intersect(c("year", "month"), names(df))) {
    df[[col]] <- as.integer(round(df[[col]]))
  }
  df
}

# Optional, tolerant reader: returns the parquet if present, otherwise an
# empty tibble with the expected columns so the app degrades gracefully when
# the detailed SITC5 series has not been fetched yet.
.read_optional <- function(path, empty) {
  if (file.exists(path)) .read_processed(path) else empty
}

# Raw optional reader: returns the parquet exactly as stored (no flow_label /
# period coercion), for auxiliary tables that already carry the right schema.
.read_optional_raw <- function(path, empty) {
  if (file.exists(path)) dplyr::collect(arrow::read_parquet(path)) else empty
}

# Optional 5-digit SITC product descriptions, written by the refresh script as
# data/sitc5_labels.csv when the Eurostat codelist is reachable. Absent file =>
# the app shows bare codes.
read_sitc5_labels <- function(app_dir = ".") {
  path <- file.path(app_dir, "data", "sitc5_labels.csv")
  empty <- tibble::tibble(product_sitc5 = character(), sitc5_desc = character())
  if (!file.exists(path)) {
    return(empty)
  }
  out <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )
  if (is.null(out) || !all(c("product_sitc5", "sitc5_desc") %in% names(out))) {
    return(empty)
  }
  out |>
    dplyr::transmute(
      product_sitc5 = as.character(product_sitc5),
      sitc5_desc = as.character(sitc5_desc)
    ) |>
    dplyr::distinct(product_sitc5, .keep_all = TRUE)
}

# Full SITC hierarchy labels (levels 1-5) used by the in-app product finder.
# Written by the refresh script as data/sitc_labels.csv. Absent file => the
# finder falls back to bare codes at the levels that lack descriptions.
read_sitc_labels <- function(app_dir = ".") {
  path <- file.path(app_dir, "data", "sitc_labels.csv")
  empty <- tibble::tibble(sitc_code = character(), level = integer(), sitc_desc = character())
  if (!file.exists(path)) {
    return(empty)
  }
  out <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )
  if (is.null(out) || !all(c("sitc_code", "level", "sitc_desc") %in% names(out))) {
    return(empty)
  }
  out |>
    dplyr::transmute(
      sitc_code = as.character(sitc_code),
      level = as.integer(level),
      sitc_desc = as.character(sitc_desc)
    ) |>
    dplyr::distinct(sitc_code, .keep_all = TRUE)
}

read_partner_labels <- function(app_dir = ".") {
  path <- file.path(app_dir, "data", "partner_labels.csv")
  empty <- tibble::tibble(partner = character(), partner_label = character())
  if (!file.exists(path)) {
    return(empty)
  }
  out <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )
  if (is.null(out) || !all(c("partner", "partner_label") %in% names(out))) {
    return(empty)
  }
  out |>
    dplyr::transmute(
      partner = as.character(partner),
      partner_label = as.character(partner_label)
    ) |>
    dplyr::distinct(partner, .keep_all = TRUE)
}

# Eurostat Comext sometimes serves larger extracts asynchronously: the initial
# request returns XML with a queued request id, which then has to be polled via
# the async status and data endpoints. Smaller extracts still arrive directly as
# CSV, so this helper handles both cases transparently.
read_comext_csv <- function(url, poll_interval = 2, max_polls = 60) {
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

  for (poll in seq_len(max_polls)) {
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

    Sys.sleep(poll_interval)
  }

  stop("Timed out waiting for Eurostat asynchronous extraction ", request_id, ".", call. = FALSE)
}

read_trade_data <- function(app_dir = ".") {
  data_dir <- file.path(app_dir, "data")

  # Core series the app cannot run without.
  required_files <- c(
    "monthly_total.parquet",
    "monthly_sitc1.parquet",
    "monthly_partner_total.parquet",
    "annual_total.parquet",
    "annual_sitc1.parquet",
    "annual_partner_total.parquet"
  )

  missing_files <- required_files[!file.exists(file.path(data_dir, required_files))]
  if (length(missing_files) > 0) {
    stop(
      "Missing processed data files in ", normalizePath(data_dir, mustWork = FALSE),
      ": ", paste(missing_files, collapse = ", "),
      "\nRun scripts/update_trade_data.R from the project root to (re)create them.",
      call. = FALSE
    )
  }

  empty_monthly_sitc5 <- tibble::tibble(
    flow = character(),
    year = integer(), month = integer(), date = as.Date(character()),
    sitc1 = character(), product_sitc5 = character(),
    value_eur = numeric(), quantity_kg = numeric()
  )
  empty_annual_sitc5 <- tibble::tibble(
    flow = character(),
    year = integer(), sitc1 = character(), product_sitc5 = character(),
    value_eur = numeric(), quantity_kg = numeric()
  )
  empty_monthly_partner_sitc1 <- tibble::tibble(
    flow = character(),
    year = integer(), month = integer(), date = as.Date(character()),
    partner = character(), sitc1 = character(),
    value_eur = numeric(), quantity_kg = numeric()
  )
  empty_annual_partner_sitc1 <- tibble::tibble(
    flow = character(), year = integer(), partner = character(), sitc1 = character(),
    value_eur = numeric(), quantity_kg = numeric()
  )

  # Optional pre-computed "drivers" (decomposition of a country's or product's
  # change into its components), written by scripts/precompute_drivers.R. The
  # app falls back to a live Eurostat fetch when a selection is not bundled.
  empty_drivers <- tibble::tibble(
    flow = character(), focus_code = character(), basis = character(),
    focus_month = as.Date(character()), comp_month = as.Date(character()),
    component_code = character(), component_label = character(),
    cur_eur = numeric(), prev_eur = numeric(), delta_eur = numeric(),
    contrib_pp = numeric()
  )

  list(
    monthly_total = .read_processed(file.path(data_dir, "monthly_total.parquet")),
    monthly_sitc1 = .read_processed(file.path(data_dir, "monthly_sitc1.parquet")),
    monthly_sitc5 = .read_optional(file.path(data_dir, "monthly_sitc5.parquet"), empty_monthly_sitc5),
    monthly_partner_total = .read_processed(file.path(data_dir, "monthly_partner_total.parquet")),
    monthly_partner_sitc1 = .read_optional(file.path(data_dir, "monthly_partner_sitc1.parquet"), empty_monthly_partner_sitc1),
    annual_total = .read_processed(file.path(data_dir, "annual_total.parquet")),
    annual_sitc1 = .read_processed(file.path(data_dir, "annual_sitc1.parquet")),
    annual_sitc5 = .read_optional(file.path(data_dir, "annual_sitc5.parquet"), empty_annual_sitc5),
    annual_partner_total = .read_processed(file.path(data_dir, "annual_partner_total.parquet")),
    annual_partner_sitc1 = .read_optional(file.path(data_dir, "annual_partner_sitc1.parquet"), empty_annual_partner_sitc1),
    drivers_country = .read_optional_raw(file.path(data_dir, "drivers_country.parquet"), empty_drivers),
    drivers_product = .read_optional_raw(file.path(data_dir, "drivers_product.parquet"), empty_drivers),
    sitc5_labels = read_sitc5_labels(app_dir),
    sitc_labels = read_sitc_labels(app_dir),
    partner_labels = read_partner_labels(app_dir)
  )
}

label_eur_billions <- scales::label_number(
  scale = 1e-9,
  suffix = "bn",
  accuracy = 0.1,
  big.mark = ","
)

label_trade_value <- function(x) {
  out <- dplyr::case_when(
    is.na(x) ~ NA_character_,
    abs(x) >= 1e9 ~ scales::number(x / 1e9, accuracy = 0.1, big.mark = ",", suffix = "bn"),
    abs(x) >= 1e6 ~ scales::number(x / 1e6, accuracy = 0.1, big.mark = ",", suffix = "m"),
    abs(x) >= 1e3 ~ scales::number(x / 1e3, accuracy = 0.1, big.mark = ",", suffix = "k"),
    TRUE ~ scales::number(x, accuracy = 1, big.mark = ",")
  )
  unname(out)
}

label_pct <- scales::label_number(
  accuracy = 0.1,
  suffix = "%"
)

label_share <- scales::label_percent(
  accuracy = 0.1
)

label_change_pct <- function(x) {
  out <- scales::number(
    x * 100,
    accuracy = 0.1,
    big.mark = ",",
    suffix = "%"
  )
  ifelse(is.na(x), NA_character_, out)
}

flow_label_value <- function(flow_code) {
  label <- trade_flow_reference$flow_label[match(as.character(flow_code), trade_flow_reference$flow)]
  if (length(label) == 0 || is.na(label)) as.character(flow_code) else label
}

partner_role_value <- function(flow_code) {
  label <- trade_flow_reference$partner_role[match(as.character(flow_code), trade_flow_reference$flow)]
  if (length(label) == 0 || is.na(label)) "Partner country" else label
}

prepare_sitc1 <- function(data) {
  data |>
    dplyr::left_join(sitc1_reference, by = "sitc1") |>
    dplyr::mutate(
      sitc1 = factor(sitc1, levels = sitc1_reference$sitc1),
      sitc1_label = factor(sitc1_label, levels = sitc1_reference$sitc1_label)
    )
}

# Attach the parent 1-digit section and (optional) product descriptions to a
# 5-digit SITC table, and build a display label "code - description". Handles
# zero-row inputs cleanly so the app works before SITC5 data is fetched.
prepare_sitc5 <- function(data, labels = NULL) {
  if (!"sitc1" %in% names(data)) {
    data <- dplyr::mutate(
      data,
      sitc1 = dplyr::if_else(
        as.character(product_sitc5) == "TOTAL",
        NA_character_,
        substr(as.character(product_sitc5), 1, 1)
      )
    )
  } else {
    data <- dplyr::mutate(data, sitc1 = as.character(sitc1))
  }
  if (is.null(labels)) {
    labels <- tibble::tibble(product_sitc5 = character(), sitc5_desc = character())
  }

  data |>
    prepare_sitc1() |>
    dplyr::left_join(labels, by = "product_sitc5") |>
    dplyr::mutate(
      sitc5_desc = dplyr::case_when(
        product_sitc5 == "TOTAL" ~ "All goods",
        is.na(sitc5_desc) ~ NA_character_,
        TRUE ~ sitc5_desc
      ),
      product_label = dplyr::case_when(
        product_sitc5 == "TOTAL" ~ "TOTAL - All goods",
        is.na(sitc5_desc) | sitc5_desc == "" ~ product_sitc5,
        TRUE ~ paste0(product_sitc5, " - ", sitc5_desc)
      )
    )
}

compute_ytd_contributions <- function(monthly_sitc1) {
  # Decompose year-to-date (YTD) year-on-year export growth into additive
  # percentage-point (pp) contributions from each SITC 1-digit section.
  #
  # For each month m in year t and section s:
  #   1. YTD_s,t      = sum of section s exports from January to month m, year t.
  #   2. YTD_total,t  = sum over all sections of YTD_s,t.
  #   3. contribution_s (pp) = 100 * (YTD_s,t - YTD_s,t-1) / YTD_total,t-1
  #
  # The section contributions sum to the overall YTD YoY growth rate because
  #   sum_s (YTD_s,t - YTD_s,t-1) = YTD_total,t - YTD_total,t-1,
  # so dividing by the common base YTD_total,t-1 gives
  #   sum_s contribution_s = 100 * (YTD_total,t - YTD_total,t-1) / YTD_total,t-1.
  # Verified numerically against an independent recomputation: the two agree to
  # floating-point tolerance (max abs difference ~3e-14 pp).
  by_sector <- monthly_sitc1 |>
    dplyr::arrange(sitc1, date) |>
    dplyr::group_by(year, sitc1, sitc1_label) |>
    dplyr::mutate(ytd_value_eur = cumsum(value_eur)) |>
    dplyr::ungroup()

  totals <- by_sector |>
    dplyr::group_by(year, month, date) |>
    dplyr::summarise(total_ytd_value_eur = sum(ytd_value_eur), .groups = "drop")

  by_sector |>
    dplyr::left_join(totals, by = c("year", "month", "date")) |>
    dplyr::group_by(sitc1, sitc1_label, month) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    dplyr::mutate(
      lag_ytd_value_eur = dplyr::lag(ytd_value_eur),
      lag_total_ytd_value_eur = dplyr::lag(total_ytd_value_eur),
      contribution_pp = dplyr::if_else(
        is.na(lag_total_ytd_value_eur) | lag_total_ytd_value_eur == 0,
        NA_real_,
        100 * (ytd_value_eur - lag_ytd_value_eur) / lag_total_ytd_value_eur
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(year, month, date) |>
    dplyr::mutate(
      total_ytd_growth_pct = dplyr::if_else(
        all(is.na(contribution_pp)),
        NA_real_,
        sum(contribution_pp, na.rm = TRUE)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(date, sitc1)
}

# Latest calendar year with a full 12 months of monthly observations. Because
# annual figures are now summed from monthly data, the current (in-progress)
# year is partial; "full-year" displays should use this instead of max(year).
latest_complete_year <- function(monthly_total) {
  monthly_total |>
    dplyr::count(year, name = "n_months") |>
    dplyr::filter(n_months >= 12) |>
    dplyr::summarise(y = max(year)) |>
    dplyr::pull(y)
}

latest_complete_month <- function(monthly_total) {
  monthly_total |>
    dplyr::arrange(date) |>
    dplyr::slice_tail(n = 1)
}

# Fetch 5-digit SITC detail for one partner and one selected period. Used on
# demand in the partner-led tabs so the deployed app can stay lightweight while
# still showing detailed 5-digit products for the chosen market and flow.
fetch_partner_sitc5_period <- function(partner, flow = "2", period_type = c("month", "year"), period_value) {
  period_type <- match.arg(period_type)

  if (period_type == "month") {
    date_value <- as.Date(period_value)
    start_period <- format(date_value, "%Y-%m")
    end_period <- start_period
  } else {
    year_value <- as.integer(period_value)
    start_period <- sprintf("%d-01", year_value)
    end_period <- sprintf("%d-12", year_value)
  }

  key <- paste0("M.IE.", partner, "..", flow, ".VALUE_EUR+QUANTITY_KG")
  url <- paste0(
    "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/2.1/data/DS-059331/",
    key,
    "?startPeriod=", start_period,
    "&endPeriod=", end_period,
    "&format=SDMX-CSV"
  )

  raw <- read_comext_csv(url)
  required_cols <- c("partner", "product", "indicators", "TIME_PERIOD", "OBS_VALUE")
  if (!all(required_cols %in% names(raw))) {
    stop("Eurostat did not return the expected SITC5 detail format.", call. = FALSE)
  }

  parsed <- raw |>
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
    tidyr::pivot_wider(
      names_from = indicator,
      values_from = value,
      values_fill = 0
    ) |>
    dplyr::rename(value_eur = VALUE_EUR, quantity_kg = QUANTITY_KG)

  detail <- parsed |>
    dplyr::filter(product == "TOTAL" | stringr::str_detect(product, "^[0-9]{5}$")) |>
    dplyr::mutate(
      sitc1 = dplyr::if_else(product == "TOTAL", NA_character_, stringr::str_sub(product, 1, 1)),
      product_sitc5 = product
    )

  if (period_type == "year") {
    detail |>
      dplyr::group_by(partner, year, sitc1, product_sitc5) |>
      dplyr::summarise(
        value_eur = sum(value_eur, na.rm = TRUE),
        quantity_kg = sum(quantity_kg, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(value_eur))
  } else {
    detail |>
      dplyr::group_by(partner, year, month, date, sitc1, product_sitc5) |>
      dplyr::summarise(
        value_eur = sum(value_eur, na.rm = TRUE),
        quantity_kg = sum(quantity_kg, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(value_eur))
  }
}

# Fetch partner-country detail for one 5-digit SITC product and one selected
# period. Used on demand in the product x country tab for either flow.
fetch_product_destinations_period <- function(product_sitc5, flow = "2", period_type = c("month", "year"), period_value) {
  period_type <- match.arg(period_type)

  if (period_type == "month") {
    date_value <- as.Date(period_value)
    start_period <- format(date_value, "%Y-%m")
    end_period <- start_period
  } else {
    year_value <- as.integer(period_value)
    start_period <- sprintf("%d-01", year_value)
    end_period <- sprintf("%d-12", year_value)
  }

  key <- paste0("M.IE..", product_sitc5, ".", flow, ".VALUE_EUR+QUANTITY_KG")
  url <- paste0(
    "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/2.1/data/DS-059331/",
    key,
    "?startPeriod=", start_period,
    "&endPeriod=", end_period,
    "&format=SDMX-CSV"
  )

  raw <- read_comext_csv(url)
  required_cols <- c("partner", "product", "indicators", "TIME_PERIOD", "OBS_VALUE")
  if (!all(required_cols %in% names(raw))) {
    stop("Eurostat did not return the expected product-destination format.", call. = FALSE)
  }

  parsed <- raw |>
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
    tidyr::pivot_wider(
      names_from = indicator,
      values_from = value,
      values_fill = 0
    ) |>
    dplyr::rename(value_eur = VALUE_EUR, quantity_kg = QUANTITY_KG)

  partner_aggregate_codes <- c(
    "WORLD",
    "INT_EU", "EXT_EU", "INT_EU27_2020", "EXT_EU27_2020", "INT_EU28", "EXT_EU28",
    "INT_EA", "EXT_EA", "INT_EA21", "EXT_EA21", "EU27_2020", "EA21"
  )

  detail <- parsed |>
    dplyr::filter(
      product == product_sitc5,
      !partner %in% partner_aggregate_codes,
      stringr::str_detect(partner, "^[A-Z]{2}$")
    )

  if (period_type == "year") {
    detail |>
      dplyr::group_by(partner, product) |>
      dplyr::summarise(
        value_eur = sum(value_eur, na.rm = TRUE),
        quantity_kg = sum(quantity_kg, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::rename(product_sitc5 = product) |>
      dplyr::arrange(dplyr::desc(value_eur))
  } else {
    detail |>
      dplyr::group_by(partner, date, year, month, product) |>
      dplyr::summarise(
        value_eur = sum(value_eur, na.rm = TRUE),
        quantity_kg = sum(quantity_kg, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::rename(product_sitc5 = product) |>
      dplyr::arrange(dplyr::desc(value_eur))
  }
}

# Fetch the full monthly history for one exact product-country pair. This keeps
# the product x country and country x product tabs focused on genuine pair-
# specific drill-down rather than showing another world-total product series.
fetch_product_country_monthly <- function(product_sitc5, partner_code, flow = "2") {
  start_period <- "2014-01"
  end_period <- format(Sys.Date(), "%Y-%m")

  key <- paste0("M.IE.", partner_code, ".", product_sitc5, ".", flow, ".VALUE_EUR+QUANTITY_KG")
  url <- paste0(
    "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/2.1/data/DS-059331/",
    key,
    "?startPeriod=", start_period,
    "&endPeriod=", end_period,
    "&format=SDMX-CSV"
  )

  raw <- read_comext_csv(url)
  required_cols <- c("partner", "product", "indicators", "TIME_PERIOD", "OBS_VALUE")
  if (!all(required_cols %in% names(raw))) {
    stop("Eurostat did not return the expected product-country format.", call. = FALSE)
  }

  raw |>
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
    tidyr::pivot_wider(
      names_from = indicator,
      values_from = value,
      values_fill = 0
    ) |>
    dplyr::rename(value_eur = VALUE_EUR, quantity_kg = QUANTITY_KG) |>
    dplyr::filter(product == product_sitc5, partner == .env$partner_code) |>
    dplyr::group_by(partner, product, date, year, month) |>
    dplyr::summarise(
      value_eur = sum(value_eur, na.rm = TRUE),
      quantity_kg = sum(quantity_kg, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(product_sitc5 = product) |>
    dplyr::arrange(date)
}

# ---------------------------------------------------------------------------
# Feature page: single-product export-growth decomposition
# ---------------------------------------------------------------------------
# The "Export surge" tab is built around one detailed product. The product
# code and a short, plain-English label are the only hard-coded specifics for
# that tab; everything else is computed from the data below.
FEATURE_PRODUCT_CODE  <- "54154"               # SITC Rev. 4, 5-digit
FEATURE_PRODUCT_LABEL <- "Polypeptide hormones" # short label for headings

# The narrative anchors on "since the start of 2024", so the anchor date is a
# named constant rather than a magic value scattered through the UI.
FEATURE_ANCHOR_DATE <- as.Date("2024-01-01")

# ISO-2 codes of the 27 EU member states, used only to split a product's
# destinations into EU vs non-EU on the feature page. (Eurostat reports
# partners with ISO-2 codes; aggregate codes are filtered out upstream.)
EU27_ISO2 <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR",
  "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK",
  "SI", "ES", "SE"
)

# Build a tidy monthly series for ONE product set against the matching total,
# decomposing year-on-year goods-trade growth into the product vs all other
# goods. Classification-agnostic: callers pass the product's monthly rows and
# the matching monthly total, both already filtered to a single flow. Designed
# for the polypeptide-hormones (SITC 54154) feature page but reusable for any
# product code.
#
# Inputs
#   product_monthly : data frame with columns date, value_eur (one or more
#                     rows per month; months the product did not trade may be
#                     absent and are treated as zero).
#   total_monthly   : data frame with columns date, value_eur (one contiguous
#                     monthly total series).
# Returns one row per month in total_monthly with the component levels, the
# product's share of the total, and both single-month and year-to-date (YTD)
# year-on-year growth contributions in percentage points (pp).
build_product_growth_series <- function(product_monthly, total_monthly) {
  product_monthly <- product_monthly |>
    dplyr::group_by(date) |>
    dplyr::summarise(product_eur = sum(value_eur, na.rm = TRUE), .groups = "drop")

  series <- total_monthly |>
    dplyr::distinct(date, .keep_all = TRUE) |>
    dplyr::transmute(date, total_eur = value_eur) |>
    dplyr::left_join(product_monthly, by = "date") |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      product_eur   = dplyr::coalesce(product_eur, 0),
      other_eur     = total_eur - product_eur,
      year          = lubridate::year(date),
      month         = lubridate::month(date),
      product_share = dplyr::if_else(total_eur == 0, NA_real_, product_eur / total_eur)
    )

  # --- Single-month year-on-year contributions ----------------------------
  # A component's contribution (pp) to total growth = 100 * (component change
  # vs 12 months earlier) / (total 12 months earlier). The product and
  # "all other goods" contributions sum exactly to the total growth rate,
  # because their euro changes sum to the total euro change over a common base.
  # Guarded against a missing or zero year-earlier base.
  series <- series |>
    dplyr::mutate(
      base_total_eur     = dplyr::lag(total_eur, 12),
      product_yoy_eur    = product_eur - dplyr::lag(product_eur, 12),
      other_yoy_eur      = other_eur   - dplyr::lag(other_eur, 12),
      total_yoy_eur      = total_eur   - dplyr::lag(total_eur, 12),
      product_contrib_pp = dplyr::if_else(is.na(base_total_eur) | base_total_eur == 0,
                                          NA_real_, 100 * product_yoy_eur / base_total_eur),
      other_contrib_pp   = dplyr::if_else(is.na(base_total_eur) | base_total_eur == 0,
                                          NA_real_, 100 * other_yoy_eur / base_total_eur),
      total_growth_pp    = dplyr::if_else(is.na(base_total_eur) | base_total_eur == 0,
                                          NA_real_, 100 * total_yoy_eur / base_total_eur)
    )

  # --- Year-to-date (cumulative Jan->month) year-on-year contributions -----
  # Product-level data is lumpy (single large shipments), so the YTD view
  # smooths the single-month series: cumulate Jan->current month within each
  # calendar year, then compare with the same cumulative point one year earlier
  # (matched on month-of-year). Same additive contribution identity as above.
  series |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      ytd_product_eur = cumsum(product_eur),
      ytd_total_eur   = cumsum(total_eur),
      ytd_other_eur   = cumsum(other_eur)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(month) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    dplyr::mutate(
      ytd_base_total_eur     = dplyr::lag(ytd_total_eur),
      ytd_product_contrib_pp = dplyr::if_else(is.na(ytd_base_total_eur) | ytd_base_total_eur == 0,
                                              NA_real_, 100 * (ytd_product_eur - dplyr::lag(ytd_product_eur)) / ytd_base_total_eur),
      ytd_other_contrib_pp   = dplyr::if_else(is.na(ytd_base_total_eur) | ytd_base_total_eur == 0,
                                              NA_real_, 100 * (ytd_other_eur - dplyr::lag(ytd_other_eur)) / ytd_base_total_eur),
      ytd_total_growth_pp    = dplyr::if_else(is.na(ytd_base_total_eur) | ytd_base_total_eur == 0,
                                              NA_real_, 100 * (ytd_total_eur - dplyr::lag(ytd_total_eur)) / ytd_base_total_eur)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(date)
}

# ---------------------------------------------------------------------------
# Product finder: browse the SITC hierarchy at any granularity (1-5 digit)
# ---------------------------------------------------------------------------
# The 5-digit codes nest cleanly into the hierarchy by truncation, so values at
# any level are computed by truncating + summing the bundled 5-digit data
# (level 1 uses the authoritative 1-digit series). Labels for every level come
# from data/sitc_labels.csv (read_sitc_labels); levels without a description
# fall back to the bare code.

# One row per item at the chosen level, ranked by value, with description and
# share of the level total. `detail_annual` is the flow+year-filtered 5-digit
# table; `sitc1_annual` the flow+year-filtered 1-digit table; `labels` the full
# hierarchy label table; `lvl` an integer 1-5.
build_sitc_level_choices <- function(detail_annual, sitc1_annual, labels, lvl) {
  lvl <- as.integer(lvl)

  if (lvl == 1L) {
    agg <- sitc1_annual |>
      dplyr::mutate(code = as.character(sitc1)) |>
      dplyr::filter(grepl("^[0-9]$", code)) |>
      dplyr::group_by(code) |>
      dplyr::summarise(value_eur = sum(value_eur, na.rm = TRUE), .groups = "drop")
  } else {
    agg <- detail_annual |>
      dplyr::mutate(code = substr(as.character(product_sitc5), 1, lvl)) |>
      dplyr::filter(grepl(paste0("^[0-9]{", lvl, "}$"), code)) |>
      dplyr::group_by(code) |>
      dplyr::summarise(value_eur = sum(value_eur, na.rm = TRUE), .groups = "drop")
  }

  lab <- labels |>
    dplyr::filter(level == lvl) |>
    dplyr::transmute(code = sitc_code, sitc_desc)

  total <- sum(agg$value_eur, na.rm = TRUE)

  agg |>
    dplyr::left_join(lab, by = "code") |>
    dplyr::mutate(
      share = if (total > 0) value_eur / total else NA_real_,
      label_display = dplyr::if_else(
        is.na(sitc_desc) | sitc_desc == "",
        code,
        paste0(code, " - ", sitc_desc)
      )
    ) |>
    dplyr::arrange(dplyr::desc(value_eur))
}

# Monthly value series for one item at any level, from bundled data: level 5
# from the 5-digit series, level 1 from the 1-digit series, levels 2-4 by
# truncating + summing the 5-digit series. Returns columns date, value_eur.
build_sitc_level_monthly <- function(monthly_detail, monthly_sitc1, code, lvl) {
  lvl <- as.integer(lvl)
  code <- as.character(code)

  if (lvl == 1L) {
    monthly_sitc1 |>
      dplyr::filter(as.character(sitc1) == code) |>
      dplyr::group_by(date) |>
      dplyr::summarise(value_eur = sum(value_eur, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(date)
  } else {
    monthly_detail |>
      dplyr::filter(substr(as.character(product_sitc5), 1, lvl) == code) |>
      dplyr::group_by(date) |>
      dplyr::summarise(value_eur = sum(value_eur, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(date)
  }
}
