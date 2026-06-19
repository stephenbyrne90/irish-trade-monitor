# Pre-compute "drivers of a move" decompositions for the Monthly monitoring
# page, so the hosted app serves the common cases instantly and does not depend
# on live Eurostat calls for the default view.
#
# For the latest available month and both comparison bases (year-on-year and
# month-on-month), this ranks the biggest country movers and product movers
# (from the bundled monthly series) and, for each, fetches the detailed
# cross-section needed to split its change into components:
#   - country movers  -> the products that drove the country's change
#   - product movers  -> the countries that drove the product's change
# Results are written to app/data/drivers_country.parquet and
# app/data/drivers_product.parquet. The app reads these and falls back to a
# live fetch for any selection not covered here (older months, smaller movers).
#
# Run after scripts/update_trade_data.R (the refresh calls it automatically).
# It reuses the app's tested fetch helpers in app/R/trade_utils.R.

suppressWarnings(suppressMessages({
  library(dplyr)
  library(arrow)
  library(lubridate)
}))

# Locate the project root (this file lives in scripts/).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
this_file <- if (length(file_arg)) normalizePath(sub("^--file=", "", file_arg[[1]])) else "scripts/precompute_drivers.R"
project_root <- normalizePath(file.path(dirname(this_file), ".."))
app_dir <- file.path(project_root, "app")
data_dir <- file.path(app_dir, "data")

source(file.path(app_dir, "R", "trade_utils.R"))

# ---- Tuning -----------------------------------------------------------------
N_FOCUS <- 15L      # how many top movers (per kind, per flow) to pre-compute
N_COMPONENTS <- 15L # components kept per move (the panel shows the top 12)
FLOWS <- c("2", "1") # exports first, then imports

td <- read_trade_data(app_dir)
monthly_total <- td$monthly_total
monthly_partner_total <- td$monthly_partner_total
monthly_sitc5 <- td$monthly_sitc5
sitc5_labels <- td$sitc5_labels

partner_display_lookup <- td$partner_labels |>
  transmute(
    partner = as.character(partner),
    partner_display = if_else(
      is.na(partner_label) | partner_label == "", partner,
      paste0(partner, " - ", stringr::str_squish(stringr::str_replace(partner_label, "\\s*\\(.*\\)$", "")))
    )
  )

latest_month <- max(monthly_total$date, na.rm = TRUE)
comp_dates <- list(
  yoy = latest_month %m-% months(12),
  mom = latest_month %m-% months(1)
)
message("Pre-computing drivers for ", format(latest_month, "%B %Y"),
        " (YoY vs ", format(comp_dates$yoy, "%b %Y"), ", MoM vs ", format(comp_dates$mom, "%b %Y"), ").")

# Biggest country movers for one flow in the latest month, by |YoY change|.
top_country_movers <- function(flow_code) {
  cur <- monthly_partner_total |> filter(flow == flow_code, date == latest_month) |>
    transmute(partner = as.character(partner), cur = value_eur)
  prev <- monthly_partner_total |> filter(flow == flow_code, date == comp_dates$yoy) |>
    transmute(partner = as.character(partner), prev = value_eur)
  cur |> full_join(prev, by = "partner") |>
    mutate(across(c(cur, prev), ~ coalesce(.x, 0)), delta = cur - prev) |>
    slice_max(abs(delta), n = N_FOCUS, with_ties = FALSE) |>
    pull(partner)
}

# Biggest product movers for one flow in the latest month, by |YoY change|.
top_product_movers <- function(flow_code) {
  if (nrow(monthly_sitc5) == 0) return(character(0))
  cur <- monthly_sitc5 |> filter(flow == flow_code, date == latest_month) |>
    transmute(product_sitc5 = as.character(product_sitc5), cur = value_eur)
  prev <- monthly_sitc5 |> filter(flow == flow_code, date == comp_dates$yoy) |>
    transmute(product_sitc5 = as.character(product_sitc5), prev = value_eur)
  cur |> full_join(prev, by = "product_sitc5") |>
    mutate(across(c(cur, prev), ~ coalesce(.x, 0)), delta = cur - prev) |>
    slice_max(abs(delta), n = N_FOCUS, with_ties = FALSE) |>
    pull(product_sitc5)
}

# Safe fetch wrapper: returns a data frame with code + value, or NULL on error.
safe_country_detail <- function(partner, flow, month) {
  tryCatch(
    fetch_partner_sitc5_period(partner, flow, "month", as.character(month)) |>
      prepare_sitc5(sitc5_labels) |>
      filter(product_sitc5 != "TOTAL") |>
      transmute(component_code = as.character(product_sitc5), component_label = product_label, val = value_eur),
    error = function(e) { message("  ! country ", partner, " ", month, ": ", conditionMessage(e)); NULL }
  )
}
safe_product_detail <- function(product, flow, month) {
  tryCatch(
    fetch_product_destinations_period(product, flow, "month", as.character(month)) |>
      left_join(partner_display_lookup, by = "partner") |>
      transmute(component_code = as.character(partner),
                component_label = coalesce(partner_display, partner), val = value_eur),
    error = function(e) { message("  ! product ", product, " ", month, ": ", conditionMessage(e)); NULL }
  )
}

# Build the delta table for one focus entity, both bases. detail_fn(code, flow, month).
build_one <- function(kind, code, flow, detail_fn) {
  cur <- detail_fn(code, flow, latest_month)
  if (is.null(cur) || nrow(cur) == 0) return(NULL)
  out <- list()
  for (b in names(comp_dates)) {
    prev <- detail_fn(code, flow, comp_dates[[b]])
    prev_tbl <- if (is.null(prev)) tibble(component_code = character(), prev = numeric())
                else prev |> transmute(component_code, prev = val)
    merged <- cur |> transmute(component_code, component_label, cur = val) |>
      full_join(prev_tbl, by = "component_code") |>
      mutate(across(c(cur, prev), ~ coalesce(.x, 0)), delta = cur - prev)
    # Total in the comparison period (all components) is the base for the
    # percentage-point contributions, which then sum to the entity's % change.
    base_prev <- sum(merged$prev, na.rm = TRUE)
    merged <- merged |>
      filter(delta != 0) |>
      mutate(contrib_pp = if (base_prev > 0) 100 * delta / base_prev else NA_real_) |>
      slice_max(abs(delta), n = N_COMPONENTS, with_ties = FALSE)
    if (nrow(merged) == 0) next
    out[[b]] <- merged |>
      transmute(
        flow = flow, focus_code = code, basis = b,
        focus_month = latest_month, comp_month = comp_dates[[b]],
        component_code,
        component_label = coalesce(component_label, component_code),
        cur_eur = cur, prev_eur = prev, delta_eur = delta, contrib_pp = contrib_pp
      )
  }
  if (length(out) == 0) NULL else bind_rows(out)
}

country_rows <- list()
product_rows <- list()
for (flow in FLOWS) {
  message("Flow ", flow, " - countries...")
  for (p in top_country_movers(flow)) {
    country_rows[[length(country_rows) + 1]] <- build_one("country", p, flow, safe_country_detail)
  }
  message("Flow ", flow, " - products...")
  for (pr in top_product_movers(flow)) {
    product_rows[[length(product_rows) + 1]] <- build_one("product", pr, flow, safe_product_detail)
  }
}

drivers_country <- bind_rows(country_rows)
drivers_product <- bind_rows(product_rows)

if (nrow(drivers_country) > 0) {
  arrow::write_parquet(drivers_country, file.path(data_dir, "drivers_country.parquet"))
  message("Wrote drivers_country.parquet (", nrow(drivers_country), " rows).")
} else {
  message("No country drivers computed; leaving the app to fetch live.")
}
if (nrow(drivers_product) > 0) {
  arrow::write_parquet(drivers_product, file.path(data_dir, "drivers_product.parquet"))
  message("Wrote drivers_product.parquet (", nrow(drivers_product), " rows).")
} else {
  message("No product drivers computed; leaving the app to fetch live.")
}
