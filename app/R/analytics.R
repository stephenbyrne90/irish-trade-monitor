# Analytics layer for the redesigned Irish Trade Monitor.
#
# Pure data functions: no Shiny, no plotly. Shiny auto-sources app/R/*.R before
# app.R runs its library() calls, so top-level code here only defines functions
# and plain constants (base R / tibble:: only); every call inside functions is
# namespace-qualified (except the native pipe).
#
# Conventions
#   * flow is character: "1" = Imports, "2" = Exports.
#   * date is the first day of the month. "Complete monthly grid" means every
#     month between a flow's first and last date is present (missing = 0).
#   * Percentages are in percent units (5 = 5%). Growth is NA where the window
#     is incomplete or the denominator is <= 0.

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Ten analytical product groups, in display order. `definition` documents the
# SITC Rev. 4 prefixes used by tm_assign_group().
TM_GROUPS <- tibble::tribble(
  ~group,                           ~short,        ~colour,    ~definition,
  "Pharmaceuticals",                "Pharma",      "#004d44",  "SITC 54",
  "Organic chemicals",              "Organics",    "#2e7d6f",  "SITC 51",
  "Other chemicals",                "Other chem.", "#8fc1b5",  "Rest of SITC section 5",
  "Computers & electronics",        "Computers",   "#2c55a2",  "SITC 75, 76, 77 except 774",
  "Medical devices & instruments",  "Med. devices","#8ba6d6",  "SITC 87, 8996, 774",
  "Aircraft",                       "Aircraft",    "#5e4a8c",  "SITC 792",
  "Other machinery & transport",    "Other mach.", "#6c7a99",  "Rest of SITC section 7",
  "Food, drink & tobacco",          "Food",        "#ab9965",  "SITC sections 0 and 1",
  "Fuels",                          "Fuels",       "#8c5a2b",  "SITC section 3",
  "Other goods",                    "Other",       "#c8cbd0",  "SITC sections 2, 4, 6, 9 and rest of 8"
)
TM_GROUP_LEVELS <- TM_GROUPS$group
TM_GROUP_COLOURS <- stats::setNames(TM_GROUPS$colour, TM_GROUPS$group)

# The three chemical groups (used for "ex chemicals & pharma" measures).
TM_CHEM_GROUPS <- TM_GROUPS$group[1:3]

# Partner groups. Euro-area members excluding IE. BG joined the euro in 2026 and
# is treated as euro area throughout (simple, stable groups over the sample).
# XI / XU (Northern Ireland / UK) are included in Great Britain & NI if present.
.TM_EURO_AREA_EX_IE <- c("AT", "BE", "BG", "CY", "DE", "EE", "ES", "FI", "FR",
                         "GR", "HR", "IT", "LT", "LU", "LV", "MT", "NL", "PT",
                         "SI", "SK")
# EU27 minus IE minus the euro-area members above.
.TM_OTHER_EU <- c("CZ", "DK", "HU", "PL", "RO", "SE")

TM_PARTNER_GROUP_LEVELS <- c("United States", "Great Britain & NI", "Euro area",
                             "Other EU", "Switzerland", "China", "Rest of world")
TM_PARTNER_GROUP_COLOURS <- stats::setNames(
  c("#2c55a2", "#ae1122", "#004d44", "#8fc1b5", "#ab9965", "#5e4a8c", "#c8cbd0"),
  TM_PARTNER_GROUP_LEVELS
)
# Code -> group lookup. "Rest of world" has no codes: it is the residual.
TM_PARTNER_GROUPS <- tibble::tibble(
  partner = c("US", "GB", "XI", "XU", .TM_EURO_AREA_EX_IE, .TM_OTHER_EU, "CH", "CN"),
  partner_group = c(
    "United States", rep("Great Britain & NI", 3),
    rep("Euro area", length(.TM_EURO_AREA_EX_IE)),
    rep("Other EU", length(.TM_OTHER_EU)),
    "Switzerland", "China"
  )
)

# Dated events used for chart annotation. EDIT THIS LIST to add, remove or
# reword events: every chart that calls tm_add_events() picks it up.
TM_EVENTS <- tibble::tibble(
  date = as.Date(c("2020-03-01", "2021-01-01", "2022-02-01", "2025-04-01", "2025-08-01")),
  label = c("Covid-19 pandemic",
            "Brexit: end of transition period",
            "Russia invades Ukraine",
            "US 'Liberation Day' tariff announcement",
            "EU-US trade framework agreed"),
  short = c("Covid", "Brexit", "Ukraine war", "US tariffs", "EU-US deal")
)

TM_FLOW_LABELS <- c("1" = "Imports", "2" = "Exports")

TM_SITC1_LABELS <- c(
  "0" = "Food & live animals",
  "1" = "Beverages & tobacco",
  "2" = "Crude materials",
  "3" = "Fuels",
  "4" = "Oils & fats",
  "5" = "Chemicals & pharma",
  "6" = "Manufactured goods",
  "7" = "Machinery & transport",
  "8" = "Misc. manufactures",
  "9" = "Other commodities"
)

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# ASCII-safe constants (source files stay ASCII so C-locale sessions work).
.TM_EURO <- intToUtf8(0x20AC)
.TM_ELLIPSIS <- intToUtf8(0x2026)

# Integer month index (year * 12 + month - 1) of a Date vector.
.tm_month_idx <- function(date) {
  lt <- as.POSIXlt(date, tz = "UTC")
  (lt$year + 1900L) * 12L + lt$mon
}

# First-of-month Date from a month index.
.tm_idx_date <- function(idx) {
  as.Date(sprintf("%04d-%02d-01", as.integer(idx %/% 12L), as.integer(idx %% 12L) + 1L))
}

# Lag a vector by k positions (NA fill); assumes the vector is a sorted series.
.tm_lag <- function(x, k) {
  n <- length(x)
  if (n <= k) return(rep(NA_real_, n))
  c(rep(NA, k), x[seq_len(n - k)])
}

# Trailing k-element sum via cumsum; NA until a full window of non-NA exists.
.tm_roll_sum <- function(x, k) {
  n <- length(x)
  if (n == 0) return(numeric(0))
  ok <- !is.na(x)
  cs <- cumsum(ifelse(ok, x, 0))
  cn <- cumsum(!ok)
  lag0 <- function(v) if (n > k) c(rep(0, k), v[seq_len(n - k)]) else rep(0, n)
  s <- cs - lag0(cs)
  bad <- cn - lag0(cn)
  s[bad > 0 | seq_len(n) < k] <- NA_real_
  s
}

# Percent change cur vs prev; NA if either is missing or prev <= 0.
.tm_pct <- function(cur, prev) {
  out <- 100 * (cur / prev - 1)
  out[is.na(cur) | is.na(prev) | prev <= 0] <- NA_real_
  out
}

# ---------------------------------------------------------------------------
# Grouping
# ---------------------------------------------------------------------------

#' Assign analytical group (TM_GROUPS) to 5-digit SITC codes.
#' Longest-prefix match; computed on unique codes then matched back, so it is
#' fast on 750k-row vectors. Unmatched non-NA codes fall into "Other goods".
#' @param sitc5 character vector of SITC codes.
#' @return character vector of group names (same length as `sitc5`).
tm_assign_group <- function(sitc5) {
  sitc5 <- as.character(sitc5)
  u <- unique(sitc5)
  rules <- c(
    "0" = "Food, drink & tobacco", "1" = "Food, drink & tobacco",
    "2" = "Other goods", "3" = "Fuels", "4" = "Other goods",
    "5" = "Other chemicals", "6" = "Other goods",
    "7" = "Other machinery & transport", "8" = "Other goods", "9" = "Other goods",
    "54" = "Pharmaceuticals", "51" = "Organic chemicals",
    "75" = "Computers & electronics", "76" = "Computers & electronics",
    "77" = "Computers & electronics", "87" = "Medical devices & instruments",
    "774" = "Medical devices & instruments", "792" = "Aircraft",
    "8996" = "Medical devices & instruments"
  )
  rules <- rules[order(nchar(names(rules)))]
  res <- rep(NA_character_, length(u))
  for (p in names(rules)) {
    hit <- !is.na(u) & startsWith(u, p)
    res[hit] <- rules[[p]]
  }
  res[!is.na(u) & is.na(res)] <- "Other goods"
  res[match(sitc5, u)]
}

#' Complete a monthly table to a full contiguous grid per `by` combination.
#' The grid spans each flow's min..max date (or the overall range if `flow` is
#' not in `by`); missing values are filled with 0 and year/month recomputed.
#' Duplicated rows are summed. Columns other than `by`, date, year, month,
#' flow_label and `value_cols` are dropped.
#' @param df data frame with a `date` column.
#' @param by character vector of grouping columns (include "flow" if present).
#' @param value_cols numeric columns to keep/fill.
#' @return tibble with by columns, date, year, month, value columns.
tm_complete_monthly <- function(df, by, value_cols = c("value_eur", "quantity_kg")) {
  value_cols <- intersect(value_cols, names(df))
  d <- tibble::as_tibble(df)
  d$.idx <- .tm_month_idx(d$date)
  agg <- d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, ".idx")))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(value_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  combos <- dplyr::distinct(agg[, by, drop = FALSE])
  if ("flow" %in% by) {
    rng <- agg |>
      dplyr::group_by(flow) |>
      dplyr::summarise(.lo = min(.idx), .hi = max(.idx), .groups = "drop")
    combos <- dplyr::left_join(combos, rng, by = "flow")
  } else {
    combos$.lo <- if (nrow(agg)) min(agg$.idx) else integer(0)
    combos$.hi <- if (nrow(agg)) max(agg$.idx) else integer(0)
  }
  n_i <- as.integer(combos$.hi - combos$.lo + 1L)
  ci <- rep(seq_len(nrow(combos)), n_i)
  grid <- combos[ci, by, drop = FALSE]
  grid$.idx <- sequence(n_i) - 1L + rep(as.integer(combos$.lo), n_i)
  out <- dplyr::left_join(grid, agg, by = c(by, ".idx"))
  for (v in value_cols) out[[v]][is.na(out[[v]])] <- 0
  all_idx <- sort(unique(out$.idx))
  out$date <- .tm_idx_date(all_idx)[match(out$.idx, all_idx)]
  out$year <- as.integer(out$.idx %/% 12L)
  out$month <- as.integer(out$.idx %% 12L) + 1L
  out$.idx <- NULL
  if ("flow" %in% by) {
    out$flow_label <- unname(TM_FLOW_LABELS[as.character(out$flow)])
    lead <- c(by, "flow_label")
  } else {
    lead <- by
  }
  out <- out[, c(lead, "date", "year", "month", value_cols), drop = FALSE]
  dplyr::arrange(out, dplyr::across(dplyr::all_of(c(by, "date"))))
}

#' Monthly values by the ten analytical product groups.
#' The gap between each SITC section total (monthly_sitc1) and the sum of its
#' 5-digit products (confidential trade) is added to the section's catch-all
#' group, so the result sums exactly to monthly_sitc1 per flow-month.
#' quantity_kg is the plain sum of available kg (no residual allocation).
#' monthly_sitc1 itself differs from monthly_total by up to ~EUR 1.5k per month
#' (source rounding); if `monthly_total` is supplied that tiny gap is also put in
#' "Other goods" so the groups sum exactly to the headline total.
#' @param monthly_sitc5 5-digit monthly table.
#' @param monthly_sitc1 section monthly table.
#' @param monthly_total optional monthly totals to reconcile to.
#' @return tibble(flow, flow_label, date, year, month, group [factor], value_eur, quantity_kg).
tm_group_monthly <- function(monthly_sitc5, monthly_sitc1, monthly_total = NULL) {
  catch_all <- c("0" = "Food, drink & tobacco", "1" = "Food, drink & tobacco",
                 "3" = "Fuels", "5" = "Other chemicals",
                 "7" = "Other machinery & transport")
  s5 <- tibble::tibble(
    flow = monthly_sitc5$flow,
    date = monthly_sitc5$date,
    sec = substr(monthly_sitc5$product_sitc5, 1, 1),
    group = tm_assign_group(monthly_sitc5$product_sitc5),
    value_eur = monthly_sitc5$value_eur,
    quantity_kg = monthly_sitc5$quantity_kg
  ) |>
    dplyr::group_by(flow, date, sec, group) |>
    dplyr::summarise(
      value_eur = sum(value_eur, na.rm = TRUE),
      quantity_kg = sum(quantity_kg, na.rm = TRUE),
      .groups = "drop"
    )
  sec_sum <- s5 |>
    dplyr::group_by(flow, date, sec) |>
    dplyr::summarise(s5_value = sum(value_eur), .groups = "drop")
  resid <- monthly_sitc1 |>
    dplyr::transmute(flow = as.character(flow), date, sec = as.character(sitc1), total = value_eur) |>
    dplyr::group_by(flow, date, sec) |>
    dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop") |>
    dplyr::full_join(sec_sum, by = c("flow", "date", "sec")) |>
    dplyr::mutate(
      value_eur = dplyr::coalesce(total, 0) - dplyr::coalesce(s5_value, 0),
      quantity_kg = 0,
      group = dplyr::if_else(is.na(catch_all[sec]), "Other goods", unname(catch_all[sec]))
    ) |>
    dplyr::select(flow, date, group, value_eur, quantity_kg)
  all_rows <- dplyr::bind_rows(dplyr::select(s5, flow, date, group, value_eur, quantity_kg), resid)
  if (!is.null(monthly_total)) {
    cur <- all_rows |>
      dplyr::group_by(flow, date) |>
      dplyr::summarise(cur = sum(value_eur), .groups = "drop")
    gap <- monthly_total |>
      dplyr::transmute(flow = as.character(flow), date, total = value_eur) |>
      dplyr::left_join(cur, by = c("flow", "date")) |>
      dplyr::transmute(flow, date, group = "Other goods",
                       value_eur = total - dplyr::coalesce(cur, 0), quantity_kg = 0)
    all_rows <- dplyr::bind_rows(all_rows, gap)
  }
  out <- tm_complete_monthly(all_rows, by = c("flow", "group"))
  out$group <- factor(out$group, levels = TM_GROUP_LEVELS)
  dplyr::arrange(out, flow, group, date)
}

#' Monthly values by partner group (United States, Great Britain & NI, Euro
#' area, Other EU, Switzerland, China, Rest of world).
#' "Rest of world" is the residual so the groups sum exactly to monthly_total.
#' @param monthly_partner_total flow x partner x month table.
#' @param monthly_total flow x month totals.
#' @return tibble(flow, flow_label, date, year, month, partner_group [factor], value_eur).
tm_partner_group_monthly <- function(monthly_partner_total, monthly_total) {
  named <- tibble::tibble(
    flow = as.character(monthly_partner_total$flow),
    date = monthly_partner_total$date,
    partner_group = TM_PARTNER_GROUPS$partner_group[match(monthly_partner_total$partner, TM_PARTNER_GROUPS$partner)],
    value_eur = monthly_partner_total$value_eur
  )
  named <- named[!is.na(named$partner_group), ]
  named <- named |>
    dplyr::group_by(flow, date, partner_group) |>
    dplyr::summarise(value_eur = sum(value_eur, na.rm = TRUE), .groups = "drop")
  named_sum <- named |>
    dplyr::group_by(flow, date) |>
    dplyr::summarise(named = sum(value_eur), .groups = "drop")
  rest <- monthly_total |>
    dplyr::transmute(flow = as.character(flow), date, total = value_eur) |>
    dplyr::left_join(named_sum, by = c("flow", "date")) |>
    dplyr::transmute(flow, date, partner_group = "Rest of world",
                     value_eur = total - dplyr::coalesce(named, 0))
  out <- tm_complete_monthly(dplyr::bind_rows(named, rest), by = c("flow", "partner_group"),
                             value_cols = "value_eur")
  out$partner_group <- factor(out$partner_group, levels = TM_PARTNER_GROUP_LEVELS)
  dplyr::arrange(out, flow, partner_group, date)
}

# ---------------------------------------------------------------------------
# Transforms
# ---------------------------------------------------------------------------

#' Add rolling sums, moving averages and growth rates to a complete monthly grid.
#' Adds: ma3, sum3, sum12, ma12, yoy_pct, yoy3_pct, yoy12_pct, mom3_pct (sum3 vs
#' the preceding non-overlapping sum3), ytd (calendar-year cumulative) and
#' ytd_yoy_pct. Rows are re-sorted by `by` then date. Percent units.
#' @param df complete monthly grid per `by`.
#' @param by grouping columns (character(0) for a single series).
#' @param value name of the value column.
#' @return df with the extra columns.
tm_add_transforms <- function(df, by, value = "value_eur") {
  df <- tibble::as_tibble(df) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(by, "date"))))
  df$.yr <- as.integer(format(df$date, "%Y"))
  df$.v <- df[[value]]
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::mutate(
      sum3 = .tm_roll_sum(.v, 3),
      sum12 = .tm_roll_sum(.v, 12),
      ma3 = sum3 / 3,
      ma12 = sum12 / 12,
      yoy_pct = .tm_pct(.v, .tm_lag(.v, 12)),
      yoy3_pct = .tm_pct(sum3, .tm_lag(sum3, 12)),
      yoy12_pct = .tm_pct(sum12, .tm_lag(sum12, 12)),
      mom3_pct = .tm_pct(sum3, .tm_lag(sum3, 3))
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, ".yr")))) |>
    dplyr::mutate(ytd = cumsum(.v)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::mutate(ytd_yoy_pct = .tm_pct(ytd, .tm_lag(ytd, 12))) |>
    dplyr::ungroup() |>
    dplyr::select(-.v, -.yr)
}

#' Additive contributions to year-on-year growth by component.
#' `df` must be a complete monthly grid by c(by, component). The windowed
#' level is the single month ("m"), rolling 3-month ("3m"), rolling 12-month
#' ("12m") or calendar year-to-date ("ytd") value. contrib_pp = 100 * delta /
#' (total of the windowed value a year earlier), so contributions sum to
#' total_growth_pct within each by-date. Rows with an incomplete window have NA.
#' @param df data frame.
#' @param component column naming the components (e.g. "group").
#' @param basis one of "3m", "m", "12m", "ytd".
#' @param by grouping columns, default "flow".
#' @param value value column.
#' @return tibble(<by>, date, <component>, level, delta, contrib_pp, total_growth_pct).
tm_contributions <- function(df, component, basis = c("3m", "m", "12m", "ytd"),
                             by = "flow", value = "value_eur") {
  basis <- match.arg(basis)
  d <- tibble::as_tibble(df) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(by, component, "date"))))
  d$.v <- d[[value]]
  d$.yr <- as.integer(format(d$date, "%Y"))
  gkeys <- c(by, component)
  d <- switch(basis,
    m = dplyr::mutate(dplyr::group_by(d, dplyr::across(dplyr::all_of(gkeys))), level = .v),
    `3m` = dplyr::mutate(dplyr::group_by(d, dplyr::across(dplyr::all_of(gkeys))), level = .tm_roll_sum(.v, 3)),
    `12m` = dplyr::mutate(dplyr::group_by(d, dplyr::across(dplyr::all_of(gkeys))), level = .tm_roll_sum(.v, 12)),
    ytd = dplyr::mutate(dplyr::group_by(d, dplyr::across(dplyr::all_of(c(gkeys, ".yr")))), level = cumsum(.v))
  )
  d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(gkeys))) |>
    dplyr::mutate(prev = .tm_lag(level, 12), delta = level - prev) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "date")))) |>
    dplyr::mutate(
      .base = if (anyNA(prev)) NA_real_ else sum(prev),
      .base = dplyr::if_else(!is.na(.base) & .base > 0, .base, NA_real_),
      contrib_pp = 100 * delta / .base,
      total_growth_pct = if (anyNA(delta)) NA_real_ else 100 * sum(delta) / .base
    ) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(c(by, "date", component)), level, delta, contrib_pp, total_growth_pct)
}

#' Trade balance table.
#' @param df data frame with `flow` ("1"/"2"), value_eur and the `by` columns.
#' @param by grouping columns (default "date").
#' @return tibble(<by>, exports, imports, balance).
tm_balance <- function(df, by = "date") {
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(
      exports = sum(value_eur[as.character(flow) == "2"], na.rm = TRUE),
      imports = sum(value_eur[as.character(flow) == "1"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(balance = exports - imports)
}

#' Concentration statistics of `item` within each `by` combination.
#' Non-positive values are ignored. Shares are in percent.
#' @param df data frame.
#' @param item column naming the items (e.g. product or partner).
#' @param by grouping columns.
#' @param value value column.
#' @return tibble(<by>, total, hhi [0-10000], top1_share, top3_share, top5_share,
#'   top10_share, n_effective [= 10000 / hhi]).
tm_concentration <- function(df, item, by, value = "value_eur") {
  d <- tibble::as_tibble(df)
  d$.v <- d[[value]]
  d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, item)))) |>
    dplyr::summarise(.v = sum(.v, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(.v > 0) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(by)), dplyr::desc(.v)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::mutate(.share = 100 * .v / sum(.v), .rank = dplyr::row_number()) |>
    dplyr::summarise(
      total = sum(.v),
      hhi = sum(.share^2),
      top1_share = sum(.share[.rank <= 1]),
      top3_share = sum(.share[.rank <= 3]),
      top5_share = sum(.share[.rank <= 5]),
      top10_share = sum(.share[.rank <= 10]),
      .groups = "drop"
    ) |>
    dplyr::mutate(n_effective = 10000 / hhi)
}

#' Rolling `window`-month sums per item, fast enough for the 5-digit table.
#' Builds a dense series x month matrix (months span the overall data range,
#' missing = 0), cumulates along months and differences, so the cost is linear.
#' If `dates` is given only those end dates are returned. End dates whose
#' window is incomplete (before the first `window` months) are dropped.
#' @param df data frame with date, `by`, `item`, `value`.
#' @param item item column.
#' @param by grouping columns (default "flow").
#' @param window months in the rolling window.
#' @param value value column.
#' @param dates optional Date vector of end dates.
#' @return tibble(<by>, <item>, date, value).
tm_rolling_item_sums <- function(df, item, by = "flow", window = 12, value = "value_eur", dates = NULL) {
  keys <- c(by, item)
  d <- tibble::as_tibble(df[, unique(c(keys, "date", value))])
  idx <- .tm_month_idx(d$date)
  lo <- min(idx)
  nT <- max(idx) - lo + 1L
  d <- dplyr::group_by(d, dplyr::across(dplyr::all_of(keys)))
  sid <- dplyr::group_indices(d)
  dplyr::ungroup(d) -> d
  first <- !duplicated(sid)
  S <- max(sid)
  combos <- d[first, keys, drop = FALSE][order(sid[first]), , drop = FALSE]
  lin <- sid + (idx - lo) * S
  v <- d[[value]]
  v[is.na(v)] <- 0
  rs <- rowsum(v, lin)
  vec <- numeric(S * nT)
  vec[as.integer(rownames(rs))] <- rs[, 1]
  C <- matrix(vec, nrow = S, ncol = nT)
  for (j in seq_len(nT)[-1]) C[, j] <- C[, j - 1] + C[, j]
  js <- seq_len(nT)
  js <- js[js >= window]
  if (!is.null(dates)) {
    want <- .tm_month_idx(as.Date(dates)) - lo + 1L
    js <- intersect(js, want)
  }
  if (length(js) == 0) {
    out <- combos[0, , drop = FALSE]
    out$date <- as.Date(character()); out$value <- numeric()
    return(out)
  }
  R <- C[, js, drop = FALSE]
  prev_cols <- js - window
  has_prev <- prev_cols >= 1
  if (any(has_prev)) R[, has_prev] <- R[, has_prev, drop = FALSE] - C[, prev_cols[has_prev], drop = FALSE]
  out <- combos[rep(seq_len(S), times = length(js)), , drop = FALSE]
  out$date <- rep(.tm_idx_date(js + lo - 1L), each = S)
  out$value <- as.vector(R)
  tibble::as_tibble(out)
}

#' Rolling value, volume and unit values with base-year indices.
#' Rebased indices set the base_year average of the rolled series to 100
#' (NA if the base is 0/missing).
#' @param df complete monthly grid per `by` with value_eur and quantity_kg.
#' @param by grouping columns.
#' @param window rolling window in months.
#' @param base_year year whose average equals 100.
#' @return tibble(<by>, date, value, kg, unit_value, value_index, kg_index, uv_index).
tm_unit_values <- function(df, by, window = 12, base_year = 2019) {
  rebase <- function(x, yr) {
    b <- x[yr == base_year & !is.na(x)]
    b <- if (length(b)) mean(b) else NA_real_
    if (is.na(b) || b <= 0) rep(NA_real_, length(x)) else 100 * x / b
  }
  d <- tibble::as_tibble(df) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(by, "date"))))
  d$.yr <- as.integer(format(d$date, "%Y"))
  d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::mutate(
      value = .tm_roll_sum(value_eur, window),
      kg = .tm_roll_sum(quantity_kg, window),
      unit_value = dplyr::if_else(!is.na(kg) & kg > 0, value / kg, NA_real_),
      value_index = rebase(value, .yr),
      kg_index = rebase(kg, .yr),
      uv_index = rebase(unit_value, .yr)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(by), date, value, kg, unit_value, value_index, kg_index, uv_index)
}

#' Latest date present for BOTH flows.
#' @param monthly_total monthly totals with flow and date.
#' @return Date.
tm_latest_date <- function(monthly_total) {
  mx <- tapply(monthly_total$date, monthly_total$flow, max)
  as.Date(min(mx), origin = "1970-01-01")
}

#' Aggregate the 5-digit monthly table to an n-digit SITC prefix (1-5).
#' @param monthly_sitc5 5-digit monthly table.
#' @param digits prefix length.
#' @return tibble(flow, date, code, value_eur, quantity_kg).
tm_sitc_level_monthly <- function(monthly_sitc5, digits) {
  tibble::tibble(
    flow = monthly_sitc5$flow,
    date = monthly_sitc5$date,
    code = substr(monthly_sitc5$product_sitc5, 1, digits),
    value_eur = monthly_sitc5$value_eur,
    quantity_kg = monthly_sitc5$quantity_kg
  ) |>
    dplyr::group_by(flow, date, code) |>
    dplyr::summarise(
      value_eur = sum(value_eur, na.rm = TRUE),
      quantity_kg = sum(quantity_kg, na.rm = TRUE),
      .groups = "drop"
    )
}

#' "code - description" display labels for SITC codes (truncated, with ellipsis).
#' Falls back to the bare code when there is no description.
#' @param codes character vector of SITC codes.
#' @param sitc_labels table with sitc_code, sitc_desc.
#' @param max_chars maximum characters of the whole label.
#' @return character vector.
tm_sitc_label <- function(codes, sitc_labels, max_chars = 60) {
  codes <- as.character(codes)
  desc <- sitc_labels$sitc_desc[match(codes, sitc_labels$sitc_code)]
  lab <- enc2utf8(ifelse(is.na(desc) | !nzchar(desc), codes, paste0(codes, " - ", desc)))
  long <- !is.na(lab) & nchar(lab, type = "chars") > max_chars
  lab[long] <- enc2utf8(paste0(substr(lab[long], 1, max_chars - 1), .TM_ELLIPSIS))
  lab
}

#' Plain country names (trailing parenthetical removed); falls back to the code.
#' @param codes character vector of partner codes.
#' @param partner_labels table with partner, partner_label.
#' @return character vector.
tm_partner_label <- function(codes, partner_labels) {
  codes <- as.character(codes)
  lab <- partner_labels$partner_label[match(codes, partner_labels$partner)]
  lab <- trimws(gsub("\\s*\\(.*\\)\\s*$", "", lab))
  ifelse(is.na(lab) | !nzchar(lab), codes, lab)
}

# ---------------------------------------------------------------------------
# Formatters
# ---------------------------------------------------------------------------

# Shared: format |x| with fixed digits and thousands separators.
.tm_num <- function(x, digits) formatC(as.numeric(x), format = "f", digits = digits, big.mark = ",")

#' EUR billions: "EUR12.3bn" style (euro sign), negatives "-EUR1.2bn", NA "n/a".
#' @param x numeric in EUR. @param digits decimals.
#' @return character.
tm_fmt_bn <- function(x, digits = 1) {
  r <- round(x / 1e9, digits)
  out <- paste0(ifelse(!is.na(r) & r < 0, "-", ""), .TM_EURO, .tm_num(abs(r), digits), "bn")
  out[is.na(x)] <- "n/a"
  out
}

#' EUR value auto-scaled to bn / m / k / units.
#' @param x numeric in EUR.
#' @return character.
tm_fmt_eur <- function(x) {
  a <- abs(x)
  out <- ifelse(is.na(x), "n/a",
    ifelse(a >= 1e9, tm_fmt_bn(x, 1),
      ifelse(a >= 1e6, paste0(ifelse(x < 0, "-", ""), .TM_EURO, .tm_num(a / 1e6, 1), "m"),
        paste0(ifelse(x < 0, "-", ""), .TM_EURO, .tm_num(a, 0)))))
  unname(out)
}

# Signed number with suffix; zero (after rounding) has no sign.
.tm_signed <- function(x, digits, suffix) {
  r <- round(x, digits)
  sg <- ifelse(is.na(r), "", ifelse(r > 0, "+", ifelse(r < 0, "-", "")))
  out <- paste0(sg, .tm_num(abs(r), digits), suffix)
  out[is.na(x)] <- "n/a"
  out
}

#' Percentage with explicit sign: "+3.4%", "-2.0%".
#' @param x numeric percent. @param digits decimals.
#' @return character.
tm_fmt_pct <- function(x, digits = 1) .tm_signed(x, digits, "%")

#' Percentage-point change with explicit sign: "+1.2pp".
#' @param x numeric. @param digits decimals.
#' @return character.
tm_fmt_pp <- function(x, digits = 1) .tm_signed(x, digits, "pp")

#' Share already in percent: "43.1%".
#' @param x numeric percent. @param digits decimals.
#' @return character.
tm_fmt_share <- function(x, digits = 1) {
  out <- paste0(.tm_num(x, digits), "%")
  out[is.na(x)] <- "n/a"
  out
}

#' Month label "Aug 2026" (locale independent).
#' @param date Date vector.
#' @return character.
tm_fmt_month <- function(date) {
  date <- as.Date(date)
  m <- as.integer(format(date, "%m"))
  out <- paste(month.abb[m], format(date, "%Y"))
  out[is.na(date)] <- "n/a"
  out
}

#' Long month label "August 2026" (locale independent).
#' @param date Date vector.
#' @return character.
tm_fmt_month_long <- function(date) {
  date <- as.Date(date)
  m <- as.integer(format(date, "%m"))
  out <- paste(month.name[m], format(date, "%Y"))
  out[is.na(date)] <- "n/a"
  out
}

# ---------------------------------------------------------------------------
# Bundle
# ---------------------------------------------------------------------------

#' Build the analytics bundle (`td`) from read_trade_data() output.
#' Elements (besides the raw tables passed through): monthly_sitc1 (+ section,
#' section_short), group_monthly, partner_group_monthly, total_tf, group_tf,
#' sitc1_tf, partner_group_tf, underlying, balance, latest_date, latest_label,
#' complete_year, events.
#' @param trade_data list returned by read_trade_data().
#' @return named list.
tm_build_bundle <- function(trade_data) {
  td <- trade_data
  mt <- trade_data$monthly_total
  s1 <- trade_data$monthly_sitc1
  s1$section_short <- unname(TM_SITC1_LABELS[as.character(s1$sitc1)])
  s1$section <- paste0(s1$sitc1, " - ", s1$section_short)
  td$monthly_sitc1 <- s1

  gm <- tm_group_monthly(trade_data$monthly_sitc5, trade_data$monthly_sitc1, mt)
  pgm <- tm_partner_group_monthly(trade_data$monthly_partner_total, mt)
  total_c <- tm_complete_monthly(mt, by = "flow")

  td$group_monthly <- gm
  td$partner_group_monthly <- pgm
  td$total_tf <- tm_add_transforms(total_c, by = "flow")
  td$group_tf <- tm_add_transforms(gm, by = c("flow", "group"))
  sitc1_c <- tm_complete_monthly(s1, by = c("flow", "sitc1"))
  sitc1_c$section_short <- unname(TM_SITC1_LABELS[as.character(sitc1_c$sitc1)])
  sitc1_c$section <- paste0(sitc1_c$sitc1, " - ", sitc1_c$section_short)
  td$sitc1_tf <- tm_add_transforms(sitc1_c, by = c("flow", "sitc1"))
  td$partner_group_tf <- tm_add_transforms(pgm, by = c("flow", "partner_group"))

  gw <- gm |>
    dplyr::group_by(flow, date) |>
    dplyr::summarise(
      pharma = sum(value_eur[group == "Pharmaceuticals"]),
      chem = sum(value_eur[group %in% TM_CHEM_GROUPS]),
      aircraft = sum(value_eur[group == "Aircraft"]),
      .groups = "drop"
    )
  und <- total_c |>
    dplyr::select(flow, date, total = value_eur) |>
    dplyr::left_join(gw, by = c("flow", "date")) |>
    dplyr::transmute(flow, date, total,
                     ex_pharma = total - pharma,
                     ex_chem = total - chem,
                     ex_aircraft = total - aircraft)
  measures <- c("total", "ex_pharma", "ex_chem", "ex_aircraft")
  measure_labels <- c(total = "Total", ex_pharma = "Excluding pharmaceuticals",
                      ex_chem = "Excluding chemicals & pharma", ex_aircraft = "Excluding aircraft")
  und_long <- tidyr::pivot_longer(und, dplyr::all_of(measures), names_to = "measure", values_to = "value_eur")
  und_long$measure <- factor(und_long$measure, levels = measures)
  und_long$measure_label <- factor(unname(measure_labels[as.character(und_long$measure)]),
                                   levels = unname(measure_labels))
  und_long$flow_label <- unname(TM_FLOW_LABELS[as.character(und_long$flow)])
  und_long$year <- as.integer(format(und_long$date, "%Y"))
  und_long$month <- as.integer(format(und_long$date, "%m"))
  td$underlying <- tm_add_transforms(und_long, by = c("flow", "measure"))

  bal <- tm_balance(total_c, by = "date")
  bal$value_eur <- bal$balance
  td$balance <- tm_add_transforms(bal, by = character(0)) |>
    dplyr::select(-value_eur)

  td$latest_date <- tm_latest_date(mt)
  td$latest_label <- tm_fmt_month_long(td$latest_date)
  cnt <- mt |>
    dplyr::group_by(flow, year) |>
    dplyr::summarise(n = dplyr::n_distinct(month), .groups = "drop") |>
    dplyr::group_by(year) |>
    dplyr::summarise(ok = sum(n == 12L) == dplyr::n_distinct(mt$flow), .groups = "drop")
  td$complete_year <- max(cnt$year[cnt$ok])
  td$events <- TM_EVENTS
  td
}

# Colours for the ten SITC 1-digit sections, keyed by the short section names
# in TM_SITC1_LABELS. Same hues as the Explorer pages' section palette so a
# section keeps one colour across the whole app.
TM_SITC1_COLOURS <- stats::setNames(
  c("#ab9965", "#6b5b2e", "#8c5a2b", "#a3402b", "#d9cca3",
  "#004d44", "#8fc1b5", "#2c55a2", "#8ba6d6", "#c8cbd0"),
  unname(TM_SITC1_LABELS)
)
