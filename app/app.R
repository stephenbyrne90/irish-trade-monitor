library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(arrow)
library(tidyr)
library(readr)

source(file.path("R", "trade_utils.R"))
# Analytical layer and page modules for the Briefing / Trends / Concentration /
# Markets pages. Shiny auto-loads R/ when the app is run with
# runApp(); sourcing explicitly as well keeps `source("app.R")` working.
for (f in c("analytics.R", "charts.R", "mod_briefing.R", "mod_trends.R",
            "mod_structure.R", "mod_markets.R")) {
  source(file.path("R", f))
}

trade_data <- read_trade_data(".")
sitc5_labels <- trade_data$sitc5_labels
partner_labels <- trade_data$partner_labels

monthly_total_all <- trade_data$monthly_total
monthly_sitc1_raw_all <- trade_data$monthly_sitc1
monthly_sitc1_all <- prepare_sitc1(monthly_sitc1_raw_all)
monthly_sitc5_all <- prepare_sitc5(trade_data$monthly_sitc5, sitc5_labels)
monthly_product_all <- prepare_sitc5(
  bind_rows(
    trade_data$monthly_total |>
      transmute(
        flow, flow_label, year, month, date,
        sitc1 = NA_character_,
        product_sitc5 = "TOTAL",
        value_eur, quantity_kg
      ),
    trade_data$monthly_sitc5
  ),
  sitc5_labels
)
monthly_partner_total_all <- trade_data$monthly_partner_total
monthly_partner_sitc1_all <- prepare_sitc1(trade_data$monthly_partner_sitc1)
annual_total_all <- trade_data$annual_total
annual_sitc1_all <- prepare_sitc1(trade_data$annual_sitc1)
annual_sitc5_all <- prepare_sitc5(trade_data$annual_sitc5, sitc5_labels)
annual_product_all <- prepare_sitc5(
  bind_rows(
    trade_data$annual_total |>
      transmute(
        flow, flow_label, year,
        sitc1 = NA_character_,
        product_sitc5 = "TOTAL",
        value_eur, quantity_kg
      ),
    trade_data$annual_sitc5
  ),
  sitc5_labels
)
annual_partner_total_all <- trade_data$annual_partner_total
annual_partner_sitc1_all <- prepare_sitc1(trade_data$annual_partner_sitc1)
# Full SITC hierarchy labels (levels 1-5) for the in-app product finder.
sitc_labels_all <- trade_data$sitc_labels
# Optional pre-computed move decompositions (instant; live fetch is the fallback).
drivers_country_all <- trade_data$drivers_country
drivers_product_all <- trade_data$drivers_product

# One pre-built bundle of analytical series (groups, partner groups, smoothed
# and growth transforms) shared by the analytical pages. Built once at startup.
tm_data <- tm_build_bundle(trade_data)

# Whether the detailed 5-digit series has been populated (it is fetched by
# scripts/update_trade_data.R). The drill-down degrades gracefully if not.
sitc5_available <- nrow(monthly_sitc5_all) > 0
destination_data_available <- nrow(monthly_partner_sitc1_all) > 0 && nrow(annual_partner_sitc1_all) > 0

monitor_years <- sort(unique(monthly_total_all$year))
annual_years <- sort(unique(annual_total_all$year))
complete_year <- latest_complete_year(monthly_total_all)
default_monitor_year <- max(monitor_years)
default_comparison_years <- utils::tail(annual_years[annual_years <= complete_year], 2)
latest_month_row <- latest_complete_month(monthly_total_all)

# "Last updated" shown in the footer: the most recent modification time of the
# bundled data files, i.e. when the data was last refreshed (by
# scripts/update_trade_data.R). Falls back to today's date if the files cannot
# be inspected.
data_last_updated <- tryCatch({
  data_files <- list.files(file.path(".", "data"), pattern = "\\.parquet$", full.names = TRUE)
  format(max(file.info(data_files)$mtime, na.rm = TRUE), "%e %B %Y")
}, error = function(e) format(Sys.Date(), "%e %B %Y"))

monitor_month_choices <- sort(unique(monthly_total_all$date), decreasing = TRUE)
destination_choice_year <- if (2025L %in% annual_partner_total_all$year) 2025L else complete_year
partner_display_lookup <- partner_labels |>
  transmute(
    partner = as.character(partner),
    partner_display = if_else(
      is.na(partner_label) | partner_label == "",
      partner,
      paste0(
        partner, " - ",
        stringr::str_squish(stringr::str_replace(partner_label, "\\s*\\(.*\\)$", ""))
      )
    )
  )
partner_display_value <- function(code) {
  label <- partner_display_lookup$partner_display[match(code, partner_display_lookup$partner)]
  if (length(label) == 0 || is.na(label)) code else label
}
product_choice_year <- complete_year

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#004d44",
  secondary = "#5c6270",
  success = "#2c55a2",
  bg = "#f6f6f2",
  fg = "#0b0c0c",
  base_font = font_google("Lato"),
  heading_font = font_google("Lato"),
  code_font = font_google("IBM Plex Mono")
)

# ---------------------------------------------------------------------------
# Visual identity
# ---------------------------------------------------------------------------
# A single source of truth for the app's colours, chart theme and table
# styling so every plot and table reads as one coherent, presentation-quality
# product rather than a set of unrelated outputs.

# Core institutional palette. The teal/burgundy pairing matches the bslib
# theme above; the highlight red is reserved for "latest" / "selected" points.
trade_colors <- list(
  primary   = "#004d44",  # teal  - main series, primary bars
  accent    = "#2c55a2",  # burgundy - secondary series
  highlight = "#ae1122",  # red   - latest / selected marker
  ink       = "#0b0c0c",  # body text
  grid      = "#e5e7eb",  # axis gridlines
  muted     = "#5c6270"   # axis titles, captions
)

# Ten coherent, reasonably distinct colours for the SITC 1-digit sections.
# Replaces RColorBrewer "Set3" (pastel, low contrast) with a steadier palette
# anchored on the teal/burgundy identity. Used both for the stacked
# contribution chart and, via the discrete-scale defaults below, for every
# multi-series ggplot, so a given SITC section keeps the same colour app-wide.
sitc1_section_palette <- c(
  "#ab9965", "#6b5b2e", "#8c5a2b", "#a3402b", "#d9cca3",
  "#004d44", "#8fc1b5", "#2c55a2", "#8ba6d6", "#c8cbd0"
)

# Shared ggplot theme: clean, minimal chrome, white panels (the default grey
# ggplot panel is the main "prototype" tell once piped through plotly).
theme_trade <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text             = ggplot2::element_text(color = trade_colors$ink),
      axis.title       = ggplot2::element_text(color = trade_colors$muted, size = ggplot2::rel(0.85)),
      axis.text        = ggplot2::element_text(color = trade_colors$ink, size = ggplot2::rel(0.82)),
      panel.grid.major = ggplot2::element_line(color = trade_colors$grid, linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_text(color = trade_colors$muted, size = ggplot2::rel(0.8)),
      legend.text      = ggplot2::element_text(size = ggplot2::rel(0.8)),
      plot.margin      = ggplot2::margin(6, 10, 6, 6)
    )
}

# Apply the theme and the institutional discrete palette globally, so every
# ggplot/ggplotly chart inherits them without per-chart code. Charts with a
# single hard-coded colour (e.g. geom_col(fill = ...)) are unaffected.
ggplot2::theme_set(theme_trade())
options(
  ggplot2.discrete.colour = sitc1_section_palette,
  ggplot2.discrete.fill   = sitc1_section_palette
)

# Human-readable column headers for the DT tables. Keys are the internal
# (snake_case) data-frame names; anything not listed is title-cased on the fly.
dt_label_lookup <- c(
  partner = "Country", country = "Country", section = "SITC section",
  sitc1 = "SITC 1", sitc5 = "SITC code", product = "Product",
  description = "Description", year = "Year", month = "Month",
  value_eur = "Value", current_value = "Latest value",
  quantity_kg = "Quantity (kg)",
  mom_change = "MoM change", mom_pct = "MoM %",
  yoy_change = "YoY change", yoy_pct = "YoY %",
  share_of_destination = "Share of country",
  share_of_partner = "Share of country",
  share_of_product = "Share of product",
  share_of_total = "Share of total"
)

pretty_dt_names <- function(nms) {
  out <- unname(dt_label_lookup[nms])
  missing <- is.na(out)
  out[missing] <- tools::toTitleCase(gsub("_", " ", nms[missing]))
  out
}

# Consistent, professional DT wrapper: shared styling class and readable
# headers, while forwarding every other argument (rownames, selection,
# options, ...) straight through to DT::datatable.
trade_datatable <- function(data, ...) {
  DT::datatable(
    data,
    class = "trade-dt row-border hover compact",
    colnames = pretty_dt_names(names(data)),
    ...
  )
}

# ---------------------------------------------------------------------------
# Product finder (reusable Shiny module)
# ---------------------------------------------------------------------------
# Browse the SITC hierarchy at any granularity (1-5 digit) via a searchable,
# sortable table and return the selected item. Used on the "Product by country"
# and "Product over time" pages so product discovery is identical in both places.
productFinderUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$div(
      class = "product-finder",
      radioButtons(
        ns("level"), "Level of detail",
        choices = c("Section (1)" = "1", "Division (2)" = "2", "Group (3)" = "3",
                    "Subgroup (4)" = "4", "Detailed (5)" = "5"),
        selected = "5", inline = TRUE
      ),
      tags$p(
        class = "text-muted small mb-2",
        paste0(
          "Pick a level, then search by code or description and click a row to select. ",
          "Value and share rank products by their full-year total in ", product_choice_year,
          " (the latest complete year)."
        )
      ),
      tags$div(
        class = "trade-monitor-table",
        DT::DTOutput(ns("table"))
      )
    )
  )
}

# choice_fn : function(level_int) -> tibble(code, label_display, sitc_desc, value_eur, share)
# preselect : reactive() returning a code to force-select (e.g. from a drilldown), or NULL.
# Returns a reactive list(code, level, label, value_eur, share) for the selection.
productFinderServer <- function(id, choice_fn, preselect = shiny::reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    active_code <- reactiveVal(NULL)
    pending <- reactiveVal(NULL)  # one-shot external preselect awaiting its level

    level <- reactive(as.integer(req(input$level)))
    level_data <- reactive(choice_fn(level()))

    output$table <- DT::renderDT({
      d <- level_data()
      validate(need(nrow(d) > 0, "No products available at this level."))
      d |>
        dplyr::mutate(
          value_sort = value_eur,
          share_sort = share
        ) |>
        dplyr::transmute(
          sitc = code,
          description = dplyr::coalesce(sitc_desc, ""),
          value_eur = label_eur_billions(value_eur),
          share = label_share(share),
          value_sort = value_sort,
          share_sort = share_sort
        ) |>
        DT::datatable(
          rownames = FALSE,
          selection = "single",
          class = "trade-dt row-border hover compact",
          colnames = c("SITC", "Description", paste0("Value (", product_choice_year, ")"),
                       paste0("Share (", product_choice_year, ")"), "value_sort", "share_sort"),
          options = list(
            dom = "ftp", pageLength = 12, lengthChange = FALSE,
            order = list(list(4, "desc")),
            autoWidth = TRUE,
            scrollY = 420,
            scrollCollapse = TRUE,
            columnDefs = list(
              list(targets = c(4, 5), visible = FALSE, searchable = FALSE),
              list(orderData = 4, targets = 2),
              list(orderData = 5, targets = 3),
              list(width = "13%", targets = 0),
              list(width = "55%", targets = 1),
              list(className = "dt-body-end", targets = c(2, 3))
            )
          )
        )
    })

    proxy <- DT::dataTableProxy("table")

    # When the level (or upstream flow/year) changes the table reloads: honour a
    # pending external preselect if it belongs to this level, else default to the
    # top item.
    observeEvent(level_data(), {
      d <- level_data()
      if (nrow(d) == 0) { active_code(NULL); return() }
      pend <- pending()
      if (!is.null(pend) && pend %in% d$code) {
        active_code(pend)
        DT::selectRows(proxy, which(d$code == pend))
        pending(NULL)
      } else {
        active_code(d$code[[1]])
        DT::selectRows(proxy, 1)
      }
    }, ignoreNULL = FALSE)

    observeEvent(input$table_rows_selected, {
      d <- level_data(); row <- input$table_rows_selected
      if (length(row) == 1 && row >= 1 && row <= nrow(d)) active_code(d$code[[row]])
    })

    # External preselect: switch to the code's level (then the data observer
    # selects it); if already on that level, select immediately.
    observeEvent(preselect(), {
      pre <- preselect()
      if (is.null(pre) || !nzchar(pre)) return()
      pending(pre)
      lv <- as.character(nchar(pre))
      if (!identical(input$level, lv)) {
        updateRadioButtons(session, "level", selected = lv)
      } else {
        d <- level_data()
        if (pre %in% d$code) {
          active_code(pre); DT::selectRows(proxy, which(d$code == pre)); pending(NULL)
        }
      }
    }, ignoreInit = TRUE)

    reactive({
      d <- level_data(); code <- active_code()
      if (is.null(d) || nrow(d) == 0 || is.null(code)) return(NULL)
      r <- d[d$code == code, , drop = FALSE]
      if (nrow(r) == 0) r <- d[1, , drop = FALSE]
      list(
        code = r$code[[1]], level = level(), label = r$label_display[[1]],
        value_eur = r$value_eur[[1]], share = r$share[[1]]
      )
    })
  })
}

metric_card <- function(title, value, subtitle = NULL) {
  card(
    class = "h-100 shadow-sm border-0",
    card_header(title),
    card_body(
      tags$div(class = "display-6 fw-semibold", value),
      if (!is.null(subtitle)) tags$p(class = "text-muted mb-0 mt-2", subtitle)
    )
  )
}

section_intro <- function(text) {
  card(
    class = "trade-monitor-card border-0 shadow-sm",
    fill = FALSE,
    card_body(
      tags$div(class = "trade-monitor-intro", text)
    )
  )
}

# FT-style ranked horizontal bar: clean, data-ink-only chart with the value
# printed directly at the end of each bar, no axis chrome, largest at the top.
# `label_col` / `value_col` are column names (strings). Literal vectors are
# passed to plotly so there is no non-standard evaluation at build time.
ft_hbar <- function(data, label_col, value_col, accent = trade_colors$primary,
                    value_formatter = label_trade_value, top_n = 12) {
  df <- data.frame(
    lab = as.character(data[[label_col]]),
    val = as.numeric(data[[value_col]]),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$val), , drop = FALSE]
  df <- df[order(-df$val), , drop = FALSE]
  if (nrow(df) > top_n) df <- df[seq_len(top_n), , drop = FALSE]
  df <- df[order(df$val), , drop = FALSE]
  df$lab <- factor(df$lab, levels = df$lab)

  plotly::plot_ly(
    x = df$val, y = df$lab, type = "bar", orientation = "h",
    marker = list(color = accent, line = list(width = 0)),
    text = paste0("  ", value_formatter(df$val)), textposition = "outside", cliponaxis = FALSE,
    textfont = list(color = trade_colors$ink, size = 12),
    hovertext = paste0(as.character(df$lab), "<br>", value_formatter(df$val)), hoverinfo = "text"
  ) |>
    plotly::layout(
      margin = list(l = 6, r = 84, t = 6, b = 12),
      bargap = 0.34,
      xaxis = list(visible = FALSE, range = c(0, max(df$val, na.rm = TRUE) * 1.22)),
      yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 12.5)),
      showlegend = FALSE
    ) |>
    plotly::config(displayModeBar = FALSE)
}

quick_link_button <- function(id, label) {
  actionButton(
    id,
    label,
    class = "btn btn-outline-primary btn-sm trade-monitor-quick-link"
  )
}

# Consistent provenance line for every chart. An optional chart-specific note
# (e.g. units or a methodology pointer) is shown above the shared source note.
chart_footer <- function(note = NULL) {
  card_footer(
    class = "text-muted small",
    if (!is.null(note)) tagList(tags$span(note), tags$br()),
    EUROSTAT_SOURCE_NOTE
  )
}

destination_chart_footer <- function(note = NULL) {
  card_footer(
    class = "text-muted small",
    if (!is.null(note)) tagList(tags$span(note), tags$br()),
    paste(
      "Source: Eurostat Comext, monthly API dataset DS-059331",
      "(reporter = Ireland, selected partner country). Annual figures",
      "are summed from the monthly series. SITC Rev. 4 classification."
    )
  )
}

product_country_chart_footer <- function(note = NULL) {
  card_footer(
    class = "text-muted small",
    if (!is.null(note)) tagList(tags$span(note), tags$br()),
    paste(
      "Source: Eurostat Comext, monthly API dataset DS-059331",
      "(reporter = Ireland, selected 5-digit SITC product, partner countries).",
      "Annual figures are summed from the monthly series. SITC Rev. 4 classification."
    )
  )
}

# Default zoom window (months) for the long monthly history charts. The full
# series stays available via the plotly range slider.
default_zoom_months <- 36

ui <- page_sidebar(
  title = tags$div(
    class = "trade-monitor-brand",
    tags$div(
      class = "trade-monitor-title-row",
      tags$div(
        class = "d-flex align-items-center gap-2",
        tags$span("Irish Trade Monitor"),
        tags$span(class = "trade-monitor-version", "v2.0")
      )
    ),
    tags$div(class = "trade-monitor-byline", "Economics Division, Department of Finance")
  ),
  theme = app_theme,
  fillable = FALSE,
  sidebar = sidebar(
    width = 270,
    tags$div(class = "small text-uppercase fw-semibold text-muted mb-2", "Across the app"),
    radioButtons(
      "trade_flow",
      "Show",
      choices = c("Exports" = "2", "Imports" = "1"),
      selected = "2",
      inline = TRUE
    ),
    tags$p(
      class = "small text-muted mt-n1 mb-3",
      "Switches every flow-specific chart between exports and imports. Charts that show both flows, or the trade balance, say so in their subtitle."
    ),
    tags$hr(),
    tags$p(
      class = "small text-muted mb-2",
      tags$strong("Classification: "),
      "all figures use the SITC Rev. 4 trade classification. Broad groups are the 1-digit categories; detailed products are 5-digit codes."
    ),
    tags$p(
      class = "small text-muted mb-2",
      tags$strong("Data: "),
      "Eurostat Comext monthly API (partner = World). Annual figures are summed from the monthly series."
    ),
    tags$p(
      class = "small text-muted mb-0",
      "The most recent one or two months are provisional and routinely revised by Eurostat."
    )
  ),
  tags$head(
    tags$title("Irish Trade Monitor"),
    tags$meta(
      name = "description",
      content = "Irish Trade Monitor - a dashboard for monitoring Irish goods trade with the World by product and country, built on Eurostat Comext data."
    ),
    tags$style(HTML("
      .trade-monitor-intro {
        max-width: 72rem;
        color: #4a4f57;
        font-size: 0.98rem;
        line-height: 1.55;
      }
      .trade-monitor-brand {
        line-height: 1.05;
        background: #ffffff;
        border: 1px solid #dde3e0;
        border-radius: 0.9rem;
        padding: 0.85rem 0.95rem 0.8rem;
        box-shadow: 0 8px 24px rgba(15, 23, 42, 0.06);
        margin-bottom: 0.8rem;
      }
      .trade-monitor-title-row {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 0.75rem;
        font-weight: 700;
        color: #0b0c0c;
      }
      .trade-monitor-version {
        display: inline-flex;
        align-items: center;
        padding: 0.16rem 0.5rem;
        border-radius: 999px;
        background: #004d44;
        color: #ffffff;
        font-size: 0.72rem;
        font-weight: 700;
        letter-spacing: 0.08em;
        text-transform: uppercase;
      }
      .trade-monitor-quick-links {
        display: flex;
        flex-wrap: wrap;
        gap: 0.75rem;
      }
      .trade-monitor-quick-link {
        border-width: 1px;
      }
      .trade-monitor-byline {
        margin-top: 0.32rem;
        font-size: 0.9rem;
        font-weight: 700;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        color: #374151;
      }
      .trade-monitor-card .card-body {
        padding: 1.1rem 1.2rem;
      }
      .trade-monitor-table .dataTables_filter {
        margin-bottom: 0.75rem;
      }
      .trade-monitor-table .dataTables_info {
        color: #4a4f57;
        font-size: 0.9rem;
      }
      .trade-monitor-table table.dataTable tbody tr.selected > * {
        box-shadow: inset 0 0 0 9999px rgba(15, 76, 92, 0.14);
        color: inherit;
      }
      .trade-monitor-table table.dataTable tbody td {
        vertical-align: top;
      }
      .product-finder .form-group {
        margin-bottom: 0.65rem;
      }
      .product-finder .shiny-options-group {
        display: flex;
        flex-wrap: wrap;
        gap: 0.5rem 1rem;
      }
      .product-finder .radio-inline,
      .product-finder .form-check-inline {
        margin-right: 0;
      }
      .product-finder .dataTables_wrapper .dataTables_filter,
      .product-finder .dataTables_wrapper .dataTables_paginate {
        margin-top: 0.6rem;
      }
      .trade-monitor-metric-grid .card-header {
        font-size: 0.9rem;
      }
      /* Section heading that introduces a drill-down (pair) block. */
      .trade-monitor-drilldown-head {
        margin: 1.4rem 0 0.6rem;
        padding-bottom: 0.4rem;
        border-bottom: 2px solid #e5e7eb;
        font-size: 1.05rem;
        font-weight: 700;
        color: #003a33;
      }

      /* ---- Cards ---------------------------------------------------------- */
      /* Slightly warmer page background and softer card surfaces give the app
         a steadier, more 'briefing document' feel than flat white-on-white. */
      .trade-monitor-card {
        border-radius: 0.75rem;
        overflow: hidden;
      }
      .trade-monitor-card > .card-header {
        background: #ffffff;
        border-bottom: 1px solid #e5e7eb;
        font-weight: 600;
        color: #0b0c0c;
        letter-spacing: 0.01em;
        padding: 0.7rem 1.2rem;
      }
      .card-footer.small {
        background: #fafaf7;
        border-top: 1px solid #eef0ee;
        line-height: 1.45;
      }

      /* ---- Metric (KPI) cards -------------------------------------------- */
      /* A thin accent rule plus a quieter, uppercase header turns the headline
         cards into clean KPI tiles. */
      .trade-monitor-metric-grid .card,
      .layout-column-wrap .card {
        border-top: 3px solid #004d44;
      }
      .layout-column-wrap .card .card-header,
      .trade-monitor-metric-grid .card .card-header {
        background: transparent;
        border-bottom: none;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        font-size: 0.74rem;
        font-weight: 700;
        color: #5c6270;
        padding-bottom: 0;
      }
      .layout-column-wrap .card .display-6 {
        font-size: 1.85rem;
        color: #003a33;
      }

      /* ---- Navigation tabs ----------------------------------------------- */
      .nav-tabs .nav-link {
        color: #4a4f57;
        font-weight: 600;
        border: none;
        border-bottom: 2px solid transparent;
      }
      .nav-tabs .nav-link:hover {
        color: #004d44;
        border-bottom-color: #d8dadf;
      }
      .nav-tabs .nav-link.active {
        color: #004d44;
        background: transparent;
        border-bottom: 2px solid #004d44;
      }

      /* ---- DataTables ----------------------------------------------------- */
      /* Quiet header band, right-aligned numerics and roomy rows make the
         tables read like report tables rather than raw data dumps. */
      table.trade-dt.dataTable thead th {
        background: #efefe9;
        color: #374151;
        font-size: 0.72rem;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        border-bottom: 2px solid #dde3e0 !important;
      }
      table.trade-dt.dataTable tbody td {
        vertical-align: middle;
        font-variant-numeric: tabular-nums;
      }
      table.trade-dt.dataTable tbody tr:hover > * {
        box-shadow: inset 0 0 0 9999px rgba(15, 76, 92, 0.05);
      }
      table.trade-dt.dataTable tbody tr.selected > * {
        box-shadow: inset 0 0 0 9999px rgba(15, 76, 92, 0.14);
        color: inherit;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: #004d44 !important;
        border-color: #004d44 !important;
        color: #ffffff !important;
        border-radius: 0.4rem;
      }
      .dataTables_wrapper .dataTables_filter input {
        border: 1px solid #d8dadf;
        border-radius: 0.4rem;
        padding: 0.2rem 0.5rem;
      }

      /* ---- About / methodology (README) page ----------------------------- */
      .trade-monitor-readme {
        max-width: 56rem;
        color: #374151;
        line-height: 1.6;
      }
      .trade-monitor-readme h2 {
        font-weight: 700;
        color: #003a33;
        margin-bottom: 0.5rem;
      }
      .trade-monitor-readme h3 {
        font-size: 1.05rem;
        font-weight: 700;
        color: #0b0c0c;
        margin-top: 1.6rem;
        margin-bottom: 0.5rem;
      }
      .trade-monitor-readme ul {
        padding-left: 1.2rem;
      }
      .trade-monitor-readme li {
        margin-bottom: 0.25rem;
      }

      /* Inline radio toggles sitting inside card headers should not carry the
         default bottom margin that would misalign them with the title. */
      .card-header .shiny-input-radiogroup {
        margin-bottom: 0;
      }
      .card-header .shiny-input-radiogroup .control-label {
        display: none;
      }

      /* ---- App footer ----------------------------------------------------- */
      .trade-monitor-credit {
        color: #4a4f57;
        border-top: 1px solid #e5e7eb;
        padding-top: 0.55rem;
        margin-top: 0.4rem;
      }
      .trade-monitor-footer {
        margin-top: 1.25rem;
        padding-top: 0.9rem;
        border-top: 1px solid #e5e7eb;
        color: #5c6270;
        font-size: 0.82rem;
        line-height: 1.5;
      }

      @media (max-width: 991.98px) {
        .trade-monitor-intro {
          font-size: 0.95rem;
        }
        .layout-column-wrap .card .display-6 {
          font-size: 1.55rem;
        }
      }
    "))
  ),
  tm_page_css(),
  navset_card_tab(
    id = "main_tabs",
    full_screen = TRUE,
    # --- Analytical pages (new in v2.0): each is a module in R/mod_*.R --------
    nav_panel("Briefing", mod_briefing_ui("briefing")),
    nav_panel("Trends", mod_trends_ui("trends")),
    nav_panel("Concentration", mod_structure_ui("structure")),
    nav_panel("Markets", mod_markets_ui("markets")),
    # --- Explorer: the original drill-down pages, unchanged ------------------
    nav_menu(
    "Explorer",
    nav_panel(
      "Summary tables",
      section_intro(
        tagList(
          tags$p(
            class = "mb-2",
            "Start here for the headline picture. This page shows the latest trade total, the broad product mix, and the main country markets, then points you to the deeper pages when you want to explain a move."
          ),
          tags$p(
            class = "trade-monitor-credit small mb-0",
            "Developed and maintained by ", tags$strong("Stephen Byrne"), ", Economics Division, Department of Finance. ",
            "Have a feature you'd like added to the Trade Monitor? ",
            tags$a(
              href = "mailto:stephenx.byrne@finance.gov.ie?subject=Irish%20Trade%20Monitor%20feature%20request",
              "Email me"
            ),
            " and I'll take a look."
          )
        )
      ),
      card(
        class = "trade-monitor-card border-0 shadow-sm",
        fill = FALSE,
        card_body(
          tags$p(
            class = "fw-semibold mb-2",
            "Common questions"
          ),
          tags$div(
            class = "trade-monitor-quick-links",
            quick_link_button("go_monthly_monitoring", "What changed this month?"),
            quick_link_button("go_country_view", "What do we trade with a country?"),
            quick_link_button("go_product_destinations", "Where does a product go?"),
            quick_link_button("go_product_explorer", "Track a product over time")
          )
        )
      ),
      # Wide headline chart on top, then the two summary tables side by side.
      card(
        class = "trade-monitor-card border-0 shadow-sm",
        full_screen = TRUE,
        fill = FALSE,
        card_header("Monthly goods trade with the World"),
        plotlyOutput("monthly_total_plot", height = 360),
        chart_footer("Total Irish goods trade with the World, EUR. Latest month marked in red.")
      ),
      layout_columns(
        fill = FALSE,
        col_widths = c(6, 6),
        card(
          class = "trade-monitor-card trade-monitor-table border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header(paste0("Trade by SITC 1-digit section, ", complete_year)),
          DTOutput("overview_sitc1_table"),
          chart_footer("Latest complete calendar year; share is of total goods trade.")
        ),
        card(
          class = "trade-monitor-card trade-monitor-table border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header(paste0("Top partner markets, ", complete_year)),
          DTOutput("top_partners_table"),
          chart_footer("Latest complete calendar year; share is of total goods trade.")
        )
      ),
      card(
        class = "trade-monitor-card trade-monitor-table border-0 shadow-sm",
        full_screen = TRUE,
        fill = FALSE,
        card_header("Latest monthly totals"),
        DTOutput("latest_months_table")
      )
    ),
    nav_panel(
      "Monthly monitoring",
      section_intro(
        "Use this page when you want to answer: what changed this month? Start with the headline charts, then use the country and product jump tables below to find the main drivers."
      ),
      card(
        class = "trade-monitor-card border-0 shadow-sm",
        fill = FALSE,
        card_body(
          layout_columns(
            fill = FALSE,
            col_widths = c(3, 3, 2, 2, 2),
            selectInput(
              "monitor_month",
              "Month to review",
              choices = stats::setNames(as.character(monitor_month_choices), format(monitor_month_choices, "%b %Y")),
              selected = as.character(latest_month_row$date)
            ),
            selectInput(
              "monitor_partner",
              "Country to watch",
              choices = NULL,
              selected = NULL
            ),
            radioButtons(
              "monitor_jump_basis",
              "Compare with",
              choices = c("Same month last year" = "yoy", "Previous month" = "mom"),
              selected = "yoy",
              inline = TRUE
            ),
            sliderInput(
              "monitor_jump_top_n",
              "Show top",
              min = 5,
              max = 25,
              value = 10,
              step = 1
            ),
            numericInput(
              "monitor_jump_min_bn",
              "Ignore changes smaller than (€bn)",
              value = 0.5,
              min = 0,
              step = 0.1
            )
          ),
          tags$p(
            class = "text-muted mb-0 mt-2",
            "Use this block to scan monthly jumps by partner and product. Partner drill-down uses the bundled top monitoring markets for the selected trade flow."
          )
        )
      ),
      # Biggest movers - placed high on the page as the direct answer to
      # "what changed this month?". Two ranked tables with drill-through buttons.
      tags$div(class = "trade-monitor-drilldown-head", "Biggest movers this month"),
      section_intro(
        tagList(
          "These two tables are the quickest way to see ", tags$strong("what drove the change this month"),
          ": the countries and the detailed products that moved Irish trade the most, ranked by the size of the change against the comparison period chosen above. ",
          tags$strong("Select a row and click the button beneath the table"),
          " to drill into the detail - the products behind a country's move, or the countries behind a product's move."
        )
      ),
      layout_columns(
        fill = FALSE,
        col_widths = c(6, 6),
        card(
          class = "trade-monitor-card trade-monitor-table border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header(uiOutput("monitor_partner_jump_header")),
          card_body(
            uiOutput("monitor_partner_jump_note"),
            DTOutput("monitor_partner_jump_table")
          ),
          card_footer(
            class = "d-flex gap-2 align-items-center flex-wrap",
            actionButton(
              "monitor_open_country_product",
              "Show what drove this country move ↓",
              class = "btn btn-outline-primary btn-sm"
            ),
            uiOutput("monitor_partner_jump_selection")
          )
        ),
        card(
          class = "trade-monitor-card trade-monitor-table border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header(uiOutput("monitor_product_jump_header")),
          card_body(
            uiOutput("monitor_product_jump_note"),
            DTOutput("monitor_product_jump_table")
          ),
          card_footer(
            class = "d-flex gap-2 align-items-center flex-wrap",
            actionButton(
              "monitor_open_product_country",
              "Show what drove this product move ↓",
              class = "btn btn-outline-primary btn-sm"
            ),
            uiOutput("monitor_product_jump_selection")
          )
        )
      ),
      # Drivers of a selected move: revealed when a row's button is clicked.
      # Decomposes the chosen country's (or product's) change into the products
      # (or countries) that added to or subtracted from it - so it works for
      # falls as well as rises.
      card(
        class = "trade-monitor-card border-0 shadow-sm",
        full_screen = TRUE,
        fill = FALSE,
        card_header(uiOutput("mover_drivers_header")),
        card_body(
          uiOutput("mover_drivers_headline"),
          plotlyOutput("mover_drivers_plot", height = 400)
        ),
        chart_footer("Each bar is a component's contribution, in percentage points, to the entity's year-on-year (or month-on-month) change = 100 x (component change) / (entity total a period earlier). Bars sum to the headline percentage. Hover for the euro change.")
      ),
      card(
        class = "trade-monitor-card border-0 shadow-sm",
        full_screen = TRUE,
        fill = FALSE,
        card_header("SITC contributions to year-on-year cumulative trade growth"),
        card_body(
          tags$p(
            class = "text-muted mb-1",
            "Stacked bars show each SITC 1-digit section's percentage-point contribution to year-to-date (January-to-month) trade growth versus the same period a year earlier. The black line is the overall cumulative growth rate; by construction the bars sum to it."
          ),
          tags$p(
            class = "text-muted small mb-2",
            paste0("Chart opens on the most recent ", default_zoom_months, " months. Drag the slider beneath the chart to view the full history back to 2015.")
          ),
          plotlyOutput("contribution_plot", height = 430)
        ),
        chart_footer("Contribution (pp) = 100 x (section YTD change) / (total YTD a year earlier).")
      ),
      card(
        class = "trade-monitor-card border-0 shadow-sm",
        full_screen = TRUE,
        fill = FALSE,
        card_header("Monthly trade by SITC 1-digit"),
        plotlyOutput("monthly_sitc1_plot", height = 420),
        chart_footer("Monthly trade value by section, EUR.")
      ),
      layout_column_wrap(
        fill = FALSE,
        width = 1 / 3,
        uiOutput("monitor_month_total_card"),
        uiOutput("monitor_partner_total_card"),
        uiOutput("monitor_partner_share_card")
      ),
      layout_columns(
        fill = FALSE,
        col_widths = c(7, 5),
        card(
          class = "trade-monitor-card border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header(uiOutput("monitor_partner_history_header")),
          plotlyOutput("monitor_partner_history_plot", height = 360),
          chart_footer("Selected partner market, monthly goods trade value, EUR. Red point marks the selected monitoring month.")
        ),
        card(
          class = "trade-monitor-card border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header(uiOutput("monitor_partner_sitc1_header")),
          plotlyOutput("monitor_partner_sitc1_plot", height = 360),
          chart_footer("Selected month, split by SITC 1-digit section for the chosen partner market.")
        )
      )
    ),
    nav_panel(
      "Annual comparison",
      section_intro(
        "Compare broad product groups across years, then see the leading detailed (5-digit) products for the latest selected year. Choose the years to compare below."
      ),
      card(
        class = "trade-monitor-card border-0 shadow-sm",
        fill = FALSE,
        card_body(
          selectInput(
            "comparison_years",
            "Years to compare",
            choices = rev(annual_years),
            selected = rev(default_comparison_years),
            multiple = TRUE
          )
        )
      ),
      uiOutput("annual_context_banner"),
      layout_columns(
        fill = FALSE,
        col_widths = c(7, 5),
        card(
          class = "trade-monitor-card border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header("Annual trade by SITC 1-digit"),
          plotlyOutput("annual_comparison_plot", height = 380),
          chart_footer("Selected years compared side by side, EUR.")
        ),
        card(
          class = "trade-monitor-card border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header("Annual totals"),
          DTOutput("annual_totals_table")
        )
      ),
      card(
        class = "trade-monitor-card border-0 shadow-sm",
        full_screen = TRUE,
        fill = FALSE,
        card_header("Top detailed (5-digit) products"),
        card_body(
          tags$p(
            class = "text-muted small mb-2",
            "Leading 5-digit SITC products for the latest selected year, with each product's share of total trade. Use the search box to filter; descriptions are shown where available."
          ),
          DTOutput("top_products_table")
        )
      )
    ),
    nav_panel(
      "By country",
      section_intro(
        paste0(
          "Start with one country. See its headline totals, the broad product mix (SITC sections), ",
          "the leading detailed products, and its trade over time - then click a detailed product to drill ",
          "into the exact country-product pair. Countries are the top 20 markets in ", destination_choice_year,
          "; switch between Exports and Imports with the filter in the left sidebar."
        )
      ),
      card(
        class = "trade-monitor-card border-0 shadow-sm",
        fill = FALSE,
        card_body(
          layout_columns(
            fill = FALSE,
            col_widths = c(4, 3, 5),
            selectInput(
              "country_product_partner",
              "Country",
              choices = NULL,
              selected = NULL
            ),
            radioButtons(
              "country_product_period_type",
              "Period type",
              choices = c("Month" = "month", "Year" = "year"),
              selected = "month",
              inline = TRUE
            ),
            uiOutput("country_product_period_selector")
          )
        )
      ),
      uiOutput("country_product_view_banner"),
      layout_column_wrap(
        class = "trade-monitor-metric-grid",
        fill = FALSE,
        width = 1 / 4,
        uiOutput("country_product_total_card"),
        uiOutput("cp_share_card"),
        uiOutput("cp_top_section_card"),
        uiOutput("country_product_top_product_card")
      ),
      # Broad product mix (SITC 1-digit) from bundled data - instant, no fetch.
      # Merged in from the former standalone "Country view" tab.
      layout_columns(
        fill = FALSE,
        col_widths = c(7, 5),
        card(
          class = "trade-monitor-card border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header(uiOutput("cp_sitc1_header")),
          card_body(plotlyOutput("cp_sitc1_plot", height = 380)),
          destination_chart_footer("Selected country, split by SITC 1-digit section.")
        ),
        card(
          class = "trade-monitor-card border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header("Broad product groups"),
          DTOutput("cp_sitc1_table")
        )
      ),
      # Detailed products for the selected country (full width for room).
      card(
        class = "trade-monitor-card trade-monitor-table border-0 shadow-sm",
        full_screen = TRUE,
        fill = FALSE,
        card_header(uiOutput("country_product_table_header")),
        card_body(
          tags$p(
            class = "text-muted mb-3",
            "The country's detailed (5-digit) products, plus the overall TOTAL row. ",
            tags$strong("Click a product"),
            " to see how trade in it with this country has evolved, in the panel below."
          ),
          DTOutput("country_product_sitc5_table")
        ),
        destination_chart_footer("Selected country, detailed 5-digit SITC products for the chosen period.")
      ),
      # Selected country-product pair: KPIs, then two clear time-series side by
      # side (full width so each chart is readable).
      tags$div(class = "trade-monitor-drilldown-head", uiOutput("country_product_pair_header", inline = TRUE)),
      layout_column_wrap(
        class = "trade-monitor-metric-grid",
        fill = FALSE,
        width = 1 / 4,
        uiOutput("country_product_selected_product_card"),
        uiOutput("country_product_selected_value_card"),
        uiOutput("country_product_selected_share_partner_card"),
        uiOutput("country_product_selected_share_product_card")
      ),
      layout_columns(
        fill = FALSE,
        col_widths = c(6, 6),
        card(
          class = "trade-monitor-card border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header("Trade value over time"),
          plotlyOutput("country_product_history_plot", height = 340),
          product_country_chart_footer("Monthly trade in the selected product with the selected country, EUR.")
        ),
        card(
          class = "trade-monitor-card border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header("Shares over time"),
          card_body(
            tags$p(
              class = "text-muted small mb-2",
              "How central this product is to the relationship: its share of the country's total trade, and the country's share of this product's worldwide trade."
            ),
            plotlyOutput("country_product_share_plot", height = 300)
          )
        )
      ),
      # Country trade over time (bundled data) - merged in from "Country view".
      card(
        class = "trade-monitor-card border-0 shadow-sm",
        full_screen = TRUE,
        fill = FALSE,
        card_header(uiOutput("cp_history_header")),
        plotlyOutput("cp_history_plot", height = 320),
        destination_chart_footer("Monthly goods trade with the selected country, EUR. Selected period marked in red.")
      )
    ),
    nav_panel(
      "Product by country",
      section_intro(
        paste0(
          "Start with a product at any level of detail - from a broad SITC section down to a 5-digit product - ",
          "then see which countries it goes to. Pick a level, search the table and choose an item. ",
          "The full country breakdown opens for detailed (5-digit) products; broader levels show the group's trend. ",
          "Rankings are based on the latest complete year (", product_choice_year, ")."
        )
      ),
      card(
        class = "trade-monitor-card trade-monitor-table border-0 shadow-sm",
        fill = FALSE,
        card_header("Find a product"),
        card_body(
          productFinderUI("pc_finder"),
          tags$hr(class = "my-2"),
          layout_columns(
            fill = FALSE,
            col_widths = c(3, 4, 5),
            radioButtons(
              "product_country_period_type",
              "Period type",
              choices = c("Month" = "month", "Year" = "year"),
              selected = "month",
              inline = TRUE
            ),
            uiOutput("product_country_period_selector"),
            uiOutput("pc_selected_indicator")
          ),
          tags$div(
            class = "mt-2",
            actionButton(
              "pc_open_product_over_time",
              "Open this product in Product over time",
              class = "btn btn-outline-primary btn-sm"
            )
          )
        )
      ),
      uiOutput("product_country_view_banner"),
      # Detailed (5-digit) selection: the full country breakdown + pair drill-down.
      conditionalPanel(
        condition = "input['pc_finder-level'] == '5'",
        layout_column_wrap(
          fill = FALSE,
          width = 1 / 3,
          uiOutput("product_country_total_card"),
          uiOutput("product_country_top_partner_card"),
          uiOutput("product_country_top_partner_share_card")
        ),
        # Where the product goes: ranking table + bar (full width for room).
        card(
          class = "trade-monitor-card trade-monitor-table border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header(uiOutput("product_country_table_header")),
          card_body(
            tags$p(
              class = "text-muted mb-3",
              tags$strong("Click a country"),
              " to see how trade in this product with it has evolved, in the panel below."
            ),
            layout_columns(
              fill = FALSE,
              col_widths = c(6, 6),
              DTOutput("product_country_partner_table"),
              plotlyOutput("product_country_partner_plot", height = 360)
            )
          ),
          product_country_chart_footer("Selected product, split across partner countries for the chosen period.")
        ),
        # Selected product-country pair: KPIs, then two clear time-series.
        tags$div(class = "trade-monitor-drilldown-head", uiOutput("product_country_pair_header", inline = TRUE)),
        layout_column_wrap(
          class = "trade-monitor-metric-grid",
          fill = FALSE,
          width = 1 / 4,
          uiOutput("product_country_selected_country_card"),
          uiOutput("product_country_selected_value_card"),
          uiOutput("product_country_selected_share_product_card"),
          uiOutput("product_country_selected_share_destination_card")
        ),
        layout_columns(
          fill = FALSE,
          col_widths = c(6, 6),
          card(
            class = "trade-monitor-card border-0 shadow-sm",
            full_screen = TRUE,
            fill = FALSE,
            card_header("Trade value over time"),
            plotlyOutput("product_country_history_plot", height = 340),
            product_country_chart_footer("Monthly trade in the selected product with the selected country, EUR.")
          ),
          card(
            class = "trade-monitor-card border-0 shadow-sm",
            full_screen = TRUE,
            fill = FALSE,
            card_header("Shares over time"),
            card_body(
              tags$p(
                class = "text-muted small mb-2",
                "How central this country is to the product: the country's share of the product's worldwide trade, and the product's share of the country's total trade."
              ),
              plotlyOutput("product_country_share_plot", height = 300)
            )
          )
        )
      ),
      # Broader levels (1-4 digit): the country split needs a 5-digit item, so
      # show the selected group's aggregate trend with guidance to drill down.
      conditionalPanel(
        condition = "input['pc_finder-level'] != '5'",
        card(
          class = "trade-monitor-card border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header(uiOutput("pc_aggregate_header")),
          card_body(
            tags$p(
              class = "text-muted mb-2",
              "Total value of the selected SITC group over time (partner = World). For the country breakdown, switch to Detailed (5-digit) and pick an item within this group."
            ),
            plotlyOutput("pc_aggregate_plot", height = 380)
          ),
          chart_footer("Aggregated monthly goods trade for the selected SITC group, EUR.")
        )
      )
    ),
    nav_panel(
      "Product over time",
      card(
        class = "trade-monitor-card border-0 shadow-sm",
        fill = FALSE,
        card_body(
          tags$div(
            class = "trade-monitor-intro",
            tags$strong("How to use this page. "),
            "Pick a level of detail and search the table for a product - anything from a broad SITC section to a 5-digit item - then click it. ",
            "The right-hand panel shows the product's monthly value and its shares of the parent SITC 1-digit section and of total Irish trade."
          ),
          tags$div(
            class = "mt-3",
            actionButton(
              "pe_open_product_country",
              "Open this product in Product by country",
              class = "btn btn-outline-primary btn-sm"
            )
          )
        )
      ),
      layout_columns(
        fill = FALSE,
        col_widths = c(5, 7),
        card(
          class = "trade-monitor-card trade-monitor-table border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header("Find a product"),
          card_body(productFinderUI("pe_finder"))
        ),
        card(
          class = "trade-monitor-card border-0 shadow-sm",
          full_screen = TRUE,
          fill = FALSE,
          card_header("Selected product snapshot"),
          card_body(
            layout_column_wrap(
              class = "trade-monitor-metric-grid",
              fill = FALSE,
              width = 1 / 3,
              uiOutput("selected_product_card"),
              uiOutput("product_share_sitc1_card"),
              uiOutput("product_share_total_card")
            ),
            tags$hr(class = "my-3"),
            plotlyOutput("pe_value_plot", height = 360)
          ),
          chart_footer("Monthly trade value for the selected product (partner = World), EUR.")
        )
      ),
      card(
        class = "trade-monitor-card border-0 shadow-sm",
        full_screen = TRUE,
        fill = FALSE,
        card_header("Contribution shares over time"),
        card_body(
          tags$p(
            class = "text-muted mb-3",
            "The selected product as a share of its parent SITC 1-digit section and of total Irish trade, each month."
          ),
          plotlyOutput("pe_share_plot", height = 380)
        ),
        chart_footer("Product share of parent SITC section and of total trade, monthly.")
      )
    )
    ),  # end nav_menu("Explorer")
    # -----------------------------------------------------------------------
    # Reference tab (last): a README / methodology note (authored in Markdown so it
    # reads like an R Markdown document). States authorship up front, explains
    # what the tool is, the data sources, classification and caveats. Dynamic
    # bits (latest month, last complete year) are interpolated from the data.
    # -----------------------------------------------------------------------
    nav_panel(
      "About",
      card(
        class = "trade-monitor-card border-0 shadow-sm",
        fill = FALSE,
        card_body(
          tags$div(
            class = "trade-monitor-readme",
            shiny::markdown(sprintf(
"## Irish Trade Monitor

**Developed and maintained by Stephen Byrne (Department of Finance).**

The Irish Trade Monitor is an app for tracking Ireland's goods
trade with the rest of the world. It is built for economists and non-specialists
alike: each page answers a specific question - what the latest totals are, what
changed this month, how trade compares across years, what Ireland trades with a
given country, where a given product goes - and, since version 2.0, whether a move is signal or noise, what is
driving it, and how concentrated and exposed Irish trade is.

### What you can do here

**Analytical pages** - built to answer the questions an economist asks on release day.

- **Briefing** - the month in one page: headline exports, imports and balance next to smoothed and underlying measures, auto-drafted key points, what is driving growth, and the biggest product and market movers.
- **Trends** - signal versus noise for any series: level with 3- and 12-month averages, growth on three bases, this year against the previous five, a momentum heatmap, growth contributions and a breadth-of-growth index.
- **Concentration** - how concentrated trade is and whether that is changing: composition, product and market concentration, what is inside each product group, one product against everything else, and value versus volume versus unit value.
- **Markets** - geographic exposure: partner-group shares, front-loading and payback, which markets are driving growth, bilateral balances and a country profile.

**Explorer** - the original drill-down pages, for looking up a specific number.

- **Summary tables** - annual totals by section and partner, and the latest monthly totals.
- **Monthly monitoring** - country and product \"jump\" tables for a chosen month, with drill-through.
- **Annual comparison** - compare broad product groups and detailed products across years.
- **By country** - pick a country and see its product mix, leading products and trade over time.
- **Product by country** - pick a product and see which countries it goes to or comes from.
- **Product over time** - track a product's monthly value and its share of Irish trade over time.

### How the analytical measures are defined

- **3-month measures** compare the latest three months with the same three months a year earlier (\"3m y/y\") or with the preceding three months (\"3m on 3m\"). Irish monthly trade is dominated by lumpy pharmaceutical shipments, so single months are a poor guide to the trend.
- **12-month measures** are rolling 12-month totals; shares and concentration statistics use these.
- **Underlying exports** exclude chemicals and pharmaceuticals (SITC section 5); **underlying imports** exclude aircraft (SITC 792), which are dominated by the leasing sector.
- **Analytical product groups** regroup SITC codes into ten economically meaningful categories: pharmaceuticals (54), organic chemicals (51), other chemicals (rest of section 5), computers & electronics (75, 76, 77 except 774), medical devices & instruments (87, 8996, 774), aircraft (792), other machinery & transport (rest of section 7), food, drink & tobacco (sections 0-1), fuels (section 3) and other goods. Trade that Eurostat suppresses at 5-digit level for confidentiality is allocated to the residual group of its section, so the groups sum exactly to total trade.
- **Partner groups** are the United States, Great Britain & NI, the euro area (current membership, excluding Ireland), other EU, Switzerland, China and the rest of the world.
- **Growth contributions** are additive percentage-point contributions that sum to the growth rate of the total.
- **Concentration** is measured with the Herfindahl-Hirschman index (0-10,000) across 5-digit products or partner countries; the effective number of products is 10,000 divided by the index.
- **Unit values** are value divided by kilograms. They move with product mix and transfer pricing as well as prices and are not a price index.
- **Event markers** on the charts are a short editable list (`TM_EVENTS` in `R/analytics.R`).

### Data sources

All figures come from **Eurostat Comext**, the EU's detailed external-trade
database (monthly API, dataset **DS-059331**), with **Ireland as the reporting
country**.

- Headline and most series use the **partner = World** aggregate.
- **Annual figures are summed from the monthly series**, so the current (in-progress) year is partial.
- Detailed **5-digit product x country** combinations are fetched live from the Comext API when you open the relevant pages.
- Values are **nominal goods trade in euro**; quantities, where shown, are in kilograms.

### Classification

Everything uses the **SITC Rev. 4** classification (Standard International Trade
Classification). Broad groups are the 1-digit sections (0-9); detailed products
are 5-digit codes. Customs (CN / HS) codes are not used.

### Coverage and method

- Monthly data currently runs to **%s**, and complete calendar years are available up to and including **%d**.
- Growth contributions decompose year-on-year growth into additive percentage-point shares that sum to the overall growth rate.
- The **most recent one or two months are provisional** and are routinely revised by Eurostat.

### Important note

This is a personal research and monitoring tool. It is **not an official
statistical release**, and nothing here represents the official position of the
Department of Finance or of Eurostat. For definitive statistics, please refer to
the Central Statistics Office (CSO) and Eurostat.
",
              format(latest_month_row$date, "%B %Y"),
              complete_year
            ))
          )
        )
      )
    )
  ),
  # App-level provenance / caveat footer for a consistent professional finish.
  tags$div(
    class = "trade-monitor-footer",
    tags$div(
      tags$strong("Irish Trade Monitor"),
      " - data: Eurostat Comext (dataset DS-059331), reporter = Ireland. ",
      "All values are nominal goods trade in EUR under the SITC Rev. 4 classification."
    ),
    tags$div(
      class = "mt-1",
      "Figures for the most recent one or two months are provisional and are routinely revised by Eurostat. ",
      "This is a research tool and is not an official statistical release."
    ),
    tags$div(
      class = "mt-1",
      tags$strong("Last updated: "), data_last_updated,
      " | Latest data: ", format(latest_month_row$date, "%B %Y"), "."
    )
  )
)

server <- function(input, output, session) {
  # Analytical pages. They share the pre-built bundle and follow the global
  # Exports / Imports switch in the sidebar.
  tm_flow <- reactive({
    req(input$trade_flow)
    input$trade_flow
  })
  mod_briefing_server("briefing", tm_data, tm_flow)
  mod_trends_server("trends", tm_data, tm_flow)
  mod_structure_server("structure", tm_data, tm_flow)
  mod_markets_server("markets", tm_data, tm_flow)

  observeEvent(input$go_monthly_monitoring, {
    bslib::nav_select("main_tabs", selected = "Monthly monitoring", session = session)
  })

  observeEvent(input$go_country_view, {
    bslib::nav_select("main_tabs", selected = "By country", session = session)
  })

  observeEvent(input$go_product_destinations, {
    bslib::nav_select("main_tabs", selected = "Product by country", session = session)
  })

  observeEvent(input$go_product_explorer, {
    bslib::nav_select("main_tabs", selected = "Product over time", session = session)
  })

  trade_flow <- reactive({
    req(input$trade_flow)
    as.character(input$trade_flow)
  })

  trade_flow_label <- reactive({
    flow_label_value(trade_flow())
  })

  partner_role_label <- reactive({
    partner_role_value(trade_flow())
  })

  monthly_total <- reactive({
    monthly_total_all |>
      filter(flow == trade_flow())
  })

  monthly_sitc1_raw <- reactive({
    monthly_sitc1_raw_all |>
      filter(flow == trade_flow())
  })

  monthly_sitc1 <- reactive({
    monthly_sitc1_all |>
      filter(flow == trade_flow())
  })

  monthly_sitc5 <- reactive({
    monthly_sitc5_all |>
      filter(flow == trade_flow())
  })

  monthly_product <- reactive({
    monthly_product_all |>
      filter(flow == trade_flow())
  })

  monthly_partner_total <- reactive({
    monthly_partner_total_all |>
      filter(flow == trade_flow())
  })

  monthly_partner_sitc1 <- reactive({
    monthly_partner_sitc1_all |>
      filter(flow == trade_flow())
  })

  annual_total <- reactive({
    annual_total_all |>
      filter(flow == trade_flow())
  })

  annual_sitc1 <- reactive({
    annual_sitc1_all |>
      filter(flow == trade_flow())
  })

  annual_sitc5 <- reactive({
    annual_sitc5_all |>
      filter(flow == trade_flow())
  })

  annual_product <- reactive({
    annual_product_all |>
      filter(flow == trade_flow())
  })

  annual_partner_total <- reactive({
    annual_partner_total_all |>
      filter(flow == trade_flow())
  })

  annual_partner_sitc1 <- reactive({
    annual_partner_sitc1_all |>
      filter(flow == trade_flow())
  })

  ytd_contributions <- reactive({
    compute_ytd_contributions(monthly_sitc1())
  })

  current_complete_year <- reactive({
    latest_complete_year(monthly_total())
  })

  current_latest_month <- reactive({
    latest_complete_month(monthly_total())
  })

  current_destination_choice_year <- reactive({
    partner_years <- annual_partner_total()$year
    if (2025L %in% partner_years) 2025L else max(partner_years)
  })

  current_top_destination_codes <- reactive({
    annual_partner_total() |>
      filter(year == current_destination_choice_year()) |>
      slice_max(order_by = value_eur, n = 20, with_ties = FALSE) |>
      pull(partner) |>
      as.character()
  })

  current_top_destination_choices <- reactive({
    codes <- current_top_destination_codes()
    setNames(codes, vapply(codes, partner_display_value, character(1)))
  })

  # Product finder for the "Product by country" page: browse SITC levels 1-5 against
  # the latest complete year, ranked by value. pc_preselect lets the monthly
  # monitor drill straight to a 5-digit product.
  pc_preselect <- reactiveVal(NULL)
  pe_preselect <- reactiveVal(NULL)
  pc_choice_fn <- function(lv) {
    build_sitc_level_choices(
      annual_sitc5() |> filter(year == current_complete_year()),
      annual_sitc1() |> filter(year == current_complete_year()),
      sitc_labels_all,
      lv
    )
  }
  pc_finder <- productFinderServer("pc_finder", pc_choice_fn, preselect = pc_preselect)

  # selectInput returns a character string; coerce once so year arithmetic
  # (e.g. previous-year comparisons) and joins behave as integers.
  monitor_year <- reactive({
    req(input$monitor_year)
    as.integer(input$monitor_year)
  })

  comparison_years <- reactive({
    req(length(input$comparison_years) > 0)
    as.integer(input$comparison_years)
  })


  observe({
    destination_choices <- current_top_destination_choices()
    updateSelectInput(
      session,
      "monitor_partner",
      label = "Country to watch",
      choices = destination_choices,
      selected = if (length(destination_choices) > 0) unname(destination_choices[[1]]) else character(0)
    )
    updateSelectInput(
      session,
      "country_product_partner",
      label = "Country",
      choices = destination_choices,
      selected = if (length(destination_choices) > 0) unname(destination_choices[[1]]) else character(0)
    )
  })

  monitor_month <- reactive({
    req(input$monitor_month)
    as.Date(input$monitor_month)
  })

  monitor_partner <- reactive({
    req(destination_data_available)
    req(input$monitor_partner)
    as.character(input$monitor_partner)
  })

  monitor_period_label <- reactive({
    format(monitor_month(), "%B %Y")
  })

  monitor_jump_basis <- reactive({
    req(input$monitor_jump_basis)
    input$monitor_jump_basis
  })


  monitor_jump_top_n <- reactive({
    req(input$monitor_jump_top_n)
    as.integer(input$monitor_jump_top_n)
  })

  monitor_jump_min_eur <- reactive({
    req(input$monitor_jump_min_bn)
    as.numeric(input$monitor_jump_min_bn) * 1e9
  })

  selected_monitor_partner_name <- reactive({
    req(monitor_partner())
    partner_display_value(monitor_partner())
  })

  selected_monitor_total <- reactive({
    monthly_total() |>
      filter(date == monitor_month())
  })

  selected_monitor_partner_total <- reactive({
    monthly_partner_total() |>
      filter(partner == monitor_partner(), date == monitor_month())
  })

  selected_monitor_partner_sitc1 <- reactive({
    req(destination_data_available)
    monthly_partner_sitc1() |>
      filter(partner == monitor_partner(), date == monitor_month()) |>
      arrange(desc(value_eur))
  })

  monitor_previous_month <- reactive({
    seq.Date(monitor_month(), by = "-1 month", length.out = 2)[2]
  })

  monitor_previous_year_month <- reactive({
    as.Date(sprintf(
      "%d-%02d-01",
      as.integer(format(monitor_month(), "%Y")) - 1L,
      as.integer(format(monitor_month(), "%m"))
    ))
  })

  monitor_partner_jump_data <- reactive({
    current_month <- monthly_partner_total() |>
      filter(date == monitor_month()) |>
      transmute(partner, current_value_eur = value_eur)

    previous_month <- monthly_partner_total() |>
      filter(date == monitor_previous_month()) |>
      transmute(partner, previous_month_value_eur = value_eur)

    previous_year <- monthly_partner_total() |>
      filter(date == monitor_previous_year_month()) |>
      transmute(partner, previous_year_value_eur = value_eur)

    current_month |>
      left_join(previous_month, by = "partner") |>
      left_join(previous_year, by = "partner") |>
      mutate(
        partner_display = vapply(partner, partner_display_value, character(1)),
        mom_change_eur = current_value_eur - coalesce(previous_month_value_eur, 0),
        yoy_change_eur = current_value_eur - coalesce(previous_year_value_eur, 0),
        mom_change_pct = if_else(
          is.na(previous_month_value_eur) | previous_month_value_eur == 0,
          NA_real_,
          current_value_eur / previous_month_value_eur - 1
        ),
        yoy_change_pct = if_else(
          is.na(previous_year_value_eur) | previous_year_value_eur == 0,
          NA_real_,
          current_value_eur / previous_year_value_eur - 1
        )
      ) |>
      arrange(desc(abs(yoy_change_eur)), desc(current_value_eur))
  })

  monitor_product_jump_data <- reactive({
    current_month <- monthly_sitc5() |>
      filter(date == monitor_month()) |>
      transmute(product_sitc5, product_label, current_value_eur = value_eur)

    previous_month <- monthly_sitc5() |>
      filter(date == monitor_previous_month()) |>
      transmute(product_sitc5, previous_month_value_eur = value_eur)

    previous_year <- monthly_sitc5() |>
      filter(date == monitor_previous_year_month()) |>
      transmute(product_sitc5, previous_year_value_eur = value_eur)

    current_month |>
      left_join(previous_month, by = "product_sitc5") |>
      left_join(previous_year, by = "product_sitc5") |>
      mutate(
        mom_change_eur = current_value_eur - coalesce(previous_month_value_eur, 0),
        yoy_change_eur = current_value_eur - coalesce(previous_year_value_eur, 0),
        mom_change_pct = if_else(
          is.na(previous_month_value_eur) | previous_month_value_eur == 0,
          NA_real_,
          current_value_eur / previous_month_value_eur - 1
        ),
        yoy_change_pct = if_else(
          is.na(previous_year_value_eur) | previous_year_value_eur == 0,
          NA_real_,
          current_value_eur / previous_year_value_eur - 1
        )
      ) |>
      arrange(desc(abs(yoy_change_eur)), desc(current_value_eur))
  })

  filtered_monitor_partner_jump_data <- reactive({
    rank_col <- if (identical(monitor_jump_basis(), "mom")) "mom_change_eur" else "yoy_change_eur"

    monitor_partner_jump_data() |>
      mutate(rank_change_eur = .data[[rank_col]]) |>
      filter(abs(rank_change_eur) >= monitor_jump_min_eur()) |>
      arrange(desc(abs(rank_change_eur)), desc(current_value_eur))
  })

  filtered_monitor_product_jump_data <- reactive({
    rank_col <- if (identical(monitor_jump_basis(), "mom")) "mom_change_eur" else "yoy_change_eur"

    monitor_product_jump_data() |>
      mutate(rank_change_eur = .data[[rank_col]]) |>
      filter(abs(rank_change_eur) >= monitor_jump_min_eur()) |>
      arrange(desc(abs(rank_change_eur)), desc(current_value_eur))
  })

  displayed_monitor_partner_jump_data <- reactive({
    filtered_monitor_partner_jump_data() |>
      slice_head(n = monitor_jump_top_n())
  })

  displayed_monitor_product_jump_data <- reactive({
    filtered_monitor_product_jump_data() |>
      slice_head(n = monitor_jump_top_n())
  })

  selected_monitor_partner_jump_row <- reactive({
    jump_data <- displayed_monitor_partner_jump_data()
    selected_row <- input$monitor_partner_jump_table_rows_selected

    if (nrow(jump_data) == 0) {
      return(NULL)
    }
    if (length(selected_row) == 1 && selected_row >= 1 && selected_row <= nrow(jump_data)) {
      return(selected_row)
    }
    1L
  })

  selected_monitor_partner_jump <- reactive({
    jump_data <- displayed_monitor_partner_jump_data()
    row_id <- selected_monitor_partner_jump_row()

    if (nrow(jump_data) == 0 || is.null(row_id)) {
      return(NULL)
    }

    jump_data[row_id, , drop = FALSE]
  })

  selected_monitor_product_jump_row <- reactive({
    jump_data <- displayed_monitor_product_jump_data()
    selected_row <- input$monitor_product_jump_table_rows_selected

    if (nrow(jump_data) == 0) {
      return(NULL)
    }
    if (length(selected_row) == 1 && selected_row >= 1 && selected_row <= nrow(jump_data)) {
      return(selected_row)
    }
    1L
  })

  selected_monitor_product_jump <- reactive({
    jump_data <- displayed_monitor_product_jump_data()
    row_id <- selected_monitor_product_jump_row()

    if (nrow(jump_data) == 0 || is.null(row_id)) {
      return(NULL)
    }

    jump_data[row_id, , drop = FALSE]
  })

  # Broad product mix (SITC 1-digit) for the "By country" tab, from bundled
  # data and driven by the single country/period selector shared with the
  # detailed drill-down below (no separate "destination" controls).
  cp_sitc1_period_data <- reactive({
    req(destination_data_available, country_product_partner())

    if (country_product_period_type() == "year") {
      req(input$country_product_year)
      annual_partner_sitc1() |>
        filter(partner == country_product_partner(), year == as.integer(input$country_product_year))
    } else {
      req(input$country_product_month)
      monthly_partner_sitc1() |>
        filter(partner == country_product_partner(), date == as.Date(input$country_product_month))
    }
  })

  # Country total and the matching all-Ireland total, used for the share card.
  cp_period_totals <- reactive({
    req(country_product_partner())

    if (country_product_period_type() == "year") {
      req(input$country_product_year)
      year_sel <- as.integer(input$country_product_year)
      list(
        country = annual_partner_total() |> filter(partner == country_product_partner(), year == year_sel),
        ireland = annual_total() |> filter(year == year_sel)
      )
    } else {
      req(input$country_product_month)
      date_sel <- as.Date(input$country_product_month)
      list(
        country = monthly_partner_total() |> filter(partner == country_product_partner(), date == date_sel),
        ireland = monthly_total() |> filter(date == date_sel)
      )
    }
  })

  country_product_partner <- reactive({
    req(destination_data_available)
    req(input$country_product_partner)
    as.character(input$country_product_partner)
  })

  country_product_period_type <- reactive({
    req(input$country_product_period_type)
    input$country_product_period_type
  })

  output$country_product_period_selector <- renderUI({
    validate(need(destination_data_available, "Run scripts/update_trade_data.R to populate partner-by-section data."))

    if (country_product_period_type() == "year") {
      selectInput(
        "country_product_year",
        "Year",
        choices = rev(annual_years),
        selected = current_complete_year()
      )
    } else {
      month_choices <- sort(unique(monthly_partner_total()$date), decreasing = TRUE)
      selectInput(
        "country_product_month",
        "Month",
        choices = stats::setNames(as.character(month_choices), format(month_choices, "%b %Y")),
        selected = as.character(max(monthly_partner_total()$date))
      )
    }
  })

  country_product_period_label <- reactive({
    if (country_product_period_type() == "year") {
      req(input$country_product_year)
      as.character(input$country_product_year)
    } else {
      req(input$country_product_month)
      format(as.Date(input$country_product_month), "%B %Y")
    }
  })

  selected_country_product_partner_name <- reactive({
    req(country_product_partner())
    partner_display_value(country_product_partner())
  })

  country_product_sitc5_cache <- new.env(parent = emptyenv())

  selected_country_product_sitc5_result <- reactive({
    req(country_product_partner())

    period_value <- if (country_product_period_type() == "year") {
      req(input$country_product_year)
      as.character(input$country_product_year)
    } else {
      req(input$country_product_month)
      as.character(input$country_product_month)
    }

    cache_key <- paste(trade_flow(), country_product_partner(), country_product_period_type(), period_value, sep = "|")
    if (exists(cache_key, envir = country_product_sitc5_cache, inherits = FALSE)) {
      return(get(cache_key, envir = country_product_sitc5_cache, inherits = FALSE))
    }

    result <- tryCatch({
      data <- fetch_partner_sitc5_period(
        partner = country_product_partner(),
        flow = trade_flow(),
        period_type = country_product_period_type(),
        period_value = period_value
      )
      list(
        data = prepare_sitc5(data, sitc5_labels),
        error = NULL
      )
    }, error = function(e) {
      list(
        data = NULL,
        error = conditionMessage(e)
      )
    })

    assign(cache_key, result, envir = country_product_sitc5_cache)
    result
  })

  selected_country_product_row <- reactive({
    result <- selected_country_product_sitc5_result()
    if (!is.null(result$error) && nzchar(result$error)) {
      return(NULL)
    }

    detail_data <- result$data
    selected_row <- input$country_product_sitc5_table_rows_selected

    if (is.null(detail_data) || nrow(detail_data) == 0) {
      return(NULL)
    }
    if (length(selected_row) == 1 && selected_row >= 1 && selected_row <= nrow(detail_data)) {
      return(selected_row)
    }
    non_total_row <- which(detail_data$product_sitc5 != "TOTAL")[1]
    if (is.na(non_total_row)) 1L else non_total_row
  })

  selected_country_product_code <- reactive({
    result <- selected_country_product_sitc5_result()
    row_id <- selected_country_product_row()

    if (is.null(result$data) || is.null(row_id)) {
      return(NULL)
    }

    as.character(result$data$product_sitc5[[row_id]])
  })

  selected_country_product_name <- reactive({
    result <- selected_country_product_sitc5_result()
    row_id <- selected_country_product_row()

    if (is.null(result$data) || is.null(row_id)) {
      return("No product selected")
    }

    as.character(result$data$product_label[[row_id]])
  })

  country_product_pair_cache <- new.env(parent = emptyenv())

  selected_country_product_pair_history <- reactive({
    product_code <- selected_country_product_code()
    req(product_code, country_product_partner())

    cache_key <- paste(trade_flow(), country_product_partner(), product_code, sep = "|")
    if (exists(cache_key, envir = country_product_pair_cache, inherits = FALSE)) {
      return(get(cache_key, envir = country_product_pair_cache, inherits = FALSE))
    }

    result <- tryCatch({
      product_total_lookup <- if (identical(product_code, "TOTAL")) {
        monthly_total() |>
          transmute(date, product_total_eur = value_eur)
      } else {
        monthly_sitc5() |>
          filter(product_sitc5 == product_code) |>
          transmute(date, product_total_eur = value_eur)
      }

      pair_history <- fetch_product_country_monthly(
        product_sitc5 = product_code,
        partner_code = country_product_partner(),
        flow = trade_flow()
      ) |>
        left_join(
          monthly_partner_total() |>
            filter(partner == country_product_partner()) |>
            transmute(date, partner_total_eur = value_eur),
          by = "date"
        ) |>
        left_join(
          product_total_lookup,
          by = "date"
        ) |>
        mutate(
          share_of_partner = if_else(
            is.na(partner_total_eur) | partner_total_eur == 0,
            NA_real_,
            value_eur / partner_total_eur
          ),
          share_of_product = if_else(
            is.na(product_total_eur) | product_total_eur == 0,
            NA_real_,
            value_eur / product_total_eur
          )
        ) |>
        arrange(date)

      list(data = pair_history, error = NULL)
    }, error = function(e) {
      list(data = NULL, error = conditionMessage(e))
    })

    assign(cache_key, result, envir = country_product_pair_cache)
    result
  })

  selected_country_product_pair_period <- reactive({
    result <- selected_country_product_pair_history()
    error_message <- if (is.null(result$error)) NULL else result$error
    validate(need(is.null(error_message) || !nzchar(error_message), error_message))

    pair_history <- result$data
    validate(need(!is.null(pair_history) && nrow(pair_history) > 0, "No pair-specific monthly history is available."))

    if (country_product_period_type() == "year") {
      req(input$country_product_year)
      pair_history |>
        filter(year == as.integer(input$country_product_year)) |>
        summarise(
          value_eur = sum(value_eur, na.rm = TRUE),
          partner_total_eur = sum(partner_total_eur, na.rm = TRUE),
          product_total_eur = sum(product_total_eur, na.rm = TRUE)
        ) |>
        mutate(
          share_of_partner = if_else(
            is.na(partner_total_eur) | partner_total_eur == 0,
            NA_real_,
            value_eur / partner_total_eur
          ),
          share_of_product = if_else(
            is.na(product_total_eur) | product_total_eur == 0,
            NA_real_,
            value_eur / product_total_eur
          )
        )
    } else {
      req(input$country_product_month)
      pair_history |>
        filter(date == as.Date(input$country_product_month)) |>
        summarise(
          value_eur = sum(value_eur, na.rm = TRUE),
          partner_total_eur = sum(partner_total_eur, na.rm = TRUE),
          product_total_eur = sum(product_total_eur, na.rm = TRUE)
        ) |>
        mutate(
          share_of_partner = if_else(
            is.na(partner_total_eur) | partner_total_eur == 0,
            NA_real_,
            value_eur / partner_total_eur
          ),
          share_of_product = if_else(
            is.na(product_total_eur) | product_total_eur == 0,
            NA_real_,
            value_eur / product_total_eur
          )
        )
    }
  })

  product_country_sitc5 <- reactive({
    s <- pc_finder()
    req(s)
    s$code
  })

  product_country_period_type <- reactive({
    req(input$product_country_period_type)
    input$product_country_period_type
  })

  output$product_country_period_selector <- renderUI({
    if (product_country_period_type() == "year") {
      selectInput(
        "product_country_year",
        "Year",
        choices = rev(annual_years),
        selected = current_complete_year()
      )
    } else {
      month_choices <- sort(unique(monthly_total()$date), decreasing = TRUE)
      selectInput(
        "product_country_month",
        "Month",
        choices = stats::setNames(as.character(month_choices), format(month_choices, "%b %Y")),
        selected = as.character(max(monthly_total()$date))
      )
    }
  })

  product_country_period_label <- reactive({
    if (product_country_period_type() == "year") {
      req(input$product_country_year)
      as.character(input$product_country_year)
    } else {
      req(input$product_country_month)
      format(as.Date(input$product_country_month), "%B %Y")
    }
  })

  selected_product_country_name <- reactive({
    s <- pc_finder()
    if (is.null(s)) "No product selected" else s$label
  })

  # Selected-item indicator + aggregate trend for the broad (1-4 digit) levels,
  # where the live country split is not available.
  output$pc_selected_indicator <- renderUI({
    s <- pc_finder()
    if (is.null(s)) return(NULL)
    tags$div(
      class = "pt-2",
      tags$span(class = "text-muted small", "Selected: "),
      tags$strong(s$label)
    )
  })

  output$pc_aggregate_header <- renderUI({
    s <- pc_finder()
    tags$span("Trade over time - ", if (is.null(s)) "" else s$label)
  })

  output$pc_aggregate_plot <- renderPlotly({
    s <- pc_finder()
    req(s)
    series <- build_sitc_level_monthly(monthly_sitc5(), monthly_sitc1(), s$code, s$level)
    validate(need(nrow(series) > 0, "No monthly data is available for the selected group."))

    p <- series |>
      ggplot(aes(x = date, y = value_eur)) +
      geom_line(color = trade_colors$primary, linewidth = 1) +
      geom_point(data = series |> slice_tail(n = 1), color = trade_colors$highlight, size = 2.4) +
      scale_y_continuous(labels = label_eur_billions) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
      labs(x = NULL, y = NULL)

    ggplotly(p, tooltip = c("x", "y")) |>
      layout(margin = list(l = 40, r = 20, t = 10, b = 40))
  })

  product_country_cache <- new.env(parent = emptyenv())

  selected_product_country_result <- reactive({
    period_value <- if (product_country_period_type() == "year") {
      req(input$product_country_year)
      as.character(input$product_country_year)
    } else {
      req(input$product_country_month)
      as.character(input$product_country_month)
    }

    cache_key <- paste(trade_flow(), product_country_sitc5(), product_country_period_type(), period_value, sep = "|")
    if (exists(cache_key, envir = product_country_cache, inherits = FALSE)) {
      return(get(cache_key, envir = product_country_cache, inherits = FALSE))
    }

    result <- tryCatch({
      data <- fetch_product_destinations_period(
        product_sitc5 = product_country_sitc5(),
        flow = trade_flow(),
        period_type = product_country_period_type(),
        period_value = period_value
      ) |>
        left_join(partner_display_lookup, by = "partner") |>
        mutate(
          partner_display = dplyr::coalesce(partner_display, partner)
        )

      list(data = data, error = NULL)
    }, error = function(e) {
      list(data = NULL, error = conditionMessage(e))
    })

    assign(cache_key, result, envir = product_country_cache)
    result
  })

  selected_product_country_partner_row <- reactive({
    result <- selected_product_country_result()
    if (!is.null(result$error) && nzchar(result$error)) {
      return(NULL)
    }

    detail_data <- result$data
    selected_row <- input$product_country_partner_table_rows_selected

    if (is.null(detail_data) || nrow(detail_data) == 0) {
      return(NULL)
    }
    if (length(selected_row) == 1 && selected_row >= 1 && selected_row <= nrow(detail_data)) {
      return(selected_row)
    }
    1L
  })

  selected_product_country_partner_code <- reactive({
    result <- selected_product_country_result()
    row_id <- selected_product_country_partner_row()

    if (is.null(result$data) || is.null(row_id)) {
      return(NULL)
    }

    as.character(result$data$partner[[row_id]])
  })

  selected_product_country_partner_name <- reactive({
    result <- selected_product_country_result()
    row_id <- selected_product_country_partner_row()

    if (is.null(result$data) || is.null(row_id)) {
      return("No partner selected")
    }

    as.character(result$data$partner_display[[row_id]])
  })

  observeEvent(selected_product_country_result(), {
    result <- selected_product_country_result()
    proxy <- dataTableProxy("product_country_partner_table")

    if (!is.null(result$error) && nzchar(result$error)) {
      selectRows(proxy, NULL)
    } else if (!is.null(result$data) && nrow(result$data) > 0) {
      selectRows(proxy, 1)
    } else {
      selectRows(proxy, NULL)
    }
  }, ignoreInit = FALSE)

  product_country_pair_cache <- new.env(parent = emptyenv())

  selected_product_country_pair_history <- reactive({
    partner_code <- selected_product_country_partner_code()
    req(partner_code)

    cache_key <- paste(trade_flow(), product_country_sitc5(), partner_code, sep = "|")
    if (exists(cache_key, envir = product_country_pair_cache, inherits = FALSE)) {
      return(get(cache_key, envir = product_country_pair_cache, inherits = FALSE))
    }

    result <- tryCatch({
      product_total_lookup <- if (identical(product_country_sitc5(), "TOTAL")) {
        monthly_total() |>
          transmute(date, product_total_eur = value_eur)
      } else {
        monthly_sitc5() |>
          filter(product_sitc5 == product_country_sitc5()) |>
          transmute(date, product_total_eur = value_eur)
      }

      pair_history <- fetch_product_country_monthly(
        product_sitc5 = product_country_sitc5(),
        partner_code = partner_code,
        flow = trade_flow()
      ) |>
        left_join(
          product_total_lookup,
          by = "date"
        ) |>
        left_join(
          monthly_partner_total() |>
            filter(partner == partner_code) |>
            transmute(date, destination_total_eur = value_eur),
          by = "date"
        ) |>
        mutate(
          share_of_product = if_else(
            is.na(product_total_eur) | product_total_eur == 0,
            NA_real_,
            value_eur / product_total_eur
          ),
          share_of_destination = if_else(
            is.na(destination_total_eur) | destination_total_eur == 0,
            NA_real_,
            value_eur / destination_total_eur
          )
        ) |>
        arrange(date)

      list(data = pair_history, error = NULL)
    }, error = function(e) {
      list(data = NULL, error = conditionMessage(e))
    })

    assign(cache_key, result, envir = product_country_pair_cache)
    result
  })

  selected_product_country_pair_period <- reactive({
    result <- selected_product_country_pair_history()
    error_message <- if (is.null(result$error)) NULL else result$error
    validate(need(is.null(error_message) || !nzchar(error_message), error_message))

    pair_history <- result$data
    validate(need(!is.null(pair_history) && nrow(pair_history) > 0, "No pair-specific monthly history is available."))

    if (product_country_period_type() == "year") {
      req(input$product_country_year)
      pair_history |>
        filter(year == as.integer(input$product_country_year)) |>
        summarise(
          value_eur = sum(value_eur, na.rm = TRUE),
          product_total_eur = sum(product_total_eur, na.rm = TRUE),
          destination_total_eur = sum(destination_total_eur, na.rm = TRUE)
        ) |>
        mutate(
          share_of_product = if_else(
            is.na(product_total_eur) | product_total_eur == 0,
            NA_real_,
            value_eur / product_total_eur
          ),
          share_of_destination = if_else(
            is.na(destination_total_eur) | destination_total_eur == 0,
            NA_real_,
            value_eur / destination_total_eur
          )
        )
    } else {
      req(input$product_country_month)
      pair_history |>
        filter(date == as.Date(input$product_country_month)) |>
        summarise(
          value_eur = sum(value_eur, na.rm = TRUE),
          product_total_eur = sum(product_total_eur, na.rm = TRUE),
          destination_total_eur = sum(destination_total_eur, na.rm = TRUE)
        ) |>
        mutate(
          share_of_product = if_else(
            is.na(product_total_eur) | product_total_eur == 0,
            NA_real_,
            value_eur / product_total_eur
          ),
          share_of_destination = if_else(
            is.na(destination_total_eur) | destination_total_eur == 0,
            NA_real_,
            value_eur / destination_total_eur
          )
        )
    }
  })

  current_year_total <- reactive({
    annual_total() |>
      filter(year == monitor_year())
  })

  current_year_ytd <- reactive({
    monthly_total() |>
      filter(year == monitor_year()) |>
      arrange(date) |>
      mutate(ytd_value_eur = cumsum(value_eur)) |>
      slice_tail(n = 1)
  })

  previous_year_ytd <- reactive({
    target_month <- current_year_ytd()$month

    monthly_total() |>
      filter(year == monitor_year() - 1, month <= target_month) |>
      arrange(date) |>
      mutate(ytd_value_eur = cumsum(value_eur)) |>
      slice_tail(n = 1)
  })

  selected_annual_sitc1 <- reactive({
    annual_sitc1() |>
      filter(year %in% comparison_years())
  })

  # Product over time finder (shares the choice function with Product by country).
  pe_finder <- productFinderServer("pe_finder", pc_choice_fn, preselect = pe_preselect)

  # Monthly value + shares for the selected item at any level: value from the
  # (truncated) 5-digit series, its share of the parent SITC 1-digit section,
  # and its share of total Irish trade. Generalises the former 5-digit-only
  # logic so the finder works at every level.
  pe_contributions <- reactive({
    s <- pe_finder()
    req(s)
    item <- build_sitc_level_monthly(monthly_sitc5(), monthly_sitc1(), s$code, s$level)
    validate(need(nrow(item) > 0, "No monthly data is available for the selected product."))

    section_code <- substr(s$code, 1, 1)
    section <- monthly_sitc1_raw() |>
      filter(as.character(sitc1) == section_code) |>
      transmute(date, sitc1_value_eur = value_eur)
    total <- monthly_total() |> transmute(date, total_value_eur = value_eur)

    item |>
      left_join(section, by = "date") |>
      left_join(total, by = "date") |>
      mutate(
        share_of_sitc1 = if_else(is.na(sitc1_value_eur) | sitc1_value_eur == 0, NA_real_, value_eur / sitc1_value_eur),
        share_of_total = if_else(is.na(total_value_eur) | total_value_eur == 0, NA_real_, value_eur / total_value_eur)
      ) |>
      arrange(date)
  })

  output$latest_total_card <- renderUI({
    if (is.null(input$monitor_year)) {
      return(metric_card("Full-year total", "...", "Loading"))
    }

    year_sel <- monitor_year()
    total_row <- current_year_total()
    is_complete <- year_sel <= current_complete_year()

    if (nrow(total_row) > 0 && is_complete) {
      metric_card(
        paste("Full-year total in", year_sel),
        label_eur_billions(total_row$value_eur),
        paste("Annual goods", tolower(trade_flow_label()), "summed from the monthly World series")
      )
    } else {
      current_row <- current_year_ytd()
      metric_card(
        paste("Year-to-date total in", year_sel),
        label_eur_billions(current_row$ytd_value_eur),
        paste("Cumulative", tolower(trade_flow_label()), "to", format(current_row$date, "%B"), "(year still in progress)")
      )
    }
  })

  output$latest_ytd_growth_card <- renderUI({
    if (is.null(input$monitor_year)) {
      return(metric_card("Latest cumulative growth", "...", "Loading"))
    }

    current_row <- current_year_ytd()
    previous_row <- previous_year_ytd()

    growth <- if (nrow(previous_row) == 0 || previous_row$ytd_value_eur == 0) {
      NA_real_
    } else {
      100 * (current_row$ytd_value_eur - previous_row$ytd_value_eur) / previous_row$ytd_value_eur
    }

    metric_card(
      "Latest cumulative growth",
      if (is.na(growth)) "n/a" else label_pct(growth),
      paste(
        "January to", format(current_row$date, "%B"),
        "versus the same period in", monitor_year() - 1
      )
    )
  })

  output$monthly_total_plot <- renderPlotly({
    d <- monthly_total() |> arrange(date) |> mutate(bn = value_eur / 1e9)
    latest <- d |> slice_tail(n = 1)

    plot_ly(d, x = ~date, y = ~bn) |>
      add_lines(
        line = list(color = trade_colors$primary, width = 2.4),
        fill = "tozeroy", fillcolor = "rgba(15, 76, 92, 0.08)",
        hovertemplate = "%{x|%b %Y}<br>€%{y:.1f}bn<extra></extra>"
      ) |>
      add_markers(
        data = latest, x = ~date, y = ~bn,
        marker = list(color = trade_colors$highlight, size = 9),
        hoverinfo = "skip", showlegend = FALSE
      ) |>
      layout(
        margin = list(l = 46, r = 24, t = 24, b = 28),
        hovermode = "x unified",
        xaxis = list(title = "", showgrid = FALSE, tickformat = "%Y", ticks = "outside",
                     tickcolor = trade_colors$grid, showline = FALSE),
        yaxis = list(title = "EUR bn", showgrid = TRUE, gridcolor = trade_colors$grid,
                     zeroline = FALSE, rangemode = "tozero"),
        annotations = list(list(
          x = latest$date, y = latest$bn,
          text = paste0(format(latest$date, "%b %Y"), ":  €", sprintf("%.1f", latest$bn), "bn"),
          showarrow = TRUE, arrowhead = 0, arrowcolor = trade_colors$highlight,
          ax = -46, ay = -30, xanchor = "right",
          font = list(color = trade_colors$highlight, size = 12)
        ))
      ) |>
      config(displayModeBar = FALSE)
  })

  # Trade by SITC 1-digit section as a table (value + share of total goods
  # trade), replacing the former bar chart. Sections are ordered by value and
  # the share denominator is the sum across sections (= total goods trade).
  output$overview_sitc1_table <- renderDT({
    section_data <- annual_sitc1() |>
      filter(year == current_complete_year()) |>
      arrange(desc(value_eur))
    denom <- sum(section_data$value_eur, na.rm = TRUE)

    table_data <- section_data |>
      mutate(share_of_total = if (denom == 0) NA_real_ else value_eur / denom) |>
      transmute(
        section = as.character(sitc1_label),
        value_eur = label_eur_billions(value_eur),
        share_of_total = label_share(share_of_total)
      )

    trade_datatable(table_data, rownames = FALSE, options = list(dom = "t", pageLength = 10))
  })

  output$top_partners_table <- renderDT({
    # Share is of total goods trade with the World for the year (the all-Ireland
    # annual total), so it reads as "this market's share of all Irish trade".
    denom <- annual_total() |>
      filter(year == current_complete_year()) |>
      summarise(value_eur = sum(value_eur, na.rm = TRUE)) |>
      pull(value_eur)

    table_data <- annual_partner_total() |>
      filter(year == current_complete_year()) |>
      slice_max(order_by = value_eur, n = 15, with_ties = FALSE) |>
      mutate(share_of_total = if (length(denom) == 0 || is.na(denom) || denom == 0) NA_real_ else value_eur / denom) |>
      transmute(
        partner = partner,
        value_eur = label_eur_billions(value_eur),
        share_of_total = label_share(share_of_total)
      )

    trade_datatable(table_data, rownames = FALSE, options = list(dom = "tip", pageLength = 15))
  })

  output$latest_months_table <- renderDT({
    table_data <- monthly_total() |>
      arrange(desc(date)) |>
      slice_head(n = 12) |>
      transmute(
        month = format(date, "%b %Y"),
        value_eur = scales::comma(value_eur)
      )

    trade_datatable(table_data, rownames = FALSE, options = list(dom = "tip", pageLength = 12))
  })

  output$contribution_plot <- renderPlotly({
    contribution_history <- ytd_contributions() |>
      filter(date >= as.Date("2015-01-01"), !is.na(total_ytd_growth_pct)) |>
      arrange(date, sitc1)

    contribution_totals <- contribution_history |>
      distinct(date, total_ytd_growth_pct) |>
      arrange(date)

    # Open on the most recent window; the range slider keeps full history.
    max_date <- max(contribution_history$date)
    default_start <- seq(max_date, by = paste0("-", default_zoom_months, " months"),
                         length.out = 2)[2]
    section_levels <- levels(monthly_sitc1()$sitc1_label)
    # Same palette (in the same section order) as every other multi-series
    # chart, so a given SITC section keeps one consistent colour app-wide.
    section_palette <- setNames(sitc1_section_palette[seq_along(section_levels)], section_levels)
    month_width_ms <- 25 * 24 * 60 * 60 * 1000

    p <- plot_ly()

    for (section_name in section_levels) {
      section_data <- contribution_history |>
        filter(as.character(sitc1_label) == section_name)

      if (nrow(section_data) == 0) {
        next
      }

      p <- p |>
        add_bars(
          data = section_data,
          x = ~date,
          y = ~contribution_pp,
          name = section_name,
          marker = list(color = unname(section_palette[[section_name]])),
          width = month_width_ms,
          hovertemplate = paste0(
            "<b>", section_name, "</b><br>",
            "%{x|%b %Y}<br>",
            "Contribution: %{y:.1f} pp",
            "<extra></extra>"
          )
        )
    }

    p |>
      add_lines(
        data = contribution_totals,
        x = ~date,
        y = ~total_ytd_growth_pct,
        name = "Total YTD growth",
        line = list(color = "#0b0c0c", width = 2),
        inherit = FALSE,
        hovertemplate = paste(
          "<b>Total YTD growth</b><br>",
          "%{x|%b %Y}<br>",
          "Growth: %{y:.1f}%",
          "<extra></extra>"
        )
      ) |>
      add_markers(
        data = contribution_totals,
        x = ~date,
        y = ~total_ytd_growth_pct,
        name = "Total YTD growth",
        marker = list(color = "#0b0c0c", size = 5),
        inherit = FALSE,
        showlegend = FALSE,
        hoverinfo = "skip"
      ) |>
      layout(
        barmode = "relative",
        margin = list(l = 60, r = 20, t = 60, b = 40),
        legend = list(
          orientation = "h",
          x = 0,
          xanchor = "left",
          y = 1.14,
          yanchor = "bottom"
        ),
        xaxis = list(
          title = "",
          type = "date",
          tickformat = "%b %Y",
          dtick = "M3",
          rangeslider = list(visible = TRUE),
          range = c(as.character(default_start), as.character(max_date))
        ),
        yaxis = list(
          title = "Contribution to YTD growth (pp)",
          tickformat = ".1f",
          ticksuffix = "%"
        ),
        shapes = list(list(
          type = "line",
          xref = "paper",
          x0 = 0,
          x1 = 1,
          y0 = 0,
          y1 = 0,
          line = list(color = "#9ca3af", width = 1)
        ))
      )
  })

  output$monthly_sitc1_plot <- renderPlotly({
    p <- monthly_sitc1() |>
      ggplot(aes(x = date, y = value_eur, color = sitc1_label)) +
      geom_line(linewidth = 0.9) +
      scale_y_continuous(labels = label_eur_billions) +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
      labs(x = NULL, y = NULL, color = NULL)

    ggplotly(p, tooltip = c("x", "y", "colour")) |>
      layout(margin = list(l = 40, r = 20, t = 10, b = 40), legend = list(orientation = "h", y = -0.25))
  })

  output$monitor_month_total_card <- renderUI({
    total_row <- selected_monitor_total()

    if (nrow(total_row) == 0) {
      return(metric_card("Ireland total", "n/a", "No total available for the selected month"))
    }

    metric_card(
      "Ireland total",
      label_eur_billions(total_row$value_eur[[1]]),
      paste(tolower(trade_flow_label()), "in", monitor_period_label())
    )
  })

  output$monitor_partner_total_card <- renderUI({
    partner_row <- selected_monitor_partner_total()

    if (nrow(partner_row) == 0) {
      return(metric_card("Selected country", "n/a", "No country total available for the selected month"))
    }

    metric_card(
      selected_monitor_partner_name(),
      label_eur_billions(partner_row$value_eur[[1]]),
      paste(tolower(trade_flow_label()), "in", monitor_period_label())
    )
  })

  output$monitor_partner_share_card <- renderUI({
    total_row <- selected_monitor_total()
    partner_row <- selected_monitor_partner_total()

    if (nrow(total_row) == 0 || nrow(partner_row) == 0 || total_row$value_eur[[1]] == 0) {
      return(metric_card("Country share", "n/a", "No comparable share for the selected month"))
    }

    metric_card(
      "Country share",
      label_share(partner_row$value_eur[[1]] / total_row$value_eur[[1]]),
      paste(selected_monitor_partner_name(), "in", monitor_period_label())
    )
  })

  output$monitor_partner_history_header <- renderUI({
    tags$span(selected_monitor_partner_name(), " - monthly trade over time")
  })

  output$monitor_partner_sitc1_header <- renderUI({
    tags$span(selected_monitor_partner_name(), " - broad product mix in ", monitor_period_label())
  })

  monitor_jump_basis_phrase <- reactive({
    if (identical(monitor_jump_basis(), "mom")) "month on month" else "year on year"
  })

  output$monitor_partner_jump_header <- renderUI({
    tags$span("Country moves · ", monitor_period_label())
  })

  output$monitor_product_jump_header <- renderUI({
    tags$span("Product moves · ", monitor_period_label())
  })

  output$monitor_partner_jump_note <- renderUI({
    tags$p(
      class = "text-muted small mb-3",
      "Country markets ranked by their ", tags$strong(monitor_jump_basis_phrase()),
      " change in ", monitor_period_label(), ". Moves smaller than €",
      label_eur_billions(monitor_jump_min_eur()), " are hidden."
    )
  })

  output$monitor_product_jump_note <- renderUI({
    tags$p(
      class = "text-muted small mb-3",
      "Detailed (5-digit) products ranked by their ", tags$strong(monitor_jump_basis_phrase()),
      " change in ", monitor_period_label(), ". Moves smaller than €",
      label_eur_billions(monitor_jump_min_eur()), " are hidden."
    )
  })

  output$monitor_partner_history_plot <- renderPlotly({
    history <- monthly_partner_total() |>
      filter(partner == monitor_partner()) |>
      arrange(date)

    validate(need(nrow(history) > 0, "No monthly history is available for the selected partner market."))

    highlight_row <- history |>
      filter(date == monitor_month())

    p <- history |>
      ggplot(aes(x = date, y = value_eur)) +
      geom_line(color = "#004d44", linewidth = 1) +
      geom_point(
        data = highlight_row,
        color = "#ae1122",
        size = 2.8
      ) +
      scale_y_continuous(labels = label_eur_billions) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
      labs(x = NULL, y = NULL)

    ggplotly(p, tooltip = c("x", "y")) |>
      layout(margin = list(l = 40, r = 20, t = 10, b = 40))
  })

  output$monitor_partner_sitc1_plot <- renderPlotly({
    plot_data <- selected_monitor_partner_sitc1() |>
      arrange(value_eur)

    validate(need(nrow(plot_data) > 0, "No partner-by-section data is available for the selected month and partner market."))

    p <- plot_data |>
      ggplot(aes(x = value_eur, y = forcats::fct_reorder(sitc1_label, value_eur))) +
      geom_col(fill = "#2c55a2") +
      scale_x_continuous(labels = label_eur_billions) +
      labs(x = NULL, y = NULL)

    ggplotly(p, tooltip = c("x", "y")) |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 40))
  })

  # Compact, basis-aware movers table: only the change for the chosen
  # comparison is shown (no horizontal scroll), with +/- changes colour-coded
  # and numbers right-aligned. Hidden columns drive the sort and the colour.
  jump_table_widget <- function(d, truncate_first = FALSE) {
    # Long product descriptions are truncated for display (full text on hover)
    # so rows stay one line and never push the drill-through button off-screen.
    col0_def <- if (truncate_first) {
      list(
        targets = 0, className = "dt-left",
        render = DT::JS(
          "function(data, type, row){",
          "  if(type === 'display' && data != null && data.length > 46){",
          "    return '<span title=\"' + String(data).replace(/\"/g,'&quot;') + '\">' +",
          "           String(data).substr(0,46) + '\\u2026</span>';",
          "  }",
          "  return data;",
          "}"
        )
      )
    } else {
      list(targets = 0, className = "dt-left")
    }

    DT::datatable(
      d,
      rownames = FALSE,
      selection = "single",
      class = "trade-dt row-border hover compact",
      # Column 0 of the product table uses a JS render returning a <span>, so it
      # must not be HTML-escaped. Content is trusted (app/Eurostat data).
      escape = !truncate_first,
      options = list(
        dom = "t",
        pageLength = monitor_jump_top_n(),
        ordering = TRUE,
        order = list(list(ncol(d) - 2, "desc")),
        columnDefs = list(
          list(targets = c(ncol(d) - 2, ncol(d) - 1), visible = FALSE, searchable = FALSE),
          list(targets = "_all", className = "dt-right"),
          col0_def
        )
      )
    ) |>
      DT::formatStyle(
        c("Change", "Change %"),
        valueColumns = ".sign",
        color = DT::styleInterval(0, c("#ae1122", "#2e7d6f")),
        fontWeight = "600"
      )
  }

  jump_basis_cols <- function(jump_data) {
    basis <- monitor_jump_basis()
    jump_data |>
      mutate(
        .chg = if (identical(basis, "mom")) mom_change_eur else yoy_change_eur,
        .pct = if (identical(basis, "mom")) mom_change_pct else yoy_change_pct,
        Latest = paste0("€", label_eur_billions(current_value_eur)),
        Change = paste0(if_else(.chg < 0, "-€", "+€"), label_eur_billions(abs(.chg))),
        `Change %` = if_else(is.na(.pct), "n/a",
                             paste0(if_else(.pct >= 0, "+", ""), label_change_pct(.pct)))
      )
  }

  output$monitor_partner_jump_table <- renderDT({
    jump_data <- displayed_monitor_partner_jump_data()
    validate(need(nrow(jump_data) > 0,
                  "No country moves meet the current settings. Try a smaller minimum change, or a different month."))

    d <- jump_data |>
      jump_basis_cols() |>
      transmute(
        Country = sub("^[A-Z0-9]+ - ", "", partner_display),
        `Latest value` = Latest, Change, `Change %`,
        .sort = abs(.chg), .sign = .chg
      )
    jump_table_widget(d)
  })

  output$monitor_product_jump_table <- renderDT({
    jump_data <- displayed_monitor_product_jump_data()
    validate(need(nrow(jump_data) > 0,
                  "No product moves meet the current settings. Try a smaller minimum change, or a different month."))

    d <- jump_data |>
      jump_basis_cols() |>
      transmute(
        Product = product_label,
        `Latest value` = Latest, Change, `Change %`,
        .sort = abs(.chg), .sign = .chg
      )
    jump_table_widget(d, truncate_first = TRUE)
  })

  observeEvent(displayed_monitor_partner_jump_data(), {
    jump_data <- displayed_monitor_partner_jump_data()
    proxy <- dataTableProxy("monitor_partner_jump_table")

    if (nrow(jump_data) > 0) {
      selectRows(proxy, 1)
    } else {
      selectRows(proxy, NULL)
    }
  }, ignoreInit = FALSE)

  observeEvent(displayed_monitor_product_jump_data(), {
    jump_data <- displayed_monitor_product_jump_data()
    proxy <- dataTableProxy("monitor_product_jump_table")

    if (nrow(jump_data) > 0) {
      selectRows(proxy, 1)
    } else {
      selectRows(proxy, NULL)
    }
  }, ignoreInit = FALSE)

  output$monitor_partner_jump_selection <- renderUI({
    selected_row <- selected_monitor_partner_jump()

    if (is.null(selected_row)) {
      return(tags$span(class = "text-muted small", "Select a partner row to drill into products."))
    }

    tags$span(
      class = "text-muted small",
      "Selected: ",
      selected_row$partner_display[[1]]
    )
  })

  output$monitor_product_jump_selection <- renderUI({
    selected_row <- selected_monitor_product_jump()

    if (is.null(selected_row)) {
      return(tags$span(class = "text-muted small", "Select a product row to drill into partner destinations."))
    }

    tags$span(
      class = "text-muted small",
      "Selected: ",
      selected_row$product_label[[1]]
    )
  })

  # ---- Drivers of a selected move (in-page decomposition) ----------------
  # Clicking a row button focuses the drivers panel below the movers tables on
  # that country (or product); the panel decomposes its change into the
  # products (or countries) that drove it - works for rises and falls alike.
  mover_focus <- reactiveVal(NULL)

  observeEvent(input$monitor_open_country_product, {
    row <- selected_monitor_partner_jump()
    req(!is.null(row))
    mom <- identical(monitor_jump_basis(), "mom")
    mover_focus(list(
      kind = "country", code = as.character(row$partner[[1]]), name = row$partner_display[[1]],
      change = if (mom) row$mom_change_eur[[1]] else row$yoy_change_eur[[1]],
      pct = if (mom) row$mom_change_pct[[1]] else row$yoy_change_pct[[1]]
    ))
  })

  observeEvent(input$monitor_open_product_country, {
    row <- selected_monitor_product_jump()
    req(!is.null(row))
    mom <- identical(monitor_jump_basis(), "mom")
    mover_focus(list(
      kind = "product", code = as.character(row$product_sitc5[[1]]), name = row$product_label[[1]],
      change = if (mom) row$mom_change_eur[[1]] else row$yoy_change_eur[[1]],
      pct = if (mom) row$mom_change_pct[[1]] else row$yoy_change_pct[[1]]
    ))
  })

  # Reset the focus when the month, flow or comparison basis changes, so the
  # panel never shows a stale decomposition for a different setting.
  observeEvent(list(monitor_month(), trade_flow(), monitor_jump_basis()), {
    mover_focus(NULL)
  }, ignoreInit = TRUE)

  mover_comparison_date <- reactive({
    if (identical(monitor_jump_basis(), "mom")) monitor_previous_month() else monitor_previous_year_month()
  })

  mover_drivers_cache <- new.env(parent = emptyenv())

  mover_drivers_data <- reactive({
    f <- mover_focus()
    req(f)
    cur <- as.character(monitor_month())
    cmp <- as.character(mover_comparison_date())
    key <- paste(f$kind, f$code, trade_flow(), cur, cmp, sep = "|")
    if (exists(key, envir = mover_drivers_cache, inherits = FALSE)) {
      return(get(key, envir = mover_drivers_cache, inherits = FALSE))
    }

    # Fast path: a pre-computed decomposition bundled by the refresh. Avoids any
    # live call online for the common case (latest month, top movers).
    bundle <- if (identical(f$kind, "country")) drivers_country_all else drivers_product_all
    bundled <- bundle |>
      filter(
        flow == trade_flow(),
        focus_code == f$code,
        basis == monitor_jump_basis(),
        as.Date(focus_month) == monitor_month()
      )
    if (nrow(bundled) > 0) {
      res <- list(
        data = bundled |>
          transmute(code = component_code, label = component_label,
                    cur = cur_eur, prev = prev_eur, delta = delta_eur, contrib_pp = contrib_pp) |>
          filter(delta != 0),
        error = NULL
      )
      assign(key, res, envir = mover_drivers_cache)
      return(res)
    }

    res <- tryCatch({
      # Two live Eurostat calls (selected month + comparison month). A progress
      # bar gives feedback; the result is cached per selection so repeat views
      # of the same country/product are instant.
      withProgress(message = "Fetching detail from Eurostat", detail = "selected month...", value = 0.15, {
        if (identical(f$kind, "country")) {
          curd <- fetch_partner_sitc5_period(f$code, trade_flow(), "month", cur) |>
            prepare_sitc5(sitc5_labels) |>
            filter(product_sitc5 != "TOTAL") |>
            transmute(code = as.character(product_sitc5), label = product_label, cur = value_eur)
          incProgress(0.45, detail = "comparison month...")
          cmpd <- fetch_partner_sitc5_period(f$code, trade_flow(), "month", cmp) |>
            filter(as.character(product_sitc5) != "TOTAL") |>
            transmute(code = as.character(product_sitc5), prev = value_eur)
        } else {
          curd <- fetch_product_destinations_period(f$code, trade_flow(), "month", cur) |>
            left_join(partner_display_lookup, by = "partner") |>
            transmute(code = as.character(partner), label = dplyr::coalesce(partner_display, partner), cur = value_eur)
          incProgress(0.45, detail = "comparison month...")
          cmpd <- fetch_product_destinations_period(f$code, trade_flow(), "month", cmp) |>
            transmute(code = as.character(partner), prev = value_eur)
        }
        incProgress(0.3, detail = "computing drivers...")
        merged <- full_join(curd, cmpd, by = "code") |>
          mutate(
            label = dplyr::coalesce(label, code),
            cur = dplyr::coalesce(cur, 0),
            prev = dplyr::coalesce(prev, 0),
            delta = cur - prev
          )
        # Comparison-period total (all components) is the base for the
        # percentage-point contributions, which sum to the entity's % change.
        base_prev <- sum(merged$prev, na.rm = TRUE)
        merged <- merged |>
          mutate(contrib_pp = if (base_prev > 0) 100 * delta / base_prev else NA_real_) |>
          filter(delta != 0)
        list(data = merged, error = NULL)
      })
    }, error = function(e) list(data = NULL, error = conditionMessage(e)))

    assign(key, res, envir = mover_drivers_cache)
    res
  })

  output$mover_drivers_header <- renderUI({
    f <- mover_focus()
    if (is.null(f)) return("What drove a move")
    label <- if (identical(f$kind, "country")) "Products driving " else "Countries driving "
    tags$span(label, sub("^[A-Z0-9]+ - ", "", f$name), " - ", monitor_period_label())
  })

  output$mover_drivers_headline <- renderUI({
    f <- mover_focus()
    if (is.null(f)) {
      return(tags$p(
        class = "text-muted mb-0",
        "Select a row in either table above and click its button to see what drove that move - each component's contribution to the year-on-year change, in percentage points."
      ))
    }
    basis_word <- if (identical(monitor_jump_basis(), "mom")) "month-on-month" else "year-on-year"
    cmp_label <- format(mover_comparison_date(), "%B %Y")
    dir <- if (f$change >= 0) "rose" else "fell"
    move_txt <- if (!is.null(f$pct) && !is.na(f$pct)) label_change_pct(abs(f$pct)) else paste0("€", label_trade_value(abs(f$change)))
    drove <- if (identical(f$kind, "country")) "product" else "country"
    clean_name <- sub("^[A-Z0-9]+ - ", "", f$name)
    tags$p(
      class = "mb-2",
      tags$strong(clean_name), " ", tolower(trade_flow_label()), " ", tags$strong(dir), " ",
      tags$strong(move_txt), " ", basis_word, " in ", monitor_period_label(), " (versus ", cmp_label,
      "). Each bar is a ", drove, "'s contribution to that change in percentage points; the bars sum to the total."
    )
  })

  output$mover_drivers_plot <- renderPlotly({
    f <- mover_focus()
    validate(need(!is.null(f), "Select a mover above and click its button to see what drove it."))
    res <- mover_drivers_data()
    validate(need(is.null(res$error) || !nzchar(res$error), res$error))
    d <- res$data
    validate(need(!is.null(d) && nrow(d) > 0, "No component detail is available for this move and period."))
    d <- d |> filter(!is.na(contrib_pp))
    validate(need(nrow(d) > 0, "A percentage breakdown is not available here (no comparable trade in the earlier period)."))

    truncate_lab <- function(x) ifelse(nchar(x) > 42, paste0(substr(x, 1, 40), "..."), x)

    d <- d |>
      arrange(desc(abs(contrib_pp))) |>
      head(10) |>
      arrange(contrib_pp) |>
      mutate(
        disp = truncate_lab(if (identical(f$kind, "country")) label else sub("^[A-Z0-9]+ - ", "", label)),
        lab = factor(disp, levels = disp),
        col = if_else(contrib_pp >= 0, trade_colors$primary, trade_colors$highlight),
        sign = if_else(contrib_pp >= 0, "+", "−"),
        txt = paste0(" ", sign, scales::number(abs(contrib_pp), accuracy = 0.1), " pp "),
        hov = paste0(as.character(lab), "<br>", sign, scales::number(abs(contrib_pp), accuracy = 0.1),
                     " pp  ·  ", sign, "€", label_trade_value(abs(delta)))
      )

    span <- max(abs(d$contrib_pp), na.rm = TRUE)
    plot_ly(
      x = d$contrib_pp, y = d$lab, type = "bar", orientation = "h",
      marker = list(color = d$col, line = list(width = 0)),
      text = d$txt, textposition = "outside", cliponaxis = FALSE,
      textfont = list(color = trade_colors$ink, size = 11.5),
      hovertext = d$hov, hoverinfo = "text"
    ) |>
      layout(
        margin = list(l = 6, r = 28, t = 6, b = 30),
        bargap = 0.32,
        xaxis = list(title = "Contribution to the change (percentage points)", zeroline = TRUE,
                     zerolinecolor = "#9ca3af", zerolinewidth = 1.5,
                     showgrid = FALSE, showticklabels = FALSE,
                     range = c(-span * 1.4, span * 1.4)),
        yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 12)),
        showlegend = FALSE
      ) |>
      config(displayModeBar = FALSE)
  })

  observeEvent(input$pc_open_product_over_time, {
    selected_product <- pc_finder()
    req(!is.null(selected_product))

    pe_preselect(selected_product$code)
    bslib::nav_select("main_tabs", selected = "Product over time", session = session)
  })

  observeEvent(input$pe_open_product_country, {
    selected_product <- pe_finder()
    req(!is.null(selected_product))

    pc_preselect(selected_product$code)
    bslib::nav_select("main_tabs", selected = "Product by country", session = session)
  })

  output$annual_comparison_plot <- renderPlotly({
    p <- selected_annual_sitc1() |>
      ggplot(aes(x = sitc1_label, y = value_eur, fill = factor(year))) +
      geom_col(position = position_dodge(width = 0.8)) +
      coord_flip() +
      scale_y_continuous(labels = label_eur_billions) +
      labs(x = NULL, y = NULL, fill = "Year")

    ggplotly(p, tooltip = c("x", "y", "fill")) |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 40))
  })

  output$annual_totals_table <- renderDT({
    table_data <- annual_total() |>
      filter(year %in% comparison_years()) |>
      transmute(
        year,
        value_eur = label_eur_billions(value_eur)
      )

    trade_datatable(table_data, rownames = FALSE, options = list(dom = "tip"))
  })

  output$annual_context_banner <- renderUI({
    years <- sort(comparison_years())
    card(
      class = "trade-monitor-card border-0 shadow-sm",
      fill = FALSE,
      card_body(
        tags$p(
          class = "text-muted mb-0",
          tags$strong("Comparing years: "),
          paste(years, collapse = ", "),
          ". Change these in the left sidebar. The detailed-products table below lists the leading 5-digit products for ",
          max(years), "."
        )
      )
    )
  })

  # ---- "By country" broad-mix outputs (merged from former Country view) ----
  output$cp_sitc1_header <- renderUI({
    tags$span("Broad product mix - ", country_product_period_label())
  })

  output$cp_history_header <- renderUI({
    tags$span("Monthly trade with ", selected_country_product_partner_name())
  })

  output$cp_share_card <- renderUI({
    if (!destination_data_available) {
      return(metric_card("Share of Irish trade", "n/a", "Refresh data to enable country analysis"))
    }
    totals <- cp_period_totals()
    country_row <- totals$country
    ireland_row <- totals$ireland
    if (nrow(country_row) == 0 || nrow(ireland_row) == 0 || ireland_row$value_eur[[1]] == 0) {
      return(metric_card("Share of Irish trade", "n/a", "No comparable total for the selected period"))
    }
    metric_card(
      "Share of Irish trade",
      label_share(country_row$value_eur[[1]] / ireland_row$value_eur[[1]]),
      paste(selected_country_product_partner_name(), "in", country_product_period_label())
    )
  })

  output$cp_top_section_card <- renderUI({
    if (!destination_data_available) {
      return(metric_card("Top product family", "n/a", "Refresh data to enable country analysis"))
    }
    section_row <- cp_sitc1_period_data() |>
      arrange(desc(value_eur)) |>
      slice_head(n = 1)
    if (nrow(section_row) == 0) {
      return(metric_card("Top product family", "n/a", "No section data for the selected period"))
    }
    metric_card(
      "Top product family",
      as.character(section_row$sitc1_label),
      label_trade_value(section_row$value_eur)
    )
  })

  output$cp_sitc1_plot <- renderPlotly({
    validate(need(destination_data_available, "Run scripts/update_trade_data.R to populate partner-by-section data."))
    plot_data <- cp_sitc1_period_data()
    validate(need(nrow(plot_data) > 0, "No partner-by-section data for the selected country and period."))
    ft_hbar(plot_data, "sitc1_label", "value_eur", accent = trade_colors$primary, top_n = 10)
  })

  output$cp_sitc1_table <- renderDT({
    validate(need(destination_data_available, "Run scripts/update_trade_data.R to populate partner-by-section data."))
    table_data <- cp_sitc1_period_data() |>
      arrange(desc(value_eur)) |>
      mutate(share_of_destination = value_eur / sum(value_eur, na.rm = TRUE)) |>
      transmute(
        sitc1 = as.character(sitc1),
        section = as.character(sitc1_label),
        value_eur = label_eur_billions(value_eur),
        share_of_destination = label_share(share_of_destination)
      )
    validate(need(nrow(table_data) > 0, "No partner-by-section data for the selected country and period."))
    trade_datatable(table_data, rownames = FALSE, options = list(dom = "tip", pageLength = 10))
  })

  output$cp_history_plot <- renderPlotly({
    validate(need(destination_data_available, "Run scripts/update_trade_data.R to populate partner data."))
    history <- monthly_partner_total() |>
      filter(partner == country_product_partner()) |>
      arrange(date)
    validate(need(nrow(history) > 0, "No monthly history for the selected country."))
    highlight_data <- if (country_product_period_type() == "year") {
      req(input$country_product_year)
      history |> filter(year == as.integer(input$country_product_year))
    } else {
      req(input$country_product_month)
      history |> filter(date == as.Date(input$country_product_month))
    }
    p <- history |>
      ggplot(aes(x = date, y = value_eur)) +
      geom_line(color = trade_colors$accent, linewidth = 1) +
      geom_point(data = highlight_data, color = trade_colors$highlight, size = 2.6) +
      scale_y_continuous(labels = label_eur_billions) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
      labs(x = NULL, y = NULL)
    ggplotly(p, tooltip = c("x", "y")) |>
      layout(margin = list(l = 40, r = 20, t = 10, b = 40))
  })

  output$country_product_view_banner <- renderUI({
    card(
      class = "trade-monitor-card border-0 shadow-sm",
      fill = FALSE,
      card_body(
        tags$p(
          class = "text-muted mb-0",
          tags$strong("Current view: "),
          paste0(
            trade_flow_label(),
            " | ",
            selected_country_product_partner_name(),
            " | ",
            if (country_product_period_type() == "year") "Year" else "Month",
            ": ",
            country_product_period_label()
          )
        )
      )
    )
  })

  output$country_product_table_header <- renderUI({
    tags$span("Detailed products in this country - ", country_product_period_label())
  })

  output$country_product_pair_header <- renderUI({
    tags$span("Selected product for ", selected_country_product_partner_name())
  })

  output$country_product_total_card <- renderUI({
    if (!destination_data_available) {
      return(metric_card("Trade with country", "n/a", "Refresh data to enable country analysis"))
    }

    totals <- if (country_product_period_type() == "year") {
      req(input$country_product_year)
      annual_partner_total() |>
        filter(partner == country_product_partner(), year == as.integer(input$country_product_year))
    } else {
      req(input$country_product_month)
      monthly_partner_total() |>
        filter(partner == country_product_partner(), date == as.Date(input$country_product_month))
    }

    if (nrow(totals) == 0) {
      return(metric_card("Trade with country", "n/a", "No data for the selected country and period"))
    }

    metric_card(
      "Trade with country",
      label_trade_value(totals$value_eur[[1]]),
      paste(selected_country_product_partner_name(), "in", country_product_period_label())
    )
  })

  output$country_product_top_product_card <- renderUI({
    result <- selected_country_product_sitc5_result()
    error_message <- if (is.null(result$error)) NULL else result$error
    if (!is.null(error_message) && nzchar(error_message)) {
      return(metric_card("Top detailed SITC product", "n/a", error_message))
    }

    top_row <- result$data |>
      filter(product_sitc5 != "TOTAL") |>
      arrange(desc(value_eur)) |>
      slice_head(n = 1)

    if (nrow(top_row) == 0) {
      return(metric_card("Top detailed SITC product", "n/a", "No detailed product data for the selected period"))
    }

    metric_card(
      "Top detailed SITC product",
      top_row$product_sitc5[[1]],
      top_row$product_label[[1]]
    )
  })

  output$country_product_sitc5_table <- renderDT({
    result <- selected_country_product_sitc5_result()
    error_message <- if (is.null(result$error)) NULL else result$error
    validate(need(
      is.null(error_message),
      if (is.null(error_message) || !nzchar(error_message)) "Could not load 5-digit partner detail." else error_message
    ))

    denominator <- result$data |>
      filter(product_sitc5 == "TOTAL") |>
      summarise(value_eur = sum(value_eur, na.rm = TRUE)) |>
      pull(value_eur)

    if (length(denominator) == 0 || is.na(denominator) || denominator == 0) {
      denominator <- result$data |>
        filter(product_sitc5 != "TOTAL") |>
        summarise(value_eur = sum(value_eur, na.rm = TRUE)) |>
        pull(value_eur)
    }

    table_data <- result$data |>
      arrange(desc(value_eur))

    table_data <- if (length(denominator) == 0 || is.na(denominator) || denominator == 0) {
      table_data |>
        mutate(share_of_partner = NA_real_)
    } else {
      table_data |>
        mutate(share_of_partner = value_eur / denominator)
    } |>
      transmute(
        sitc5 = product_sitc5,
        product = product_label,
        value_eur = label_trade_value(value_eur),
        share_of_partner = label_share(share_of_partner)
      )

    validate(need(nrow(table_data) > 0, "No 5-digit SITC detail is available for the selected partner and period."))

    trade_datatable(
      table_data,
      rownames = FALSE,
      selection = "single",
      options = list(dom = "tip", pageLength = 12)
    )
  })

  output$country_product_selected_product_card <- renderUI({
    product_name <- selected_country_product_name()
    if (identical(product_name, "No product selected")) {
      return(metric_card("Selected product", "n/a", "Choose a product row from the table"))
    }

    metric_card(
      "Selected product",
      selected_country_product_code(),
      product_name
    )
  })

  output$country_product_selected_value_card <- renderUI({
    pair_row <- selected_country_product_pair_period()
    validate(need(nrow(pair_row) > 0, "No pair-specific data for the selected period."))

    metric_card(
      "Trade in selected pair",
      label_trade_value(pair_row$value_eur[[1]]),
      paste(selected_country_product_name(), "with", selected_country_product_partner_name(), "in", country_product_period_label())
    )
  })

  output$country_product_selected_share_partner_card <- renderUI({
    pair_row <- selected_country_product_pair_period()
    validate(need(nrow(pair_row) > 0, "No pair-specific data for the selected period."))

    metric_card(
      "Share of partner total",
      if (is.na(pair_row$share_of_partner[[1]])) "n/a" else label_share(pair_row$share_of_partner[[1]]),
      paste(selected_country_product_name(), "within", selected_country_product_partner_name())
    )
  })

  output$country_product_selected_share_product_card <- renderUI({
    pair_row <- selected_country_product_pair_period()
    validate(need(nrow(pair_row) > 0, "No pair-specific data for the selected period."))

    metric_card(
      "Share of product total",
      if (is.na(pair_row$share_of_product[[1]])) "n/a" else label_share(pair_row$share_of_product[[1]]),
      paste(selected_country_product_partner_name(), "within", selected_country_product_name())
    )
  })

  output$country_product_history_plot <- renderPlotly({
    result <- selected_country_product_pair_history()
    error_message <- if (is.null(result$error)) NULL else result$error
    validate(need(is.null(error_message) || !nzchar(error_message), error_message))

    history <- result$data |>
      arrange(date)

    validate(need(nrow(history) > 0, "No monthly history is available for the selected country-product pair."))

    highlight_data <- if (country_product_period_type() == "year") {
      req(input$country_product_year)
      history |>
        filter(year == as.integer(input$country_product_year))
    } else {
      req(input$country_product_month)
      history |>
        filter(date == as.Date(input$country_product_month))
    }

    p <- history |>
      ggplot(aes(x = date, y = value_eur)) +
      geom_line(color = "#2c55a2", linewidth = 1) +
      geom_point(
        data = highlight_data,
        color = "#ae1122",
        size = 2.6
      ) +
      scale_y_continuous(labels = label_trade_value) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
      labs(x = NULL, y = NULL)

    ggplotly(p, tooltip = c("x", "y")) |>
      layout(margin = list(l = 40, r = 20, t = 10, b = 40))
  })

  output$country_product_share_plot <- renderPlotly({
    result <- selected_country_product_pair_history()
    error_message <- if (is.null(result$error)) NULL else result$error
    validate(need(is.null(error_message) || !nzchar(error_message), error_message))

    history <- result$data |>
      arrange(date)

    validate(need(nrow(history) > 0, "No monthly share history is available for the selected country-product pair."))

    plot_ly(history, x = ~date) |>
      add_lines(
        y = ~share_of_partner,
        name = "Share of partner total",
        line = list(color = "#004d44", width = 2),
        hovertemplate = "%{x|%b %Y}<br>Share of partner total: %{y:.1%}<extra></extra>"
      ) |>
      add_lines(
        y = ~share_of_product,
        name = "Share of product total",
        line = list(color = "#ae1122", width = 2),
        hovertemplate = "%{x|%b %Y}<br>Share of product total: %{y:.1%}<extra></extra>"
      ) |>
      layout(
        yaxis = list(tickformat = ".0%"),
        xaxis = list(title = NULL),
        margin = list(l = 40, r = 20, t = 10, b = 40),
        legend = list(orientation = "h", y = 1.1, x = 0)
      )
  })

  output$product_country_view_banner <- renderUI({
    card(
      class = "trade-monitor-card border-0 shadow-sm",
      fill = FALSE,
      card_body(
        tags$p(
          class = "text-muted mb-0",
          tags$strong("Current view: "),
          paste0(
            trade_flow_label(),
            " | ",
            selected_product_country_name(),
            " | ",
            if (product_country_period_type() == "year") "Year" else "Month",
            ": ",
            product_country_period_label()
          )
        )
      )
    )
  })

  output$product_country_table_header <- renderUI({
    tags$span("Country ranking - ", product_country_period_label())
  })

  output$product_country_pair_header <- renderUI({
    tags$span("Selected country for ", selected_product_country_name())
  })

  output$product_country_total_card <- renderUI({
    result <- selected_product_country_result()
    error_message <- if (is.null(result$error)) NULL else result$error
    if (!is.null(error_message) && nzchar(error_message)) {
      return(metric_card("Product trade", "n/a", error_message))
    }

    total_value <- sum(result$data$value_eur, na.rm = TRUE)

    metric_card(
      "Product trade",
      label_trade_value(total_value),
      paste(selected_product_country_name(), "in", product_country_period_label())
    )
  })

  output$product_country_top_partner_card <- renderUI({
    result <- selected_product_country_result()
    error_message <- if (is.null(result$error)) NULL else result$error
    if (!is.null(error_message) && nzchar(error_message)) {
      return(metric_card("Top country", "n/a", error_message))
    }

    top_row <- result$data |>
      arrange(desc(value_eur)) |>
      slice_head(n = 1)

    if (nrow(top_row) == 0) {
      return(metric_card("Top country", "n/a", "No country data for the selected period"))
    }

    metric_card(
      "Top country",
      top_row$partner[[1]],
      top_row$partner_display[[1]]
    )
  })

  output$product_country_top_partner_share_card <- renderUI({
    result <- selected_product_country_result()
    error_message <- if (is.null(result$error)) NULL else result$error
    if (!is.null(error_message) && nzchar(error_message)) {
      return(metric_card("Top country share", "n/a", error_message))
    }

    top_row <- result$data |>
      arrange(desc(value_eur)) |>
      slice_head(n = 1)

    total_value <- sum(result$data$value_eur, na.rm = TRUE)

    if (nrow(top_row) == 0 || total_value == 0) {
      return(metric_card("Top country share", "n/a", "No country data for the selected period"))
    }

    metric_card(
      "Top country share",
      label_share(top_row$value_eur[[1]] / total_value),
      paste(top_row$partner_display[[1]], "in", product_country_period_label())
    )
  })

  output$product_country_partner_plot <- renderPlotly({
    result <- selected_product_country_result()
    error_message <- if (is.null(result$error)) NULL else result$error
    validate(need(is.null(error_message) || !nzchar(error_message), error_message))

    plot_data <- result$data |>
      mutate(country = sub("^[A-Z0-9]+ - ", "", partner_display))

    validate(need(nrow(plot_data) > 0, "No partner-country data for the selected product and period."))

    ft_hbar(plot_data, "country", "value_eur", accent = trade_colors$primary, top_n = 12)
  })

  output$product_country_partner_table <- renderDT({
    result <- selected_product_country_result()
    error_message <- if (is.null(result$error)) NULL else result$error
    validate(need(is.null(error_message) || !nzchar(error_message), error_message))

    table_data <- result$data |>
      arrange(desc(value_eur)) |>
      mutate(
        share_of_product = value_eur / sum(value_eur, na.rm = TRUE),
        value_sort = value_eur,
        share_sort = share_of_product
      ) |>
      transmute(
        partner = partner,
        country = partner_display,
        value_eur = label_trade_value(value_eur),
        share_of_product = label_share(share_of_product),
        value_sort = value_sort,
        share_sort = share_sort
      )

    validate(need(nrow(table_data) > 0, "No partner-country data for the selected product and period."))

    trade_datatable(
      table_data,
      rownames = FALSE,
      selection = "single",
      options = list(
        dom = "tip",
        pageLength = 15,
        order = list(list(4, "desc")),
        columnDefs = list(
          list(targets = c(4, 5), visible = FALSE),
          list(orderData = 4, targets = 2),
          list(orderData = 5, targets = 3)
        )
      )
    )
  })

  output$product_country_selected_country_card <- renderUI({
    partner_name <- selected_product_country_partner_name()
    if (identical(partner_name, "No partner selected")) {
      return(metric_card("Selected country", "n/a", "Choose a country from the ranking table"))
    }

    metric_card(
      "Selected country",
      sub(" - .*", "", partner_name),
      partner_name
    )
  })

  output$product_country_selected_value_card <- renderUI({
    pair_row <- selected_product_country_pair_period()
    validate(need(nrow(pair_row) > 0, "No pair-specific data for the selected period."))

    metric_card(
      "Trade with selected country",
      label_trade_value(pair_row$value_eur[[1]]),
      paste(selected_product_country_partner_name(), "in", product_country_period_label())
    )
  })

  output$product_country_selected_share_product_card <- renderUI({
    pair_row <- selected_product_country_pair_period()
    validate(need(nrow(pair_row) > 0, "No pair-specific data for the selected period."))

    metric_card(
      "Share of product total",
      if (is.na(pair_row$share_of_product[[1]])) "n/a" else label_share(pair_row$share_of_product[[1]]),
      paste(selected_product_country_partner_name(), "within", selected_product_country_name())
    )
  })

  output$product_country_selected_share_destination_card <- renderUI({
    pair_row <- selected_product_country_pair_period()
    validate(need(nrow(pair_row) > 0, "No pair-specific data for the selected period."))

    metric_card(
      "Share of partner total",
      if (is.na(pair_row$share_of_destination[[1]])) "n/a" else label_share(pair_row$share_of_destination[[1]]),
      paste(selected_product_country_name(), "within", selected_product_country_partner_name())
    )
  })

  output$product_country_history_plot <- renderPlotly({
    result <- selected_product_country_pair_history()
    error_message <- if (is.null(result$error)) NULL else result$error
    validate(need(is.null(error_message) || !nzchar(error_message), error_message))

    history <- result$data |>
      arrange(date)

    validate(need(nrow(history) > 0, "No monthly history is available for the selected product-country pair."))

    highlight_data <- if (product_country_period_type() == "year") {
      req(input$product_country_year)
      history |>
        filter(year == as.integer(input$product_country_year))
    } else {
      req(input$product_country_month)
      history |>
        filter(date == as.Date(input$product_country_month))
    }

    p <- history |>
      ggplot(aes(x = date, y = value_eur)) +
      geom_line(color = "#2c55a2", linewidth = 1) +
      geom_point(
        data = highlight_data,
        color = "#ae1122",
        size = 2.6
      ) +
      scale_y_continuous(labels = label_trade_value) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
      labs(x = NULL, y = NULL)

    ggplotly(p, tooltip = c("x", "y")) |>
      layout(margin = list(l = 40, r = 20, t = 10, b = 40))
  })

  output$product_country_share_plot <- renderPlotly({
    result <- selected_product_country_pair_history()
    error_message <- if (is.null(result$error)) NULL else result$error
    validate(need(is.null(error_message) || !nzchar(error_message), error_message))

    history <- result$data |>
      arrange(date)

    validate(need(nrow(history) > 0, "No monthly share history is available for the selected product-country pair."))

    share_plot <- plot_ly(history, x = ~date)

    share_plot |>
      add_lines(
        y = ~share_of_product,
        name = "Share of product total",
        line = list(color = "#004d44", width = 2),
        hovertemplate = "%{x|%b %Y}<br>Share of product total: %{y:.1%}<extra></extra>"
      ) |>
      add_lines(
        y = ~share_of_destination,
        name = "Share of partner total",
        line = list(color = "#ae1122", width = 2),
        hovertemplate = "%{x|%b %Y}<br>Share of partner total: %{y:.1%}<extra></extra>"
      ) |>
      layout(
        yaxis = list(tickformat = ".0%"),
        xaxis = list(title = NULL),
        margin = list(l = 40, r = 20, t = 10, b = 40),
        legend = list(orientation = "h", y = 1.1, x = 0)
      )
  })

  # Annual comparison: leading detailed (5-digit) products for the latest
  # selected year, ranked by value with share of total. Searchable; no longer
  # tied to a sidebar product family.
  output$top_products_table <- renderDT({
    validate(need(
      sitc5_available,
      "Detailed 5-digit SITC data has not been fetched yet. Run scripts/update_trade_data.R to populate it."
    ))

    latest_selected_year <- max(comparison_years())
    year_data <- annual_sitc5() |> filter(year == latest_selected_year)
    denom <- sum(year_data$value_eur, na.rm = TRUE)

    table_data <- year_data |>
      slice_max(order_by = value_eur, n = 25, with_ties = FALSE) |>
      arrange(desc(value_eur)) |>
      mutate(share_of_total = if (denom > 0) value_eur / denom else NA_real_) |>
      transmute(
        sitc5 = product_sitc5,
        description = dplyr::coalesce(sitc5_desc, ""),
        value_eur = label_eur_billions(value_eur),
        share_of_total = label_share(share_of_total)
      )

    trade_datatable(table_data, rownames = FALSE, options = list(dom = "ftp", pageLength = 15))
  })

  # ---- Product explorer (finder-driven) ----------------------------------
  output$selected_product_card <- renderUI({
    if (!sitc5_available) {
      return(metric_card("Selected product", "n/a", "No detailed data yet"))
    }
    s <- pe_finder()
    if (is.null(s)) {
      return(metric_card("Selected product", "None selected", "Choose a product in the finder"))
    }
    desc <- if (grepl(" - ", s$label, fixed = TRUE)) {
      sub("^.*? - ", "", s$label)
    } else {
      as.character(sitc1_reference$sitc1_label[match(substr(s$code, 1, 1), sitc1_reference$sitc1)])
    }
    metric_card("Selected product", s$code, desc)
  })

  output$product_share_sitc1_card <- renderUI({
    s <- pe_finder()
    if (!sitc5_available || is.null(s)) {
      return(metric_card("Latest share of SITC 1-digit section", "n/a", "Select a product to load this measure"))
    }
    contribution_data <- pe_contributions() |> arrange(date) |> slice_tail(n = 1)
    req(nrow(contribution_data) > 0)
    metric_card(
      "Latest share of SITC 1-digit section",
      label_share(contribution_data$share_of_sitc1),
      paste(s$code, "as a share of SITC section", substr(s$code, 1, 1),
            "in", format(contribution_data$date, "%B %Y"))
    )
  })

  output$product_share_total_card <- renderUI({
    s <- pe_finder()
    if (!sitc5_available || is.null(s)) {
      return(metric_card("Latest share of total trade", "n/a", "Select a product to load this measure"))
    }
    contribution_data <- pe_contributions() |> arrange(date) |> slice_tail(n = 1)
    req(nrow(contribution_data) > 0)
    metric_card(
      "Latest share of total trade",
      label_share(contribution_data$share_of_total),
      paste(s$code, "as a share of overall trade in", format(contribution_data$date, "%B %Y"))
    )
  })

  output$pe_value_plot <- renderPlotly({
    validate(need(sitc5_available, "Run scripts/update_trade_data.R to populate the 5-digit SITC series."))
    validate(need(!is.null(pe_finder()), "Select a product in the finder first."))
    contribution_data <- pe_contributions()

    p <- contribution_data |>
      ggplot(aes(x = date, y = value_eur)) +
      geom_line(color = trade_colors$highlight, linewidth = 1) +
      geom_point(
        data = contribution_data |> arrange(date) |> slice_tail(n = 1),
        color = trade_colors$highlight, size = 2.5
      ) +
      scale_y_continuous(labels = label_eur_billions) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
      labs(x = NULL, y = NULL)

    ggplotly(p, tooltip = c("x", "y")) |>
      layout(margin = list(l = 40, r = 20, t = 10, b = 40))
  })

  output$pe_share_plot <- renderPlotly({
    validate(need(sitc5_available, "Run scripts/update_trade_data.R to populate the 5-digit SITC series."))
    validate(need(!is.null(pe_finder()), "Select a product in the finder first."))
    share_history <- pe_contributions() |>
      select(date, share_of_sitc1, share_of_total) |>
      pivot_longer(
        cols = c(share_of_sitc1, share_of_total),
        names_to = "share_type",
        values_to = "share_value"
      ) |>
      mutate(
        share_type = recode(
          share_type,
          share_of_sitc1 = "Share of parent SITC 1-digit section",
          share_of_total = "Share of total trade"
        )
      )

    p <- share_history |>
      ggplot(aes(x = date, y = share_value, color = share_type)) +
      geom_line(linewidth = 1) +
      scale_y_continuous(labels = label_share) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
      labs(x = NULL, y = NULL, color = NULL)

    ggplotly(p, tooltip = c("x", "y", "colour")) |>
      layout(margin = list(l = 40, r = 20, t = 10, b = 40), legend = list(orientation = "h", y = -0.25))
  })

}

shinyApp(ui, server)
