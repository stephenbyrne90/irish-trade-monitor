# Trends page module: "signal versus noise".
#
# One selected series seen four ways (level, growth rates, seasonal comparison),
# then the cross-section view (momentum heatmap, contributions to growth) and a
# diffusion index of breadth of growth across SITC 3-digit product groups.
#
# Contract
#   mod_trends_ui(id)                 -> tagList
#   mod_trends_server(id, td, flow)   -> td: analytics bundle (plain list),
#                                        flow: reactive returning "1" or "2".
#
# Every chart / table is built by a pure trends_* function taking (td, flow_code,
# ...), so it can be tested without Shiny. Shiny auto-sources app/R/*.R before
# library() calls: top level only defines functions and constants, everything
# inside functions is namespace-qualified (except the native pipe).

# ---------------------------------------------------------------------------
# Constants and small helpers
# ---------------------------------------------------------------------------

TRENDS_HEADLINE <- c(
  "u:total"       = "Total goods",
  "u:ex_pharma"   = "Total excluding pharmaceuticals",
  "u:ex_chem"     = "Total excluding chemicals & pharma",
  "u:ex_aircraft" = "Total excluding aircraft"
)

TRENDS_GROWTH_COLS <- c("3m" = "yoy3_pct", "m" = "yoy_pct", "12m" = "yoy12_pct")
TRENDS_BASIS_LABELS <- c("3m" = "3-month y/y", "m" = "Monthly y/y",
                         "12m" = "12-month y/y", "ytd" = "Year-to-date y/y")
TRENDS_BREAKDOWNS <- c("group" = "Analytical groups", "sitc" = "SITC sections",
                       "partner" = "Partner groups")
TRENDS_BREADTH_MIN_BASE <- 1e6

# Series choices as a grouped (optgroup) list for selectInput().
trends_series_choices <- function() {
  sec <- names(TM_SITC1_LABELS)
  list(
    "Headline" = stats::setNames(names(TRENDS_HEADLINE), unname(TRENDS_HEADLINE)),
    "Analytical groups" = stats::setNames(paste0("g:", TM_GROUP_LEVELS), TM_GROUP_LEVELS),
    "SITC sections" = stats::setNames(paste0("s:", sec),
                                      paste0(sec, " - ", unname(TM_SITC1_LABELS)))
  )
}

trends_series_ids <- function() unlist(trends_series_choices(), use.names = FALSE)

# Return `x` if it is a single allowed value, otherwise `default`.
trends_pick <- function(x, allowed, default) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !(as.character(x) %in% allowed)) default
  else as.character(x)
}

trends_flow_label <- function(flow_code) unname(TM_FLOW_LABELS[[as.character(flow_code)]])

trends_flow_colour <- function(flow_code) {
  if (identical(as.character(flow_code), "2")) TM_COL$exports else TM_COL$imports
}

# Date `k` months from first-of-month date `d` (k may be negative).
trends_shift_months <- function(d, k) {
  d <- as.Date(d)
  lt <- as.POSIXlt(d, tz = "UTC")
  idx <- (lt$year + 1900L) * 12L + lt$mon + as.integer(k)
  as.Date(sprintf("%04d-%02d-01", idx %/% 12L, idx %% 12L + 1L))
}

# Default x range (last `years` years, small right pad for bars) as character.
trends_window <- function(latest, years = 6) {
  c(as.character(trends_shift_months(latest, -12L * years)), as.character(as.Date(latest) + 16))
}

# Padded y range from finite values, always including `include`.
trends_yrange <- function(v, include = NULL, pad = 0.08) {
  v <- c(v[is.finite(v)], include)
  if (length(v) == 0) return(NULL)
  r <- range(v)
  span <- if (diff(r) > 0) diff(r) else max(abs(r), 1)
  c(r[1] - if (r[1] < 0 || is.null(include)) pad * span else 0, r[2] + pad * span)
}

# ---------------------------------------------------------------------------
# Series accessor and summary
# ---------------------------------------------------------------------------

#' Transform table for any choice in the Series selector.
#' @param td bundle. @param flow_code "1" or "2".
#' @param series_id "u:<measure>", "g:<group name>" or "s:<sitc1 digit>".
#' @return tibble, one row per month: date, value_eur, transforms, series_id,
#'   series_label.
trends_series <- function(td, flow_code, series_id) {
  flow_code <- as.character(flow_code)
  kind <- sub(":.*$", "", series_id)
  key <- sub("^[^:]*:", "", series_id)
  cols <- c("date", "value_eur", "sum3", "sum12", "ma3", "ma12", "yoy_pct", "yoy3_pct",
            "yoy12_pct", "mom3_pct", "ytd", "ytd_yoy_pct")
  if (kind == "u" && key %in% sub("^u:", "", names(TRENDS_HEADLINE))) {
    d <- td$underlying[as.character(td$underlying$flow) == flow_code &
                         as.character(td$underlying$measure) == key, , drop = FALSE]
    label <- unname(TRENDS_HEADLINE[[series_id]])
  } else if (kind == "g" && key %in% TM_GROUP_LEVELS) {
    d <- td$group_tf[as.character(td$group_tf$flow) == flow_code &
                       as.character(td$group_tf$group) == key, , drop = FALSE]
    label <- key
  } else if (kind == "s" && key %in% names(TM_SITC1_LABELS)) {
    d <- td$sitc1_tf[as.character(td$sitc1_tf$flow) == flow_code &
                       as.character(td$sitc1_tf$sitc1) == key, , drop = FALSE]
    label <- paste0(key, " - ", unname(TM_SITC1_LABELS[[key]]))
  } else {
    stop("Unknown series id: ", series_id, call. = FALSE)
  }
  d <- tibble::as_tibble(d[order(d$date), cols, drop = FALSE])
  d$series_id <- series_id
  d$series_label <- label
  d
}

#' One-sentence latest-month summary for the selected series and flow.
trends_summary <- function(td, flow_code, series_id) {
  d <- trends_series(td, flow_code, series_id)
  if (nrow(d) == 0) return("No data for this selection.")
  r <- d[which.max(d$date), ]
  paste0(trends_flow_label(flow_code), " of ", r$series_label, ", ", tm_fmt_month(r$date), ": ",
         tm_fmt_bn(r$value_eur), " in the month; 3-month y/y ", tm_fmt_pct(r$yoy3_pct),
         "; 12-month total ", tm_fmt_bn(r$sum12), ".")
}

# ---------------------------------------------------------------------------
# Chart 1: level
# ---------------------------------------------------------------------------

trends_chart_level <- function(td, flow_code, series_id) {
  d <- trends_series(td, flow_code, series_id)
  if (nrow(d) == 0) return(tm_empty_plot("No data"))
  col <- trends_flow_colour(flow_code)
  p <- tm_chart_trend(d, y = "value_eur", smooth = "ma3", colour = col)
  p <- plotly::add_trace(
    p, type = "scatter", mode = "lines", x = d$date, y = d$ma12 / 1e9,
    name = "12-month average", connectgaps = FALSE,
    line = list(color = TM_COL$ink, width = 1.5, dash = "dash"),
    hovertemplate = paste0(.TM_EURO, "%{y:,.1f}bn<extra>12-month average</extra>"))
  p <- tm_add_events(p)
  win <- trends_window(max(d$date))
  vis <- d$date >= as.Date(win[1])
  yr <- trends_yrange(c(d$value_eur[vis], d$ma3[vis]) / 1e9, include = 0, pad = 0.06)
  p <- plotly::layout(p, xaxis = list(range = as.list(win)))
  if (!is.null(yr)) p <- plotly::layout(p, yaxis = list(range = as.list(yr)))
  p
}

# ---------------------------------------------------------------------------
# Chart 2: growth rates
# ---------------------------------------------------------------------------

trends_chart_growth <- function(td, flow_code, series_id) {
  d <- trends_series(td, flow_code, series_id)
  if (nrow(d) == 0) return(tm_empty_plot("No data"))
  col <- trends_flow_colour(flow_code)
  hov <- function(nm) paste0("%{y:+.1f}%<extra>", nm, "</extra>")
  p <- plotly::plot_ly() |>
    plotly::add_trace(type = "scatter", mode = "lines", x = d$date, y = d$yoy_pct,
                      name = "Monthly y/y", connectgaps = FALSE,
                      line = list(color = .tm_rgba(col, 0.35), width = 1),
                      hovertemplate = hov("Monthly y/y")) |>
    plotly::add_trace(type = "scatter", mode = "lines", x = d$date, y = d$yoy3_pct,
                      name = "3-month y/y", connectgaps = FALSE,
                      line = list(color = col, width = 3),
                      hovertemplate = hov("3-month y/y")) |>
    plotly::add_trace(type = "scatter", mode = "lines", x = d$date, y = d$yoy12_pct,
                      name = "12-month y/y", connectgaps = FALSE,
                      line = list(color = TM_COL$ink, width = 1.8, dash = "dash"),
                      hovertemplate = hov("12-month y/y"))
  p <- tm_layout(p, y_title = "% change on a year earlier", y_suffix = "%",
                 zero_line = TRUE, range_buttons = TRUE)
  p <- plotly::layout(p, xaxis = list(hoverformat = "%b %Y"))
  p <- tm_add_events(p)
  win <- trends_window(max(d$date))
  vis <- d$date >= as.Date(win[1])
  yr <- trends_yrange(c(d$yoy3_pct[vis], d$yoy12_pct[vis]), include = 0, pad = 0.12)
  p <- plotly::layout(p, xaxis = list(range = as.list(win)))
  if (!is.null(yr)) p <- plotly::layout(p, yaxis = list(range = as.list(yr)))
  p
}

# ---------------------------------------------------------------------------
# Chart 3: this year against previous years
# ---------------------------------------------------------------------------

trends_chart_seasonal <- function(td, flow_code, series_id, cumulative = FALSE) {
  d <- trends_series(td, flow_code, series_id)
  if (nrow(d) == 0) return(tm_empty_plot("No data"))
  tm_chart_seasonal(d, value = "value_eur", cumulative = isTRUE(cumulative),
                    colour = trends_flow_colour(flow_code),
                    y_title = if (isTRUE(cumulative)) "EUR bn, cumulative" else "EUR bn")
}

# ---------------------------------------------------------------------------
# Chart 4: momentum heatmap
# ---------------------------------------------------------------------------

#' Long table (row, date, growth) for the heatmap: Total goods + ten groups.
trends_heatmap_data <- function(td, flow_code, basis = "3m", months = 36) {
  flow_code <- as.character(flow_code)
  basis <- trends_pick(basis, names(TRENDS_GROWTH_COLS), "3m")
  gcol <- TRENDS_GROWTH_COLS[[basis]]
  tot <- td$total_tf[as.character(td$total_tf$flow) == flow_code, , drop = FALSE]
  grp <- td$group_tf[as.character(td$group_tf$flow) == flow_code, , drop = FALSE]
  latest <- max(tot$date)
  from <- trends_shift_months(latest, -(as.integer(months) - 1L))
  out <- tibble::tibble(
    row = c(rep("Total goods", nrow(tot)), as.character(grp$group)),
    date = c(tot$date, grp$date),
    growth = c(tot[[gcol]], grp[[gcol]])
  )
  out <- out[out$date >= from & out$date <= latest, , drop = FALSE]
  out$row <- factor(out$row, levels = c("Total goods", TM_GROUP_LEVELS))
  out[order(out$row, out$date), , drop = FALSE]
}

trends_chart_heatmap <- function(td, flow_code, basis = "3m") {
  d <- trends_heatmap_data(td, flow_code, basis)
  if (nrow(d) == 0) return(tm_empty_plot("No data"))
  tm_chart_heatmap(d, x = "date", y = "row", z = "growth", cap = 50)
}

# ---------------------------------------------------------------------------
# Chart 5: contributions to growth
# ---------------------------------------------------------------------------

# Complete monthly grid (flow, component, date, value_eur) for a breakdown.
trends_breakdown_grid <- function(td, breakdown) {
  switch(breakdown,
    group = list(df = td$group_monthly, component = "group", palette = TM_GROUP_COLOURS),
    partner = list(df = td$partner_group_monthly, component = "partner_group",
                   palette = TM_PARTNER_GROUP_COLOURS),
    sitc = {
      s1 <- tm_complete_monthly(td$monthly_sitc1, by = c("flow", "section_short"),
                                value_cols = "value_eur")
      s1$section_short <- factor(s1$section_short, levels = unname(TM_SITC1_LABELS))
      list(df = s1, component = "section_short", palette = TM_SITC1_COLOURS)
    },
    stop("Unknown breakdown: ", breakdown, call. = FALSE)
  )
}

#' Contribution table for one flow (complete windows only).
#' @return tibble(flow, date, <component>, level, delta, contrib_pp, total_growth_pct)
trends_contrib_data <- function(td, flow_code, basis = "3m", breakdown = "group") {
  flow_code <- as.character(flow_code)
  basis <- trends_pick(basis, names(TRENDS_BASIS_LABELS), "3m")
  breakdown <- trends_pick(breakdown, names(TRENDS_BREAKDOWNS), "group")
  g <- trends_breakdown_grid(td, breakdown)
  df <- g$df[as.character(g$df$flow) == flow_code, , drop = FALSE]
  ct <- tm_contributions(df, component = g$component, basis = basis)
  ct <- ct[!is.na(ct$contrib_pp) & !is.na(ct$total_growth_pct), , drop = FALSE]
  attr(ct, "component") <- g$component
  attr(ct, "palette") <- g$palette
  ct
}

trends_chart_contrib <- function(td, flow_code, basis = "3m", breakdown = "group", months = 36) {
  ct <- trends_contrib_data(td, flow_code, basis, breakdown)
  if (nrow(ct) == 0) return(tm_empty_plot("No data"))
  comp <- attr(ct, "component")
  pal <- attr(ct, "palette")
  p <- tm_chart_contrib(ct, component = comp, palette = pal,
                        total_name = "Total growth (%)", y_title = "Percentage points")
  latest <- max(ct$date)
  from <- trends_shift_months(latest, -(as.integer(months) - 1L))
  win <- c(as.character(trends_shift_months(from, -1L)), as.character(latest + 16))
  vis <- ct[ct$date >= from, , drop = FALSE]
  pos <- tapply(pmax(vis$contrib_pp, 0), vis$date, sum)
  neg <- tapply(pmin(vis$contrib_pp, 0), vis$date, sum)
  tot <- vis$total_growth_pct
  yr <- trends_yrange(c(pos, neg, tot), include = 0, pad = 0.08)
  p <- plotly::layout(p, xaxis = list(range = as.list(win)))
  if (!is.null(yr)) p <- plotly::layout(p, yaxis = list(range = as.list(yr)))
  p
}

# ---------------------------------------------------------------------------
# Chart 6: breadth of growth (diffusion index)
# ---------------------------------------------------------------------------

#' Diffusion index per month for one flow over SITC 3-digit groups.
#' share_unweighted: % of groups whose 3-month total exceeds the same 3 months a
#' year earlier; share_weighted: same, weighted by the year-earlier 3-month
#' value. Only groups with a year-earlier 3-month value above EUR 1m count.
#' @param cache optional environment used to memoise the result per flow.
#' @return tibble(date, n_groups, share_unweighted, share_weighted)
trends_breadth <- function(td, flow_code, cache = NULL) {
  flow_code <- as.character(flow_code)
  key <- paste0("breadth_", flow_code)
  if (is.environment(cache) && !is.null(cache[[key]])) return(cache[[key]])
  s5 <- td$monthly_sitc5[as.character(td$monthly_sitc5$flow) == flow_code, , drop = FALSE]
  g <- tm_sitc_level_monthly(s5, 3)
  g <- tm_complete_monthly(g, by = c("flow", "code"), value_cols = "value_eur")
  g <- tm_add_transforms(g, by = c("flow", "code"))
  g <- dplyr::group_by(g, code)
  g <- dplyr::mutate(g, base3 = dplyr::lag(sum3, 12L))
  g <- dplyr::ungroup(g)
  g <- g[!is.na(g$sum3) & !is.na(g$base3) & g$base3 > TRENDS_BREADTH_MIN_BASE, , drop = FALSE]
  g$up <- g$sum3 > g$base3
  out <- g |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      n_groups = dplyr::n(),
      share_unweighted = 100 * mean(up),
      share_weighted = 100 * sum(base3[up]) / sum(base3),
      .groups = "drop"
    ) |>
    dplyr::arrange(date)
  if (is.environment(cache)) cache[[key]] <- out
  out
}

trends_chart_breadth <- function(td, flow_code, breadth = NULL) {
  b <- if (is.null(breadth)) trends_breadth(td, flow_code) else breadth
  if (nrow(b) == 0) return(tm_empty_plot("No data"))
  col <- trends_flow_colour(flow_code)
  hov <- function(nm) paste0("%{y:.0f}%<extra>", nm, "</extra>")
  p <- plotly::plot_ly() |>
    plotly::add_trace(type = "scatter", mode = "lines", x = b$date, y = b$share_unweighted,
                      name = "Share of groups (unweighted)", connectgaps = FALSE,
                      line = list(color = TM_COL$balance, width = 2, dash = "dot"),
                      hovertemplate = hov("Share of groups (unweighted)")) |>
    plotly::add_trace(type = "scatter", mode = "lines", x = b$date, y = b$share_weighted,
                      name = "Share of value (weighted)", connectgaps = FALSE,
                      line = list(color = col, width = 3),
                      hovertemplate = hov("Share of value (weighted)"))
  p <- tm_layout(p, y_title = "% of product groups growing y/y", y_suffix = "%",
                 range_buttons = TRUE)
  p <- plotly::layout(
    p,
    xaxis = list(hoverformat = "%b %Y", range = as.list(trends_window(max(b$date)))),
    yaxis = list(range = c(0, 100), dtick = 25),
    shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = 50, y1 = 50,
                       line = list(color = TM_COL$highlight, width = 1.2, dash = "dash"),
                       layer = "below"))
  )
  tm_add_events(p)
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

trends_css <- function() {
  htmltools::tags$style(htmltools::HTML("
.tm-trends-controls { display: flex; flex-wrap: wrap; align-items: center; gap: 0.6rem 1.5rem; }
.tm-trends-controls .form-group, .tm-trends-controls .shiny-input-container { margin-bottom: 0; }
.tm-trends-controls .tm-trends-series { flex: 0 0 auto; min-width: 280px; max-width: 360px; }
.tm-trends-summary { flex: 1 1 320px; color: #6b7280; font-size: 0.88rem; }
.tm-trends-page .tm-inline-controls { display: flex; flex-wrap: wrap; align-items: center; gap: 0.2rem 1.2rem; font-size: 0.82rem; margin: 0.1rem 0 0.2rem 0; }
.tm-trends-page .tm-inline-controls .form-group, .tm-trends-page .tm-inline-controls .shiny-input-container { margin-bottom: 0; width: auto; }
.tm-trends-page .tm-inline-controls .control-label { display: none; }
.tm-trends-page .tm-inline-controls .radio-inline, .tm-trends-page .tm-inline-controls .form-check-inline { margin-right: 0.9rem; }
.tm-trends-page .tm-trends-ctl-label { color: #6b7280; font-weight: 600; text-transform: uppercase; font-size: 0.7rem; letter-spacing: 0.04em; }
.tm-trends-page .tm-card { margin-bottom: 0; }
"))
}

trends_inline <- function(label, input) {
  htmltools::div(class = "tm-inline-controls",
                 htmltools::span(class = "tm-trends-ctl-label", label), input)
}

mod_trends_ui <- function(id) {
  ns <- shiny::NS(id)
  radio <- function(inputId, choices, selected) {
    shiny::radioButtons(ns(inputId), label = NULL, choices = choices, selected = selected, inline = TRUE)
  }
  title_ui <- function(name) shiny::uiOutput(ns(name), inline = TRUE)
  htmltools::tagList(
    tm_page_css(),
    trends_css(),
    htmltools::div(
      class = "tm-trends-page",
      bslib::card(
        class = "tm-card border-0 shadow-sm", fill = FALSE,
        bslib::card_body(
          class = "tm-trends-controls",
          htmltools::div(
            class = "tm-trends-series",
            shiny::selectInput(ns("series"), "Series", choices = trends_series_choices(),
                               selected = "u:total", width = "100%")
          ),
          htmltools::div(class = "tm-trends-summary", shiny::uiOutput(ns("summary"), inline = TRUE))
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6), fill = FALSE,
        tm_chart_card(
          title_ui("t_level"),
          "Faint bars: monthly values. Bold line: 3-month average. Dashed: 12-month average. Dotted lines mark events.",
          plotly::plotlyOutput(ns("level"), height = "340px")
        ),
        tm_chart_card(
          title_ui("t_growth"),
          paste0("Year-on-year % change: monthly (thin), 3-month (bold) and 12-month rolling (dashed). ",
                 "The vertical scale is set from the 3- and 12-month series, so single-month spikes can run ",
                 "off the chart; double-click the chart to autoscale and reveal them."),
          plotly::plotlyOutput(ns("growth"), height = "340px")
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6), fill = FALSE,
        tm_chart_card(
          title_ui("t_seasonal"),
          shiny::uiOutput(ns("sub_seasonal"), inline = TRUE),
          trends_inline("View", radio("seasonal_mode",
                                      c("Monthly" = "monthly", "Cumulative year-to-date" = "cumulative"),
                                      "monthly")),
          plotly::plotlyOutput(ns("seasonal"), height = "340px")
        ),
        tm_chart_card(
          title_ui("t_heatmap"),
          paste0("Year-on-year growth by analytical group over the last 36 months (colour capped at +/-50%). ",
                 "Cross-section view: it does not follow the Series selector."),
          trends_inline("Basis", radio("heat_basis",
                                       c("3-month" = "3m", "Monthly" = "m", "12-month" = "12m"), "3m")),
          plotly::plotlyOutput(ns("heatmap"), height = "340px")
        )
      ),
      tm_chart_card(
        title_ui("t_contrib"),
        paste0("Percentage-point contribution of each component to year-on-year growth in total goods; ",
               "bars sum to the black line. Cross-section view: it does not follow the Series selector."),
        trends_inline("Basis", radio("contrib_basis",
                                     c("3-month y/y" = "3m", "Monthly y/y" = "m",
                                       "12-month y/y" = "12m", "Year-to-date y/y" = "ytd"), "3m")),
        trends_inline("Breakdown", radio("contrib_breakdown",
                                         c("Analytical groups" = "group", "SITC sections" = "sitc",
                                           "Partner groups" = "partner"), "group")),
        plotly::plotlyOutput(ns("contrib"), height = "400px")
      ),
      tm_chart_card(
        title_ui("t_breadth"),
        paste0("Share of SITC 3-digit product groups whose 3-month total is above the same 3 months a year earlier ",
               "(groups with a year-earlier 3-month value above ", .TM_EURO, "1m only). Above 50% = growth is ",
               "broad-based; a high headline growth rate with breadth below 50% means a few products are doing the work. ",
               "Weighted line uses each group's year-earlier value as weight. Ignores the Series selector."),
        plotly::plotlyOutput(ns("breadth"), height = "340px")
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

mod_trends_server <- function(id, td, flow) {
  shiny::moduleServer(id, function(input, output, session) {
    breadth_cache <- new.env(parent = emptyenv())

    fl <- shiny::reactive(trends_pick(flow(), c("1", "2"), "2"))
    fl_lab <- shiny::reactive(trends_flow_label(fl()))
    sid <- shiny::reactive(trends_pick(input$series, trends_series_ids(), "u:total"))
    s_lab <- shiny::reactive(trends_series(td, fl(), sid())$series_label[1])
    cumulative <- shiny::reactive(identical(input$seasonal_mode, "cumulative"))
    heat_basis <- shiny::reactive(trends_pick(input$heat_basis, names(TRENDS_GROWTH_COLS), "3m"))
    c_basis <- shiny::reactive(trends_pick(input$contrib_basis, names(TRENDS_BASIS_LABELS), "3m"))
    c_break <- shiny::reactive(trends_pick(input$contrib_breakdown, names(TRENDS_BREAKDOWNS), "group"))
    breadth <- shiny::reactive(trends_breadth(td, fl(), cache = breadth_cache))

    # renderUI, not renderText: renderText round-trips through the native encoding,
    # which turns the euro sign into "<U+20AC>" when R runs in a non-UTF-8 locale.
    output$summary <- shiny::renderUI(htmltools::span(trends_summary(td, fl(), sid())))

    output$t_level <- shiny::renderUI(paste0(fl_lab(), " level: ", s_lab()))
    output$t_growth <- shiny::renderUI(paste0(fl_lab(), " growth rates: ", s_lab()))
    output$t_seasonal <- shiny::renderUI(
      paste0(fl_lab(), ": ", s_lab(), " this year against previous years"))
    output$sub_seasonal <- shiny::renderUI({
      yr <- max(trends_series(td, fl(), sid())$date)
      yr <- as.integer(format(yr, "%Y"))
      paste0(if (cumulative()) "Cumulative calendar-year total" else "Monthly value",
             ". Shaded band = range of the previous five years (", yr - 5, "-", yr - 1,
             "); dashed = their average; thin line = ", yr - 1, "; bold = ", yr, ".")
    })
    output$t_heatmap <- shiny::renderUI(
      paste0(fl_lab(), " momentum heatmap: ", tolower(TRENDS_BASIS_LABELS[[heat_basis()]]), " growth by group"))
    output$t_contrib <- shiny::renderUI(
      paste0(fl_lab(), ": contributions to growth in total goods (", tolower(TRENDS_BASIS_LABELS[[c_basis()]]),
             ", by ", tolower(TRENDS_BREAKDOWNS[[c_break()]]), ")"))
    output$t_breadth <- shiny::renderUI(paste0(fl_lab(), ": breadth of growth across SITC 3-digit product groups"))

    output$level <- plotly::renderPlotly(trends_chart_level(td, fl(), sid()))
    output$growth <- plotly::renderPlotly(trends_chart_growth(td, fl(), sid()))
    output$seasonal <- plotly::renderPlotly(trends_chart_seasonal(td, fl(), sid(), cumulative()))
    output$heatmap <- plotly::renderPlotly(trends_chart_heatmap(td, fl(), heat_basis()))
    output$contrib <- plotly::renderPlotly(trends_chart_contrib(td, fl(), c_basis(), c_break()))
    output$breadth <- plotly::renderPlotly(trends_chart_breadth(td, fl(), breadth()))
  })
}
