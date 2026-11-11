# Native plotly chart toolkit and small UI helpers for the redesigned Irish
# Trade Monitor. All charts use plotly::plot_ly (not ggplotly) and share one
# house style applied by tm_layout().
#
# Shiny auto-sources app/R/*.R before app.R runs its library() calls, so top-level
# code only defines functions and plain constants; everything inside functions is
# namespace-qualified (except the native pipe). TM_EVENTS / TM_GROUP_COLOURS etc.
# come from analytics.R and are only touched when functions run.

# ---------------------------------------------------------------------------
# Constants and internal helpers
# ---------------------------------------------------------------------------

TM_COL <- list(
  exports = "#0f4c5c", imports = "#bb3e03", balance = "#52606d",
  ink = "#1f2933", muted = "#6b7280", grid = "#e2e8ee", highlight = "#9a031e",
  positive = "#0f4c5c", negative = "#bb3e03", band = "#d9e2ec"
)

.TM_EURO <- intToUtf8(0x20AC)
.TM_FONT <- "Source Sans 3, Source Sans Pro, Helvetica Neue, Arial, sans-serif"
.TM_DEFAULT_PALETTE <- c("#0f4c5c", "#e09f3e", "#9e2a2b", "#335c67", "#7b6d8d",
                         "#4f772d", "#8d99ae", "#bc6c25", "#2a9d8f", "#6b705c")
.TM_SOURCE_NOTE <- "Source: Eurostat Comext (DS-059331), reporter Ireland. Nominal EUR."

.tm_is_date <- function(x) inherits(x, c("Date", "POSIXct"))

# Series levels: factor levels (those present) or order of appearance.
.tm_levels <- function(x) {
  if (is.factor(x)) levels(droplevels(x)) else unique(as.character(x))
}

# Resolve a colour for each level from a (possibly named / unnamed) palette.
.tm_palette_for <- function(levels, palette = NULL) {
  n <- length(levels)
  out <- stats::setNames(rep(NA_character_, n), levels)
  if (!is.null(palette)) {
    if (!is.null(names(palette))) {
      hit <- intersect(levels, names(palette))
      out[hit] <- palette[hit]
    } else {
      out[] <- rep_len(palette, n)
    }
  }
  miss <- is.na(out)
  if (any(miss)) out[miss] <- rep_len(.TM_DEFAULT_PALETTE, n)[miss]
  out
}

# d3 hover format and axis divisor for a scale of 1e9 (bn) / 1e6 (m) / other.
.tm_value_hover <- function(scale, prefix = .TM_EURO) {
  if (isTRUE(all.equal(scale, 1e9))) paste0(prefix, "%{y:,.1f}bn")
  else if (isTRUE(all.equal(scale, 1e6))) paste0(prefix, "%{y:,.0f}m")
  else "%{y:,.2f}"
}

# Convert hex colour to rgba() string.
.tm_rgba <- function(hex, alpha) {
  v <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], alpha)
}

# Effective shapes/annotations already stored in a plotly object's layout.
.tm_existing_layout <- function(p, key) {
  out <- p$x$layout[[key]]
  for (a in p$x$layoutAttrs) {
    if (!is.null(a[[key]])) out <- a[[key]]
  }
  out
}

# Min / max of the x values of a plot's traces (Dates), or NULL.
.tm_x_range <- function(p) {
  b <- tryCatch(plotly::plotly_build(p), error = function(e) NULL)
  if (is.null(b)) return(NULL)
  xs <- unlist(lapply(b$x$data, function(tr) {
    x <- tr$x
    if (is.null(x) || is.list(x)) return(NULL)
    d <- suppressWarnings(tryCatch(as.Date(x), error = function(e) NULL))
    if (is.null(d)) return(NULL)
    as.numeric(d)
  }))
  xs <- xs[is.finite(xs)]
  if (length(xs) == 0) return(NULL)
  as.Date(range(xs), origin = "1970-01-01")
}

# ---------------------------------------------------------------------------
# Layout and annotations
# ---------------------------------------------------------------------------

#' Apply the house style to a plotly chart.
#' White background, Source Sans font, light horizontal gridlines only, a zero
#' line when requested, compact margins, bottom legend and a modebar reduced to
#' a "download PNG" button.
#' @param p plotly object.
#' @param y_title,y_suffix,y_prefix,y_tickformat y-axis title and tick formatting.
#' @param legend "bottom", "none" or "right".
#' @param zero_line draw a darker line at y = 0.
#' @param range_buttons add 2Y / 5Y / 10Y / All range-selector buttons (date x axis).
#' @param x_range optional length-2 range for the x axis.
#' @param hovermode plotly hovermode.
#' @return plotly object.
tm_layout <- function(p, y_title = NULL, y_suffix = "", y_prefix = "", y_tickformat = NULL,
                      legend = c("bottom", "none", "right"), zero_line = FALSE,
                      range_buttons = FALSE, x_range = NULL, hovermode = "x unified") {
  legend <- match.arg(legend)
  xaxis <- list(
    title = list(text = ""), showgrid = FALSE, zeroline = FALSE, showline = FALSE,
    ticks = "outside", tickcolor = "#cbd2d9", ticklen = 3, automargin = TRUE,
    tickfont = list(size = 11, color = TM_COL$muted)
  )
  if (!is.null(x_range)) xaxis$range <- as.list(as.character(x_range))
  if (range_buttons) {
    xaxis$rangeselector <- list(
      buttons = list(
        list(count = 2, label = "2Y", step = "year", stepmode = "backward"),
        list(count = 5, label = "5Y", step = "year", stepmode = "backward"),
        list(count = 10, label = "10Y", step = "year", stepmode = "backward"),
        list(step = "all", label = "All")
      ),
      x = 0, y = 1.02, xanchor = "left", yanchor = "bottom",
      font = list(size = 11, color = TM_COL$muted),
      bgcolor = "#f1f4f7", activecolor = TM_COL$band, borderwidth = 0
    )
  }
  yaxis <- list(
    title = list(text = if (is.null(y_title)) "" else y_title,
                 font = list(size = 11, color = TM_COL$muted)),
    showgrid = TRUE, gridcolor = TM_COL$grid, gridwidth = 1,
    zeroline = isTRUE(zero_line), zerolinecolor = "#52606d", zerolinewidth = 1.4,
    showline = FALSE, ticks = "", automargin = TRUE,
    ticksuffix = y_suffix, tickprefix = y_prefix,
    tickfont = list(size = 11, color = TM_COL$muted)
  )
  if (!is.null(y_tickformat)) yaxis$tickformat <- y_tickformat
  leg <- switch(legend,
    bottom = list(orientation = "h", x = 0, xanchor = "left", y = -0.12, yanchor = "top",
                  font = list(size = 11), title = list(text = "")),
    right = list(orientation = "v", x = 1.02, xanchor = "left", y = 1, yanchor = "top",
                 font = list(size = 11), title = list(text = "")),
    none = list()
  )
  p |>
    plotly::layout(
      font = list(family = .TM_FONT, color = TM_COL$ink, size = 12),
      paper_bgcolor = "white", plot_bgcolor = "white",
      margin = list(l = 40, r = 12, t = if (range_buttons) 36 else 12,
                    b = if (legend == "bottom") 48 else 28, pad = 2),
      hovermode = hovermode,
      hoverlabel = list(font = list(family = .TM_FONT, size = 12)),
      showlegend = legend != "none",
      legend = leg,
      xaxis = xaxis,
      yaxis = yaxis
    ) |>
    plotly::config(
      displaylogo = FALSE, responsive = TRUE,
      modeBarButtons = list(list("toImage")),
      toImageButtonOptions = list(format = "png", filename = "irish-trade-monitor", scale = 2)
    )
}

#' Add dotted vertical event lines with short labels.
#' Existing shapes and annotations in the layout are preserved. Only events
#' within [x_min, x_max] are drawn; if omitted the range is taken from the
#' plot's own date x data.
#' @param p plotly object with a date x axis.
#' @param events tibble(date, label, short); default TM_EVENTS.
#' @param x_min,x_max optional Date bounds.
#' @return plotly object.
tm_add_events <- function(p, events = TM_EVENTS, x_min = NULL, x_max = NULL) {
  if (is.null(x_min) || is.null(x_max)) {
    rng <- .tm_x_range(p)
    if (is.null(x_min) && !is.null(rng)) x_min <- rng[1]
    if (is.null(x_max) && !is.null(rng)) x_max <- rng[2]
  }
  ev <- events
  if (!is.null(x_min)) ev <- ev[ev$date >= as.Date(x_min), ]
  if (!is.null(x_max)) ev <- ev[ev$date <= as.Date(x_max), ]
  if (nrow(ev) == 0) return(p)
  shapes <- lapply(seq_len(nrow(ev)), function(i) {
    list(type = "line", xref = "x", yref = "paper",
         x0 = as.character(ev$date[i]), x1 = as.character(ev$date[i]), y0 = 0, y1 = 1,
         line = list(color = "#9aa5b1", width = 1, dash = "dot"), layer = "below")
  })
  annots <- lapply(seq_len(nrow(ev)), function(i) {
    list(x = as.character(ev$date[i]), y = 1, xref = "x", yref = "paper",
         text = ev$short[i], showarrow = FALSE, textangle = -90,
         xanchor = "right", yanchor = "top", xshift = -1,
         font = list(size = 10, color = TM_COL$muted))
  })
  shapes <- c(.tm_existing_layout(p, "shapes"), shapes)
  annots <- c(.tm_existing_layout(p, "annotations"), annots)
  # plotly merges repeated layout() calls element-wise, so drop the old arrays
  # (already folded into `shapes` / `annots`) before setting the combined ones.
  p$x$layoutAttrs <- lapply(p$x$layoutAttrs, function(a) {
    a[c("shapes", "annotations")] <- NULL
    a
  })
  p$x$layout[c("shapes", "annotations")] <- NULL
  plotly::layout(p, shapes = shapes, annotations = annots)
}

# ---------------------------------------------------------------------------
# Time series charts
# ---------------------------------------------------------------------------

#' Faint raw monthly series plus a bold smoothed line.
#' @param df data with `date`, the raw column `y` and the smoothed column `smooth`.
#' @param y,smooth column names. @param name series name prefix.
#' @param colour line colour. @param scale divisor applied to values.
#' @param y_title axis title. @param raw_as "bars" or "line".
#' @param smooth_label legend label of the smoothed line.
#' @return plotly object.
tm_chart_trend <- function(df, y = "value_eur", smooth = "ma3", name = "", colour = TM_COL$exports,
                           scale = 1e9, y_title = "EUR bn", raw_as = c("bars", "line"),
                           smooth_label = "3-month average") {
  raw_as <- match.arg(raw_as)
  hov <- .tm_value_hover(scale)
  pre <- if (nzchar(name)) paste0(name, " ") else ""
  p <- plotly::plot_ly()
  if (raw_as == "bars") {
    p <- plotly::add_trace(p, type = "bar", x = df$date, y = df[[y]] / scale,
                           name = paste0(pre, "monthly"),
                           marker = list(color = .tm_rgba(colour, 0.35)),
                           hovertemplate = paste0(hov, "<extra>", pre, "monthly</extra>"))
  } else {
    p <- plotly::add_trace(p, type = "scatter", mode = "lines", x = df$date, y = df[[y]] / scale,
                           name = paste0(pre, "monthly"),
                           line = list(color = .tm_rgba(colour, 0.4), width = 1),
                           hovertemplate = paste0(hov, "<extra>", pre, "monthly</extra>"))
  }
  p <- plotly::add_trace(p, type = "scatter", mode = "lines", x = df$date, y = df[[smooth]] / scale,
                         name = paste0(pre, smooth_label), connectgaps = FALSE,
                         line = list(color = colour, width = 3),
                         hovertemplate = paste0(hov, "<extra>", pre, smooth_label, "</extra>"))
  p |>
    tm_layout(y_title = y_title, range_buttons = TRUE) |>
    plotly::layout(xaxis = list(hoverformat = "%b %Y"), bargap = 0.15)
}

#' Multi-line chart, one line per `colour_by` level.
#' `highlight` draws the named series bold and the rest thin grey. `dash_by`
#' splits each colour into further traces with different dash styles.
#' @param df data. @param x,y column names. @param colour_by series column.
#' @param palette optional named/unnamed colours. @param scale divisor for y.
#' @param dash_by optional column mapped to dash style. @param highlight series to emphasise.
#' @param y_title,y_suffix axis labelling. @param width line width.
#' @return plotly object.
tm_chart_lines <- function(df, x = "date", y, colour_by, palette = NULL, scale = 1, dash_by = NULL,
                           highlight = NULL, y_title = NULL, y_suffix = "", width = 2.2) {
  lv <- .tm_levels(df[[colour_by]])
  cols <- .tm_palette_for(lv, palette)
  dashes <- c("solid", "dash", "dot", "dashdot")
  dlv <- if (!is.null(dash_by)) .tm_levels(df[[dash_by]]) else NA_character_
  p <- plotly::plot_ly()
  for (s in lv) {
    for (k in seq_along(dlv)) {
      sel <- as.character(df[[colour_by]]) == s
      if (!is.null(dash_by)) sel <- sel & as.character(df[[dash_by]]) == dlv[k]
      d <- df[sel, , drop = FALSE]
      if (nrow(d) == 0) next
      d <- d[order(d[[x]]), , drop = FALSE]
      nm <- if (is.null(dash_by)) s else paste(s, dlv[k], sep = paste0(" ", intToUtf8(0xB7), " "))
      is_hi <- is.null(highlight) || s %in% highlight
      col <- if (is_hi) cols[[s]] else "#c4ccd4"
      w <- if (is.null(highlight)) width else if (is_hi) width * 1.5 else 1.1
      p <- plotly::add_trace(
        p, type = "scatter", mode = "lines", x = d[[x]], y = d[[y]] / scale,
        name = nm, legendgroup = s, connectgaps = FALSE,
        line = list(color = col, width = w, dash = dashes[((k - 1) %% 4) + 1]),
        hovertemplate = paste0("%{y:,.1f}", y_suffix, "<extra>", nm, "</extra>")
      )
    }
  }
  p <- tm_layout(p, y_title = y_title, y_suffix = y_suffix, range_buttons = .tm_is_date(df[[x]]))
  if (.tm_is_date(df[[x]])) p <- plotly::layout(p, xaxis = list(hoverformat = "%b %Y"))
  p
}

#' Stacked contribution bars (relative barmode) with a total-growth line.
#' `df` needs a `date` column, the `component`, `contrib` and `total` columns.
#' @param df data. @param component component column (factor order = stack order).
#' @param contrib,total column names. @param palette named/unnamed colours.
#' @param total_name legend label of the total line. @param y_title axis title.
#' @return plotly object.
tm_chart_contrib <- function(df, component, contrib = "contrib_pp", total = "total_growth_pct",
                             palette = NULL, total_name = "Total growth (%)", y_title = "Percentage points") {
  lv <- .tm_levels(df[[component]])
  cols <- .tm_palette_for(lv, palette)
  p <- plotly::plot_ly()
  for (s in lv) {
    d <- df[as.character(df[[component]]) == s, , drop = FALSE]
    p <- plotly::add_trace(p, type = "bar", x = d$date, y = d[[contrib]], name = s,
                           marker = list(color = cols[[s]]),
                           hovertemplate = paste0("%{y:+.1f}pp<extra>", s, "</extra>"))
  }
  tot <- df[!duplicated(df$date), c("date", total)]
  tot <- tot[order(tot$date), ]
  p <- plotly::add_trace(p, type = "scatter", mode = "lines+markers", x = tot$date, y = tot[[total]],
                         name = total_name, line = list(color = "#111111", width = 2),
                         marker = list(color = "#111111", size = 5),
                         hovertemplate = paste0("%{y:+.1f}%<extra>", total_name, "</extra>"))
  p |>
    tm_layout(y_title = y_title, zero_line = TRUE, range_buttons = TRUE) |>
    plotly::layout(barmode = "relative", bargap = 0.15, xaxis = list(hoverformat = "%b %Y"))
}

#' Diverging heatmap (burnt orange - white - teal), y order = factor levels (first on top).
#' z is capped to +/-cap for colour; hover shows the true value.
#' @param df long data. @param x,y,z column names.
#' @param cap colour cap. @param zmid centre value. @param x_is_date treat x as dates.
#' @param z_suffix suffix for values. @param text_values print values in cells.
#' @return plotly object.
tm_chart_heatmap <- function(df, x, y, z, cap = 50, zmid = 0, x_is_date = TRUE,
                             z_suffix = "%", text_values = FALSE) {
  ys <- .tm_levels(df[[y]])
  xs <- sort(unique(df[[x]]))
  zi <- matrix(NA_real_, nrow = length(ys), ncol = length(xs))
  ri <- match(as.character(df[[y]]), ys)
  ci <- match(df[[x]], xs)
  zi[cbind(ri, ci)] <- df[[z]]
  zc <- pmin(pmax(zi, zmid - cap), zmid + cap)
  xlab <- if (x_is_date) tm_fmt_month(xs) else as.character(xs)
  hov <- matrix(NA_character_, nrow = length(ys), ncol = length(xs))
  for (i in seq_along(ys)) {
    hov[i, ] <- ifelse(is.na(zi[i, ]), paste0(ys[i], "<br>", xlab, "<br>n/a"),
      paste0(ys[i], "<br>", xlab, "<br>", ifelse(zi[i, ] > 0, "+", ""),
             formatC(zi[i, ], format = "f", digits = 1, big.mark = ","), z_suffix))
  }
  args <- list(
    type = "heatmap", x = xs, y = ys, z = zc, hovertext = hov, hoverinfo = "text",
    xgap = 1, ygap = 1, zmin = zmid - cap, zmax = zmid + cap,
    colorscale = list(c(0, TM_COL$negative), c(0.5, "#ffffff"), c(1, TM_COL$positive)),
    colorbar = list(thickness = 10, len = 0.7, ticksuffix = z_suffix,
                    tickfont = list(size = 10, color = TM_COL$muted), outlinewidth = 0),
    showscale = TRUE
  )
  if (text_values) {
    args$text <- matrix(ifelse(is.na(zi), "", formatC(zi, format = "f", digits = 0)),
                        nrow = length(ys))
    args$texttemplate <- "%{text}"
    args$textfont <- list(size = 10)
  }
  p <- do.call(plotly::add_trace, c(list(plotly::plot_ly()), args))
  p <- tm_layout(p, legend = "none", hovermode = "closest")
  plotly::layout(p, yaxis = list(autorange = "reversed", categoryorder = "array",
                                 categoryarray = ys, showgrid = FALSE, type = "category",
                                 tickfont = list(size = 11, color = TM_COL$ink)),
                 margin = list(l = 40, r = 8, t = 12, b = 28))
}

#' Seasonal comparison for ONE monthly series.
#' Shaded min-max band and mean of the previous `n_years`, the previous year as
#' a thin dark line and `highlight_year` (default latest) bold with markers.
#' @param df data with `date` and `value`. @param value value column.
#' @param highlight_year year to emphasise. @param n_years comparison years.
#' @param cumulative plot calendar year-to-date sums. @param scale,y_title,colour styling.
#' @return plotly object.
tm_chart_seasonal <- function(df, value = "value_eur", highlight_year = NULL, n_years = 5,
                              cumulative = FALSE, scale = 1e9, y_title = "EUR bn",
                              colour = TM_COL$exports) {
  d <- data.frame(
    year = as.integer(format(df$date, "%Y")),
    month = as.integer(format(df$date, "%m")),
    v = df[[value]] / scale
  )
  d <- d[order(d$year, d$month), ]
  if (cumulative) d$v <- stats::ave(d$v, d$year, FUN = cumsum)
  if (is.null(highlight_year)) highlight_year <- max(d$year)
  prev_years <- (highlight_year - n_years):(highlight_year - 1)
  hov <- if (isTRUE(all.equal(scale, 1e9))) paste0(.TM_EURO, "%{y:,.1f}bn") else "%{y:,.2f}"
  p <- plotly::plot_ly()
  pd <- d[d$year %in% prev_years, ]
  if (nrow(pd) > 0) {
    mx <- tapply(pd$v, pd$month, max)
    mn <- tapply(pd$v, pd$month, min)
    av <- tapply(pd$v, pd$month, mean)
    m_lab <- month.abb[as.integer(names(mx))]
    rng_lab <- paste0(min(pd$year), "-", max(pd$year))
    p <- p |>
      plotly::add_trace(type = "scatter", mode = "lines", x = m_lab, y = as.numeric(mx),
                        line = list(color = "rgba(0,0,0,0)", width = 0),
                        showlegend = FALSE, hoverinfo = "skip") |>
      plotly::add_trace(type = "scatter", mode = "lines", x = m_lab, y = as.numeric(mn),
                        fill = "tonexty", fillcolor = .tm_rgba(TM_COL$band, 0.7),
                        line = list(color = "rgba(0,0,0,0)", width = 0),
                        name = paste0("Range ", rng_lab), hoverinfo = "skip") |>
      plotly::add_trace(type = "scatter", mode = "lines", x = m_lab, y = as.numeric(av),
                        line = list(color = "#7b8794", width = 1.6, dash = "dash"),
                        name = paste0("Average ", rng_lab),
                        hovertemplate = paste0(hov, "<extra>Average ", rng_lab, "</extra>"))
  }
  py <- d[d$year == highlight_year - 1, ]
  if (nrow(py) > 0) {
    p <- plotly::add_trace(p, type = "scatter", mode = "lines", x = month.abb[py$month], y = py$v,
                           line = list(color = "#3e4c59", width = 1.6), name = as.character(highlight_year - 1),
                           hovertemplate = paste0(hov, "<extra>", highlight_year - 1, "</extra>"))
  }
  hy <- d[d$year == highlight_year, ]
  if (nrow(hy) > 0) {
    p <- plotly::add_trace(p, type = "scatter", mode = "lines+markers", x = month.abb[hy$month], y = hy$v,
                           line = list(color = colour, width = 3.2), marker = list(color = colour, size = 6),
                           name = as.character(highlight_year),
                           hovertemplate = paste0(hov, "<extra>", highlight_year, "</extra>"))
  }
  p |>
    tm_layout(y_title = y_title) |>
    plotly::layout(xaxis = list(type = "category", categoryorder = "array", categoryarray = month.abb))
}

#' 100% stacked area of shares (percent) over time.
#' @param df data. @param x date column. @param share share column in percent.
#' @param group group column (factor order = stack order). @param palette colours.
#' @return plotly object.
tm_chart_area_share <- function(df, x = "date", share, group, palette = NULL) {
  lv <- .tm_levels(df[[group]])
  cols <- .tm_palette_for(lv, palette)
  p <- plotly::plot_ly()
  for (s in lv) {
    d <- df[as.character(df[[group]]) == s, , drop = FALSE]
    d <- d[order(d[[x]]), , drop = FALSE]
    p <- plotly::add_trace(
      p, type = "scatter", mode = "lines", x = d[[x]], y = d[[share]], name = s,
      stackgroup = "one", fillcolor = .tm_rgba(cols[[s]], 0.92),
      line = list(color = "white", width = 0.5),
      hovertemplate = paste0("%{y:.1f}%<extra>", s, "</extra>")
    )
  }
  p <- tm_layout(p, y_suffix = "%", range_buttons = .tm_is_date(df[[x]]))
  p <- plotly::layout(p, yaxis = list(range = c(0, 100), ticksuffix = "%"))
  if (.tm_is_date(df[[x]])) p <- plotly::layout(p, xaxis = list(hoverformat = "%b %Y"))
  p
}

# ---------------------------------------------------------------------------
# Ranked / composition charts
# ---------------------------------------------------------------------------

#' Ranked horizontal bars, largest at top, values printed at the bar end.
#' With `diverging` bars are coloured by sign. With `top_n`, the n items with
#' the largest absolute value (largest value if all are positive) are kept.
#' @param df data. @param label,value column names.
#' @param scale divisor. @param value_suffix,value_prefix,unit text around the number.
#' @param digits decimals. @param diverging colour by sign.
#' @param top_n keep only n bars. @param colour fixed colour if not diverging.
#' @return plotly object.
tm_chart_hbar <- function(df, label, value, scale = 1e9, value_suffix = "", value_prefix = .TM_EURO,
                          digits = 1, unit = "bn", diverging = TRUE, top_n = NULL, colour = NULL) {
  d <- data.frame(label = as.character(df[[label]]), v = df[[value]] / scale, stringsAsFactors = FALSE)
  d <- d[!is.na(d$v), ]
  if (!is.null(top_n) && nrow(d) > top_n) {
    key <- if (any(d$v < 0)) abs(d$v) else d$v
    d <- d[order(-key), ][seq_len(top_n), ]
  }
  d <- d[order(-d$v), ]
  if (nrow(d) == 0) return(tm_empty_plot("No data"))
  num <- formatC(abs(d$v), format = "f", digits = digits, big.mark = ",")
  txt <- paste0(ifelse(d$v < 0, "-", ""), value_prefix, num, unit, value_suffix)
  col <- if (diverging) ifelse(d$v >= 0, TM_COL$positive, TM_COL$negative)
         else rep(if (is.null(colour)) TM_COL$exports else colour, nrow(d))
  lo <- min(0, d$v); hi <- max(0, d$v)
  pad <- 0.22 * (hi - lo)
  rng <- c(if (lo < 0) lo - pad else 0, if (hi > 0) hi + pad else 0)
  p <- plotly::plot_ly() |>
    plotly::add_trace(
      type = "bar", orientation = "h", x = d$v, y = d$label, text = txt,
      textposition = "outside", cliponaxis = FALSE,
      textfont = list(size = 11, color = TM_COL$ink),
      marker = list(color = col), showlegend = FALSE,
      hovertemplate = "%{y}<br>%{text}<extra></extra>"
    )
  p <- tm_layout(p, legend = "none", hovermode = "closest")
  plotly::layout(
    p, bargap = 0.28,
    xaxis = list(range = rng, showgrid = FALSE, showticklabels = FALSE, ticks = "",
                 zeroline = lo < 0, zerolinecolor = "#52606d", zerolinewidth = 1.2, fixedrange = TRUE),
    yaxis = list(autorange = "reversed", categoryorder = "array", categoryarray = d$label,
                 type = "category", showgrid = FALSE, ticks = "", automargin = TRUE,
                 tickfont = list(size = 11, color = TM_COL$ink), fixedrange = TRUE),
    margin = list(l = 8, r = 8, t = 8, b = 8)
  )
}

#' Treemap with white tile borders.
#' Hover shows the value in EUR bn and the share of the parent and of the root.
#' @param labels,parents character vectors (parents "" for the root). @param values numeric EUR.
#' @param ids optional unique node ids. @param colours optional node colours. @param text optional extra tile text.
#' @return plotly object.
tm_chart_treemap <- function(labels, parents, values, ids = NULL, colours = NULL, text = NULL) {
  args <- list(
    type = "treemap", labels = labels, parents = parents, values = values,
    branchvalues = "total", sort = TRUE, pathbar = list(visible = FALSE),
    marker = list(line = list(color = "white", width = 2), pad = list(t = 4, l = 2, r = 2, b = 2)),
    customdata = tm_fmt_bn(values),
    hovertemplate = paste0("<b>%{label}</b><br>%{customdata}<br>%{percentParent:.1%} of parent",
                           "<br>%{percentRoot:.1%} of total<extra></extra>"),
    textfont = list(family = .TM_FONT, size = 13)
  )
  if (!is.null(ids)) args$ids <- ids
  if (!is.null(colours)) args$marker$colors <- colours
  if (!is.null(text)) {
    args$text <- text
    args$textinfo <- "label+text"
  } else {
    args$textinfo <- "label"
  }
  p <- do.call(plotly::add_trace, c(list(plotly::plot_ly()), args))
  p |>
    tm_layout(legend = "none", hovermode = "closest") |>
    plotly::layout(margin = list(l = 2, r = 2, t = 2, b = 2))
}

#' Labelled bubble chart with a zero reference line on y.
#' @param df data. @param x,y,size,label column names. @param colour marker colour.
#' @param x_title,y_title axis titles. @param x_suffix,y_suffix tick suffixes.
#' @return plotly object.
tm_chart_scatter_bubble <- function(df, x, y, size, label, colour = TM_COL$exports,
                                    x_title = NULL, y_title = NULL, x_suffix = "", y_suffix = "") {
  sz <- abs(df[[size]])
  sz[is.na(sz)] <- 0
  mx <- max(sz, 1e-9)
  p <- plotly::plot_ly() |>
    plotly::add_trace(
      type = "scatter", mode = "markers+text", x = df[[x]], y = df[[y]],
      text = df[[label]], textposition = "middle right",
      textfont = list(size = 11, color = TM_COL$ink),
      marker = list(size = sz, sizemode = "area", sizeref = 2 * mx / (46^2), sizemin = 4,
                    color = .tm_rgba(colour, 0.7), line = list(color = "white", width = 1)),
      showlegend = FALSE, cliponaxis = FALSE,
      hovertemplate = paste0("<b>%{text}</b><br>", if (is.null(x_title)) "x" else x_title,
                             ": %{x:,.1f}", x_suffix, "<br>",
                             if (is.null(y_title)) "y" else y_title, ": %{y:,.1f}", y_suffix,
                             "<extra></extra>")
    )
  p <- tm_layout(p, y_title = y_title, y_suffix = y_suffix, legend = "none", hovermode = "closest")
  plotly::layout(
    p,
    xaxis = list(title = list(text = if (is.null(x_title)) "" else x_title,
                              font = list(size = 11, color = TM_COL$muted)),
                 ticksuffix = x_suffix),
    shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = 0, y1 = 0,
                       line = list(color = "#52606d", width = 1.2), layer = "below"))
  )
}

#' Blank plotly with a centred muted message (empty states).
#' @param message text to show.
#' @return plotly object.
tm_empty_plot <- function(message) {
  plotly::plot_ly() |>
    plotly::layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      paper_bgcolor = "white", plot_bgcolor = "white",
      annotations = list(list(text = message, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                              showarrow = FALSE, font = list(size = 14, color = TM_COL$muted,
                                                              family = .TM_FONT))),
      margin = list(l = 8, r = 8, t = 8, b = 8)
    ) |>
    plotly::config(displaylogo = FALSE, displayModeBar = FALSE)
}

# ---------------------------------------------------------------------------
# UI helpers
# ---------------------------------------------------------------------------

#' Inline SVG sparkline (no JS); last point dotted. Handles NA and constant series.
#' @param values numeric vector. @param colour line colour.
#' @param width,height pixel size. @param fill shade under the line.
#' @return htmltools tag.
tm_sparkline <- function(values, colour = TM_COL$exports, width = 120, height = 32, fill = TRUE) {
  n <- length(values)
  ok <- which(is.finite(values))
  svg <- function(...) htmltools::tag("svg", list(
    class = "tm-spark", width = width, height = height,
    viewBox = paste(0, 0, width, height), role = "img", `aria-hidden` = "true", ...))
  if (length(ok) < 2) return(svg())
  pad <- 3
  v <- values[ok]
  rngv <- range(v)
  span <- if (diff(rngv) == 0) 1 else diff(rngv)
  xs <- pad + (ok - 1) / max(n - 1, 1) * (width - 2 * pad)
  ys <- if (diff(rngv) == 0) rep(height / 2, length(v)) else
    height - pad - (v - rngv[1]) / span * (height - 2 * pad)
  pts <- paste(sprintf("%.1f,%.1f", xs, ys), collapse = " ")
  kids <- list()
  if (fill) {
    poly <- paste(pts, sprintf("%.1f,%.1f %.1f,%.1f", xs[length(xs)], height - 1, xs[1], height - 1))
    kids <- c(kids, list(htmltools::tag("polygon", list(points = poly, fill = colour, `fill-opacity` = "0.12", stroke = "none"))))
  }
  kids <- c(kids, list(
    htmltools::tag("polyline", list(points = pts, fill = "none", stroke = colour, `stroke-width` = "1.6",
                                    `stroke-linejoin` = "round", `stroke-linecap` = "round")),
    htmltools::tag("circle", list(cx = sprintf("%.1f", xs[length(xs)]), cy = sprintf("%.1f", ys[length(ys)]),
                                  r = "2.4", fill = colour))
  ))
  do.call(svg, kids)
}

#' Coloured up/down delta indicator for KPI lines.
#' Teal for positive, burnt orange for negative, grey for NA / zero.
#' @param x numeric change. @param suffix text after the number ("%" or "pp").
#' @param digits decimals. @param label optional muted label after the value (e.g. "y/y").
#' @return htmltools span.
tm_delta_span <- function(x, suffix = "%", digits = 1, label = NULL) {
  if (length(x) != 1 || is.na(x)) {
    cls <- "tm-delta-na"; colr <- TM_COL$muted; txt <- "n/a"
  } else {
    r <- round(x, digits)
    cls <- if (r > 0) "tm-delta-up" else if (r < 0) "tm-delta-down" else "tm-delta-na"
    colr <- if (r > 0) TM_COL$positive else if (r < 0) TM_COL$negative else TM_COL$muted
    arrow <- if (r > 0) paste0(intToUtf8(0x25B2), " ") else if (r < 0) paste0(intToUtf8(0x25BC), " ") else paste0(intToUtf8(0x2013), " ")
    txt <- paste0(arrow, ifelse(r > 0, "+", ifelse(r < 0, "-", "")),
                  formatC(abs(r), format = "f", digits = digits, big.mark = ","), suffix)
  }
  htmltools::tags$span(
    class = paste("tm-delta", cls), style = paste0("color:", colr, ";"), txt,
    if (!is.null(label)) htmltools::tags$span(class = "tm-delta-label", paste0(" ", label))
  )
}

#' KPI tile.
#' @param title small uppercase label. @param value large headline value.
#' @param lines list of tags/strings shown as small rows (e.g. tm_delta_span()).
#' @param spark optional tm_sparkline(). @param accent top-border colour.
#' @param footnote optional small muted note.
#' @return bslib card.
tm_kpi_card <- function(title, value, lines = list(), spark = NULL, accent = TM_COL$exports, footnote = NULL) {
  bslib::card(
    class = "tm-kpi border-0 shadow-sm",
    style = paste0("border-top: 3px solid ", accent, " !important;"),
    fill = FALSE,
    bslib::card_body(
      class = "tm-kpi-body",
      htmltools::div(class = "tm-kpi-title", title),
      htmltools::div(class = "tm-kpi-value", value),
      htmltools::div(
        class = "tm-kpi-foot",
        htmltools::div(class = "tm-kpi-lines",
                       lapply(lines, function(l) htmltools::div(class = "tm-kpi-line", l))),
        if (!is.null(spark)) htmltools::div(class = "tm-kpi-spark", spark)
      ),
      if (!is.null(footnote)) htmltools::div(class = "tm-kpi-note", footnote)
    )
  )
}

#' Standard card wrapper for every chart.
#' Bold title (state the takeaway/topic), optional muted subtitle describing
#' units/transform, the body (...), and a small muted footer.
#' @param title,subtitle header text. @param ... body content (e.g. plotlyOutput).
#' @param footer footer text (NULL for none). @param full_screen allow expanding the card.
#' @return bslib card.
tm_chart_card <- function(title, subtitle = NULL, ..., footer = .TM_SOURCE_NOTE, full_screen = TRUE) {
  bslib::card(
    full_screen = full_screen,
    class = "tm-card border-0 shadow-sm",
    bslib::card_header(
      class = "tm-card-header",
      htmltools::div(class = "tm-card-title", title),
      if (!is.null(subtitle)) htmltools::div(class = "tm-subtitle", subtitle)
    ),
    bslib::card_body(class = "tm-card-body", ...),
    if (!is.null(footer)) bslib::card_footer(class = "tm-footer", footer)
  )
}

#' CSS needed by the helpers above (include once per page).
#' @return htmltools style tag.
tm_page_css <- function() {
  htmltools::tags$style(htmltools::HTML("
.tm-card, .tm-kpi { background: #fff; border-radius: 0.6rem; box-shadow: 0 1px 3px rgba(31,41,51,0.08) !important; }
.tm-card .tm-card-header { background: transparent; border-bottom: 0; padding: 0.85rem 1rem 0.2rem 1rem; }
.tm-card-title { font-weight: 700; font-size: 1.02rem; color: #1f2933; line-height: 1.25; }
.tm-subtitle { color: #6b7280; font-size: 0.84rem; margin-top: 0.15rem; }
.tm-card .tm-card-body { padding: 0.25rem 0.6rem 0.4rem 0.6rem; }
.tm-card .tm-footer { background: transparent; border-top: 0; color: #6b7280; font-size: 0.74rem; padding: 0.2rem 1rem 0.65rem 1rem; }
.tm-kpi .tm-kpi-body { padding: 0.8rem 1rem 0.7rem 1rem; }
.tm-kpi-title { text-transform: uppercase; letter-spacing: 0.05em; font-size: 0.72rem; font-weight: 600; color: #6b7280; }
.tm-kpi-value { font-size: 2rem; font-weight: 700; line-height: 1.15; color: #1f2933; margin: 0.15rem 0 0.35rem 0; }
.tm-kpi-foot { display: flex; justify-content: space-between; align-items: flex-end; gap: 0.5rem; }
.tm-kpi-lines { font-size: 0.84rem; color: #1f2933; }
.tm-kpi-line { margin-top: 0.05rem; }
.tm-kpi-spark { flex: 0 0 auto; line-height: 0; }
.tm-kpi-note { margin-top: 0.4rem; font-size: 0.72rem; color: #6b7280; }
.tm-delta { font-weight: 600; white-space: nowrap; }
.tm-delta-up { color: #0f4c5c; }
.tm-delta-down { color: #bb3e03; }
.tm-delta-na { color: #6b7280; }
.tm-delta-label { font-weight: 400; color: #6b7280; }
.tm-keypoints { background: #fff; border-left: 4px solid #0f4c5c; border-radius: 0.4rem; padding: 0.7rem 1rem; margin-bottom: 1rem; box-shadow: 0 1px 3px rgba(31,41,51,0.08); }
.tm-keypoints ul { margin: 0.2rem 0 0 0; padding-left: 1.1rem; }
.tm-keypoints li { margin-bottom: 0.2rem; }
"))
}
