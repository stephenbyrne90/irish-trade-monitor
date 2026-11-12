# Static charts and LaTeX helpers for the PDF briefing (report/briefing.Rmd).
#
# The app's charts are interactive plotly objects, which cannot go into a PDF,
# so this file draws the same Briefing charts with ggplot2. Every number comes
# from the app's own functions (app/R/analytics.R, app/R/mod_briefing.R, ...),
# so the PDF always agrees with the Briefing page.
#
# Expects ggplot2 to be attached and the app's R/ files to be sourced first.
# Source stays ASCII-only, like the rest of the project.

# ---------------------------------------------------------------------------
# Fonts and theme
# ---------------------------------------------------------------------------

#' Register Lato (the app's typeface) from the TeX distribution for charts.
#' Falls back to the device's default sans face when showtext or Lato is missing.
#' @return font family name to use in the charts.
pdf_font_family <- function() {
  if (!requireNamespace("showtext", quietly = TRUE)) return("sans")
  kp <- Sys.which("kpsewhich")
  if (!nzchar(kp)) return("sans")
  find <- function(file) {
    p <- suppressWarnings(tryCatch(system2(kp, file, stdout = TRUE, stderr = FALSE),
                                   error = function(e) character()))
    if (length(p) > 0 && file.exists(p[1])) p[1] else NULL
  }
  regular <- find("Lato-Regular.ttf")
  if (is.null(regular)) return("sans")
  sysfonts::font_add("Lato", regular = regular, bold = find("Lato-Bold.ttf"),
                     italic = find("Lato-Italic.ttf"), bolditalic = find("Lato-BoldItalic.ttf"))
  showtext::showtext_auto()
  # showtext draws text as outlines; 3 segments per curve (default 10) is still
  # sharp at chart text sizes and keeps the PDF to roughly a third of the size.
  showtext::showtext_opts(nseg = 3)
  "Lato"
}

pdf_theme <- function(family = "sans", base_size = 7.5) {
  theme_minimal(base_size = base_size, base_family = family) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = TM_COL$grid, linewidth = 0.3),
      axis.title.x = element_blank(),
      axis.title.y = element_text(colour = TM_COL$muted, size = base_size - 0.5),
      axis.text = element_text(colour = TM_COL$muted),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(colour = TM_COL$ink, size = base_size - 0.5),
      legend.key.height = grid::unit(7, "pt"),
      legend.key.width = grid::unit(11, "pt"),
      legend.spacing.x = grid::unit(2, "pt"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-5, 0, 0, 0),
      legend.box = "vertical",
      legend.spacing.y = grid::unit(0, "pt"),
      plot.margin = margin(3, 6, 2, 2)
    )
}

pdf_year_axis <- function() {
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.02)))
}

# "Aug 26" month labels (locale independent).
pdf_month_label <- function(d) {
  paste(month.abb[as.integer(format(d, "%m"))], format(d, "%y"))
}

pdf_empty <- function(message, family = "sans") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = message, colour = TM_COL$muted, family = family, size = 3) +
    theme_void()
}

#' Save a chart as a vector PDF and return its path (relative, for LaTeX).
pdf_save <- function(plot, name, width_cm, height_cm, dir = "figs") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, paste0(name, ".pdf"))
  w <- width_cm / 2.54
  h <- height_cm / 2.54
  # pdf() rather than cairo_pdf(): cairo is often unavailable on macOS. With
  # showtext the text is drawn as outlines; without it WinAnsi (CP1252) covers
  # the euro sign and the ellipsis used in labels.
  grDevices::pdf(path, width = w, height = h, useDingbats = FALSE, encoding = "WinAnsi.enc")
  on.exit(grDevices::dev.off())
  print(plot)
  path
}

# ---------------------------------------------------------------------------
# Charts (static versions of the Briefing page charts)
# ---------------------------------------------------------------------------

#' Exports and imports: faint monthly lines, bold 3-month averages, balance bars.
pdf_chart_trade <- function(td, family = "sans") {
  d <- td$latest_date
  x0 <- briefing_months_back(d, 60)
  tt <- td$total_tf
  tt <- tt[as.character(tt$flow) %in% c("1", "2") & tt$date >= x0 & tt$date <= d, , drop = FALSE]
  nm <- unname(TM_FLOW_LABELS[as.character(tt$flow)])
  lv <- c("Exports, 3-month average", "Imports, 3-month average", "Exports, monthly", "Imports, monthly")
  long <- rbind(
    data.frame(date = tt$date, series = paste0(nm, ", 3-month average"), v = tt$ma3 / 1e9),
    data.frame(date = tt$date, series = paste0(nm, ", monthly"), v = tt$value_eur / 1e9)
  )
  long$series <- factor(long$series, levels = lv)
  b <- td$balance[td$balance$date >= x0 & td$balance$date <= d, , drop = FALSE]
  cols <- stats::setNames(c(TM_COL$exports, TM_COL$imports,
                            scales::alpha(TM_COL$exports, 0.45), scales::alpha(TM_COL$imports, 0.45)), lv)
  lw <- stats::setNames(c(0.95, 0.95, 0.35, 0.35), lv)
  p <- ggplot() +
    geom_col(data = b, aes(date, balance / 1e9, fill = "Monthly balance"), width = 22) +
    geom_hline(yintercept = 0, colour = TM_COL$balance, linewidth = 0.3)
  ev <- td$events
  ev <- ev[ev$date >= x0 & ev$date <= d, , drop = FALSE]
  if (nrow(ev) > 0) {
    p <- p +
      geom_vline(data = ev, aes(xintercept = date), colour = "#9ea3aa", linewidth = 0.3, linetype = "dotted") +
      geom_text(data = ev, aes(x = date, y = Inf, label = short), angle = 90, hjust = 1.08, vjust = -0.5,
                size = 2.1, colour = TM_COL$muted, family = family)
  }
  p +
    geom_line(data = long, aes(date, v, colour = series, linewidth = series), na.rm = TRUE) +
    scale_colour_manual(values = cols) +
    scale_linewidth_manual(values = lw) +
    scale_fill_manual(values = c("Monthly balance" = "#9aa5b1")) +
    guides(fill = guide_legend(order = 2, override.aes = list(alpha = 0.45)),
           colour = guide_legend(order = 1), linewidth = guide_legend(order = 1)) +
    pdf_year_axis() +
    labs(y = "EUR bn") +
    pdf_theme(family) +
    theme(legend.box = "horizontal", legend.spacing.x = grid::unit(4, "pt"))
}

#' Headline versus underlying 3-month y/y growth for one flow. Unlike the
#' interactive chart, the y axis is not clipped: a printed chart cannot be
#' rescaled, so the full range of the five-year window is shown.
pdf_chart_growth <- function(td, flow, family = "sans") {
  g <- briefing_growth_data(td, flow)
  s <- g$series[g$series$date >= g$x0 & g$series$date <= td$latest_date, , drop = FALSE]
  if (nrow(s) == 0) return(pdf_empty("No growth data", family))
  labs_ <- unname(g$measures)
  s$label <- factor(unname(g$measures[as.character(s$measure)]), levels = labs_)
  is_total <- as.character(s$measure) == "total"
  cols <- stats::setNames(unlist(BRIEFING_COL[names(g$measures)]), labs_)
  lw <- stats::setNames(ifelse(names(g$measures) == "total", 0.95, 0.55), labs_)
  ggplot(mapping = aes(date, yoy3_pct, colour = label, linewidth = label)) +
    geom_hline(yintercept = 0, colour = TM_COL$balance, linewidth = 0.3) +
    geom_line(data = s[!is_total, , drop = FALSE], na.rm = TRUE) +
    geom_line(data = s[is_total, , drop = FALSE], na.rm = TRUE) +
    scale_colour_manual(values = cols, limits = labs_) +
    scale_linewidth_manual(values = lw, limits = labs_) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE), linewidth = guide_legend(nrow = 2, byrow = TRUE)) +
    pdf_year_axis() +
    labs(y = NULL) +
    pdf_theme(family)
}

#' Contributions of the ten analytical groups to 3-month y/y growth, last 36 months.
pdf_chart_drivers <- function(td, flow, family = "sans") {
  cc <- briefing_drivers_data(td, flow)
  if (nrow(cc) == 0) return(pdf_empty("No contribution data", family))
  short <- stats::setNames(TM_GROUPS$short, TM_GROUPS$group)
  cc$grp <- factor(unname(short[as.character(cc$group)]), levels = TM_GROUPS$short)
  cols <- stats::setNames(TM_GROUPS$colour, TM_GROUPS$short)
  tot <- cc[!duplicated(cc$date), c("date", "total_growth_pct")]
  tot <- tot[order(tot$date), ]
  ggplot() +
    geom_col(data = cc, aes(date, contrib_pp, fill = grp), width = 23,
             position = position_stack(reverse = TRUE)) +
    geom_hline(yintercept = 0, colour = TM_COL$balance, linewidth = 0.3) +
    geom_line(data = tot, aes(date, total_growth_pct, colour = "Total growth (%)"), linewidth = 0.6) +
    geom_point(data = tot, aes(date, total_growth_pct, colour = "Total growth (%)"), size = 0.6) +
    scale_fill_manual(values = cols) +
    scale_colour_manual(values = c("Total growth (%)" = TM_COL$ink)) +
    guides(fill = guide_legend(nrow = 3, order = 1), colour = guide_legend(order = 2)) +
    scale_x_date(date_breaks = "6 months", labels = pdf_month_label, expand = expansion(mult = c(0.01, 0.02))) +
    labs(y = "Percentage points") +
    pdf_theme(family) +
    theme(legend.key.width = grid::unit(7, "pt"), legend.text = element_text(size = 6.3))
}

#' Biggest movers (horizontal diverging bars with value labels).
pdf_chart_movers <- function(td, flow, kind = c("product", "market"), family = "sans") {
  kind <- match.arg(kind)
  mv <- briefing_movers(td, flow, kind)
  if (nrow(mv) == 0) return(pdf_empty("No data", family))
  d <- data.frame(label = mv$label, v = mv$delta / 1e9, txt = tm_fmt_bn(mv$delta), stringsAsFactors = FALSE)
  d <- d[order(-d$v), ]
  d$label <- factor(d$label, levels = rev(d$label))
  d$up <- d$v >= 0
  lo <- min(0, d$v)
  hi <- max(0, d$v)
  pad <- 0.24 * (hi - lo)
  ggplot(d, aes(v, label, fill = up)) +
    geom_col(width = 0.68) +
    geom_vline(xintercept = 0, colour = TM_COL$balance, linewidth = 0.35) +
    geom_text(aes(label = txt, hjust = ifelse(up, -0.12, 1.12)), size = 2.25, colour = TM_COL$ink, family = family) +
    scale_fill_manual(values = c(`TRUE` = TM_COL$positive, `FALSE` = TM_COL$negative), guide = "none") +
    scale_x_continuous(limits = c(if (lo < 0) lo - pad else 0, if (hi > 0) hi + pad else 0)) +
    labs(y = NULL) +
    pdf_theme(family) +
    theme(panel.grid.major.y = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_text(colour = TM_COL$ink))
}

# ---------------------------------------------------------------------------
# LaTeX helpers (macros are defined in report/preamble.tex)
# ---------------------------------------------------------------------------

#' Escape text for LaTeX.
pdf_tex <- function(x) {
  x <- as.character(x)
  x <- gsub("\\", "\\textbackslash ", x, fixed = TRUE)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
  gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
}

#' Emit a block of raw LaTeX from a chunk with results = "asis".
pdf_raw <- function(...) {
  cat("\n```{=latex}\n", paste0(c(...), collapse = "\n"), "\n```\n", sep = "")
}

#' Signed, coloured change ("+3.4%" in green, "-2.0%" in red) with a muted label.
pdf_delta <- function(x, suffix = "%", digits = 1, label = NULL) {
  lab <- if (is.null(label)) "" else paste0(" \\tmlab{", pdf_tex(label), "}")
  if (length(x) != 1 || is.na(x)) return(paste0("\\tmflat{n/a}", lab))
  r <- round(x, digits)
  cmd <- if (r > 0) "\\tmup" else if (r < 0) "\\tmdown" else "\\tmflat"
  txt <- paste0(if (r > 0) "+" else if (r < 0) "-" else "",
                formatC(abs(r), format = "f", digits = digits, big.mark = ","), suffix)
  paste0(cmd, "{", pdf_tex(txt), "}", lab)
}

#' One KPI tile. `lines` are already-LaTeX strings (use pdf_delta() / pdf_tex()).
pdf_kpi <- function(accent, title, value, lines = character()) {
  hex <- toupper(sub("^#", "", accent))
  paste0("\\tmkpi{", hex, "}{", pdf_tex(title), "}{", pdf_tex(value), "}{",
         paste(lines, collapse = "\\\\ "), "}")
}

pdf_kpi_row <- function(tiles) {
  pdf_raw("\\noindent", paste(tiles, collapse = "\\hfill\n"), "\\par")
}

#' The five headline tiles of the Briefing page (same numbers as
#' briefing_kpi_row()). Titles omit the month, which is in the page header.
pdf_briefing_tiles <- function(m) {
  strong <- function(label, value) paste0("\\tmlab{", pdf_tex(label), "} \\textbf{", pdf_tex(value), "}")
  flow_tile <- function(title, x, colour) {
    pdf_kpi(colour, title, tm_fmt_bn(x$value), c(
      pdf_delta(x$yoy, "%", 1, "y/y"),
      pdf_delta(x$mom3, "%", 1, "3m on 3m"),
      pdf_delta(x$yoy3, "%", 1, "3m y/y"),
      strong("12-month total", tm_fmt_bn(x$sum12))
    ))
  }
  us_col <- TM_PARTNER_GROUP_COLOURS[["United States"]]
  c(
    flow_tile("Exports", m$exp, TM_COL$exports),
    flow_tile("Imports", m$imp, TM_COL$imports),
    pdf_kpi(TM_COL$balance, "Trade balance", tm_fmt_bn(m$bal$value), c(
      strong("12-month balance", tm_fmt_bn(m$bal$bal12)),
      pdf_delta(m$bal$bal12_chg / 1e9, "bn", 1, "EUR vs a year earlier")
    )),
    pdf_kpi("#2e7d6f", "Underlying exports", tm_fmt_bn(m$und_chem$value), c(
      pdf_delta(m$und_chem$yoy, "%", 1, "y/y"),
      pdf_delta(m$und_chem$yoy3, "%", 1, "3m y/y"),
      "\\tmlab{Excl. chemicals \\& pharma}"
    )),
    pdf_kpi(us_col, "US share of exports", tm_fmt_share(m$us$share12), c(
      pdf_delta(m$us$share_chg, "pp", 1, "vs 12 months earlier"),
      "\\tmlab{Share of 12-month exports}"
    ))
  )
}

#' Five concentration / market-exposure tiles for one flow, from the same
#' functions as the Concentration and Markets pages.
pdf_exposure_tiles <- function(td, flow = "2") {
  k <- structure_kpis(td, flow)
  mk <- markets_kpi_data(td, flow)
  lab12 <- "vs 12 months earlier"
  c(
    pdf_kpi(TM_GROUP_COLOURS[["Pharmaceuticals"]], "Chemicals & pharma share", tm_fmt_share(k$chem$share), c(
      pdf_delta(k$chem$d12, "pp", 1, lab12),
      pdf_delta(k$chem$d60, "pp", 1, "vs 5 years earlier")
    )),
    pdf_kpi("#ae1122", "Top 3 products", tm_fmt_share(k$top3$share), c(
      pdf_delta(k$top3$d12, "pp", 1, lab12),
      paste0("\\tmlab{SITC ", pdf_tex(paste(k$top3$codes, collapse = ", ")), "}")
    )),
    pdf_kpi(TM_COL$balance, "Effective no. of products", formatC(k$neff$neff, format = "f", digits = 1), c(
      pdf_delta(k$neff$neff - k$neff$neff12, "", 1, lab12),
      paste0("\\tmlab{HHI ", formatC(k$neff$hhi, format = "f", digits = 0, big.mark = ","), "}")
    )),
    pdf_kpi(TM_PARTNER_GROUP_COLOURS[["Euro area"]], "Euro area share", tm_fmt_share(mk$ea$value), c(
      pdf_delta(mk$ea$change, "pp", 1, lab12)
    )),
    pdf_kpi(TM_PARTNER_GROUP_COLOURS[["United States"]], "Goods balance with the US", tm_fmt_bn(mk$bal$value), c(
      pdf_delta(mk$bal$change / 1e9, "bn", 1, "EUR vs a year earlier"),
      "\\tmlab{12-month total}"
    ))
  )
}

#' Key points as a LaTeX list.
pdf_keypoints <- function(points) {
  pdf_raw("\\begin{tmpoints}", paste0("\\item ", pdf_tex(points)), "\\end{tmpoints}")
}

#' One chart with a bold title (optional) and a muted subtitle.
pdf_chart_block <- function(title, subtitle, file) {
  paste0("\\tmchart{", if (is.null(title)) "" else pdf_tex(title), "}{", pdf_tex(subtitle), "}{", file, "}")
}

#' Two charts side by side. The subtitles get a fixed two-line box so the two
#' charts line up whatever the length of the text.
pdf_chart_pair <- function(left, right) {
  col <- function(x) paste0("\\begin{minipage}[t]{0.485\\textwidth}\\setlength{\\tmsubh}{17.5pt}", x,
                            "\\end{minipage}")
  pdf_raw("\\noindent", paste0(col(left), "\\hfill"), col(right), "\\par\\vspace{8pt}")
}
