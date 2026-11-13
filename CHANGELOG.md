# Changelog

All notable changes to the Irish Trade Monitor are recorded here, newest first.
Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/). Each
entry gives the git commit so any change can be inspected with
`git show <commit>`.

## [2.0] - 2026-11-13

Version 2 is the analytical redesign. It is built on version 1, the snapshot
recorded below as `1.0` (`7066be4`, tag `baseline-2026-11-11`).

### Added
- **Briefing page:** the month in one page. Headline, smoothed and underlying
  measures, auto-drafted key points, growth drivers and biggest movers
  (`c7e28f1`, `cfc5c1d`).
- **Trends page:** signal versus noise for any series. Three- and 12-month
  averages, growth on three bases, this year against the previous five, a
  momentum heatmap, growth contributions and a breadth-of-growth index
  (`c7e28f1`).
- **Concentration page:** composition, product and market concentration, what
  is inside each product group, and value versus volume versus unit value
  (`c7e28f1`, renamed from "Pharma & concentration" in `f0ce1c2`).
- **Markets page:** partner-group shares, front-loading and payback, bilateral
  balances and country profiles (`c7e28f1`).
- **Ten analytical product groups** built from SITC 1-digit sections and 5-digit
  products, defined in `app/R/analytics.R` (`cfc5c1d`).
- **PDF briefing:** a pre-rendered PDF of the month's briefing, generated from
  the app's data by `scripts/render_briefing.R` and served from
  `app/www/briefing/` (`3854bb2`, merge `7b774c3`).
- **Tests** for the analytics, charts, each page module and the PDF briefing
  (`tests/`). All pass on this version.
- **CHANGELOG.md** (this file) and a rewritten README describing version 2.

### Changed
- **Department of Finance colour scheme and Lato typeface** (`c83c184`).
- **Restyle: beige removed and presentation tidied** (`e98f533`, 2026-11-13).
  - Page background from beige (`#f6f6f2`) to a cool grey canvas (`#f4f5f7`),
    with white cards and inputs.
  - Thinner, flatter card and key-point borders; tighter corners.
  - Type scale standardised on Lato (labels 0.75rem, secondary 0.8125rem,
    body 1rem, headline figures 1.75rem).
  - Chart series colours: imports and "Food" move to amber `#c9821a`; purple
    `#7c62b8`, yellow `#dcb945` and rust `#b5561f` are added to the palette.
  - Presentation only. No change to data, figures, wording, chart structure or
    page structure.
- **Package metadata:** `DESCRIPTION` lists the suggested packages used for the
  PDF briefing (`knitr`, `rmarkdown`, `showtext`).

### Known issues
- `DESCRIPTION` still reads `Version: 0.2.0`. The app is version 2.0; the
  package version was not updated.

## [1.0] - 2026-11-11

Snapshot before the analytical redesign. The public GitHub `main` at
`2570468` ("Clean up public repo instructions") is the state before this
snapshot.

- Baseline snapshot (`7066be4`, tag `baseline-2026-11-11`), including refreshed
  monthly and annual data files, the new driver tables (`drivers_country`,
  `drivers_product`), `scripts/update_trade_data.R`, and a `.gitignore`.
- Explorer pages: monthly movers, annual comparison, by country, product by
  country and product over time.
