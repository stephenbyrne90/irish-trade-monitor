# Irish Trade Monitor

An R Shiny app for monitoring detailed Irish goods-trade data from Eurostat
Comext. The app is aimed at economists and policy analysts: it supports exports
and imports, monthly monitoring and annual comparison, a year-on-year cumulative
growth decomposition by SITC 1-digit section, and drill-downs into detailed
5-digit SITC products and partner-country combinations.

Everything is SITC Rev. 4. The classification levels used are 1-digit sections
(0-9) and 5-digit detailed products, for example `54154`. CN8 customs codes are
not used by the current app.

The app is designed to be shared as source code. Clone the repository, install
the R packages listed below, and run the Shiny app locally.

## Project Structure

```text
.
├── app/
│   ├── app.R              # Shiny app UI and server
│   ├── R/trade_utils.R    # data loading, labels, YTD contribution maths
│   └── data/*.parquet     # processed, app-ready data committed with the repo
├── scripts/
│   └── update_trade_data.R# refresh processed data from the Comext API
├── DESCRIPTION            # declared R dependencies
├── run_app.command        # macOS helper to launch the app
└── README.md
```

The app only needs the `app/` folder to run because the processed parquet files
live in `app/data/`.

## Run Locally

Requires R 4.1 or newer.

Install dependencies once:

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "ggplot2", "plotly", "DT", "scales", "arrow",
  "tidyr", "readr", "forcats", "tibble", "stringr", "lubridate",
  "RColorBrewer"
))
```

Then run the app from the project root:

```bash
R -e 'shiny::runApp("app")'
```

On macOS, you can also double-click `run_app.command`.

The app reads the committed parquet files for bundled totals, SITC sections,
partners, and products. Some detailed product-country drill-downs make live
Eurostat queries at runtime, so those views need an internet connection.

## Share The Code On GitHub

If this folder is not already a Git repository, initialize it from the project
root:

```bash
git init
git branch -M main
git add .
git status
git commit -m "Initial Irish Trade Monitor app"
```

Create an empty GitHub repository, then connect and push:

```bash
git remote add origin https://github.com/YOUR-USERNAME/irish-trade-monitor.git
git push -u origin main
```

The `.gitignore` keeps old raw downloads, generated site files, local editor
files, and other legacy material out of the repository. The app-ready data in
`app/data/` is small enough to commit and is needed for people to run the app
without first rebuilding the data.

## Update The Data

```bash
Rscript scripts/update_trade_data.R
```

The script pulls monthly series from the Eurostat Comext API for imports and
exports, derives annual series by summing monthly observations, refreshes
partner and SITC breakdowns, best-effort fetches product and partner labels, and
writes updated parquet files into `app/data/`.

It resolves the project root from the script location, so it can be launched
from outside the repo as well as from the project root. It needs internet access.
The `xml2` package is required only for the optional label fetch.

`1a_clean.R` is a thin legacy wrapper that sources the refresh script.

## Data Sources

Everything comes from the Eurostat Comext monthly API, dataset `DS-059331`, with
Ireland as reporter. The processed app data covers both flows, imports and
exports. Annual totals are summed from monthly observations so annual and monthly
views remain internally consistent.

For the partner-led tabs, the refresh script bundles monthly and annual SITC
1-digit breakdowns for the top 20 partner markets in the latest complete-year
setup used by the script. This keeps the app fast and the repository manageable
while covering the main monitoring markets.

The detailed `Product x country` and `Country x product` tabs query Eurostat on
demand for selected 5-digit SITC products, partner countries, and periods.

Optional 5-digit product descriptions are fetched from the Comext SITC codelist
and written to `app/data/sitc5_labels.csv`. If the fetch fails, the app still
runs and shows bare codes.

## SITC Contribution Chart

Implemented in `app/R/trade_utils.R::compute_ytd_contributions()`. For each
month `m` in year `t` and section `s`:

1. `YTD_s,t` is cumulative January-to-`m` trade for section `s` in year `t`.
2. `YTD_total,t` is the sum of `YTD_s,t` over all sections.
3. Contribution of section `s`, in percentage points:

```text
contribution_s = 100 * (YTD_s,t - YTD_s,t-1) / YTD_total,t-1
```

Because the section-level changes sum to the total change, the section
contributions sum exactly to the overall YTD year-on-year growth rate shown as
the black line on the chart.

## Important Data Note

The Comext API can return aggregate partner rows such as `WORLD`,
`INT_EU`/`EXT_EU`, `INT_EA`/`EXT_EA`, and EU/euro-area variants. The refresh
script pins `partner = WORLD` for headline total and SITC-level series and
excludes aggregate codes from partner rankings. This avoids double-counting
trade across overlapping aggregate partner groups.

## Known Limitations

- **Provisional months.** Recent months are provisional and may be revised by
  Eurostat.
- **Annuals are summed from monthly.** Full-year displays use the latest year
  with 12 complete months where appropriate.
- **Partner coverage.** Partner-led bundled data is intentionally limited to the
  top partner markets used by the refresh script to keep the app lightweight.
- **Live detailed queries.** Product-country drill-downs need internet access
  because they query Eurostat at runtime.
- **Label fetch is best-effort.** If the Comext codelist is unreachable when the
  script runs, products show as bare 5-digit codes.

## License

MIT.
