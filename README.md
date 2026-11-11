# Irish Trade Monitor

An R Shiny app for monitoring Irish goods-trade data from Eurostat Comext, built
for economists and policy analysts.

Version 2.0 is organised around the questions asked on release day:

- **Briefing** - the month in one page: headline, smoothed and underlying
  measures, auto-drafted key points, growth drivers, biggest movers.
- **Trends** - signal versus noise for any series: 3- and 12-month averages,
  growth on three bases, this year against the previous five, a momentum
  heatmap, growth contributions and a breadth-of-growth index.
- **Pharma & concentration** - composition, product and market concentration,
  what is inside each product group, value versus volume versus unit value.
- **Markets** - partner-group shares, front-loading and payback, bilateral
  balances and country profiles.
- **Explorer** - the original drill-down pages (monthly movers, annual
  comparison, by country, product by country, product over time).

Everything is SITC Rev. 4. The app uses 1-digit sections, 5-digit detailed
products, and ten analytical product groups built from them (defined in
`app/R/analytics.R` and on the app's About page). CN8 customs codes are not used.

## Project Structure

```text
.
├── app/
│   ├── app.R                # app shell, navigation and the Explorer pages
│   ├── R/trade_utils.R      # data loading, labels, Comext API helpers
│   ├── R/analytics.R        # analytical groups, transforms, contributions,
│   │                        #   concentration, unit values, event list
│   ├── R/charts.R           # chart toolkit: one house style for every chart
│   ├── R/mod_briefing.R     # Briefing page
│   ├── R/mod_trends.R       # Trends page
│   ├── R/mod_structure.R    # Pharma & concentration page
│   ├── R/mod_markets.R      # Markets page
│   └── data/*.parquet       # processed, app-ready data committed with the repo
├── scripts/
│   ├── update_trade_data.R  # refresh processed data from the Comext API
│   └── precompute_drivers.R # pre-compute the monthly-mover decompositions
├── tests/                   # plain Rscript checks, one per layer / page
├── DESCRIPTION              # declared R dependencies
├── run_app.command          # macOS helper to launch the app
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

## Version Control

The project is tracked in Git; the old dated folder copies are no longer needed.

- `main` is the stable version. The tag `baseline-2026-11-11` marks the app
  exactly as it was before the version 2.0 redesign.
- The redesign was developed on the branch `analytical-redesign`.

Useful commands, run from the project root:

```bash
git status                       # what has changed since the last commit
git log --oneline                # history
git switch main                  # go back to the pre-redesign app
git switch analytical-redesign   # return to the redesigned app
```

To save a data refresh or a change:

```bash
git add -A
git commit -m "Refresh data to <month>"
```

`.gitignore` keeps local deployment helpers (`git/`, which holds account
tokens), the old folder backups (`z99_backups/`), raw downloads and editor files
out of the repository. Nothing is published to GitHub until you run `git push`.

## Tests

```bash
Rscript tests/test_foundation.R
Rscript tests/test_mod_briefing.R
Rscript tests/test_mod_trends.R
Rscript tests/test_mod_structure.R
Rscript tests/test_mod_markets.R
```

Each script rebuilds the analytical series from the bundled data and checks
them against brute-force recalculations, then forces every chart to build.

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
