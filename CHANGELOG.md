# Changelog

All notable changes to the Irish Trade Monitor are recorded here, newest first.
Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/). Each
entry gives the git commit so any change can be inspected with
`git show <commit>`.

## [Unreleased]

### Changed
- **Restyle: beige removed and presentation tidied** (2026-11-13)
  - Page background changed from beige (`#f6f6f2`) to a cool grey canvas (`#f4f5f7`), with white cards and inputs.
  - Card and key-point borders are thinner and flatter; corners are tighter.
  - Type scale standardised on Lato (labels 0.75rem, secondary 0.8125rem, body 1rem, headline figures 1.75rem).
  - Chart series colours: imports and "Food" move to amber `#c9821a`; purple `#7c62b8`, yellow `#dcb945` and rust `#b5561f` are added to the palette.
  - Presentation only. No change to data, figures, wording, chart structure or page structure.

### Earlier changes since 2.0
- PDF briefing generated from the app's data (`3854bb2`, merge `7b774c3`).
- Renamed the "Pharma & concentration" page to "Concentration" (`f0ce1c2`).
- Department of Finance colour scheme and Lato typeface (`c83c184`).

## [2.0] - 2026-11-11

### Added
- Analytical pages: briefing, trends, concentration and markets (`c7e28f1`).
- Analytics and chart foundation for the redesign (`cfc5c1d`).

## [Baseline] - 2026-11-11

- Snapshot before the analytical redesign (`7066be4`, tag `baseline-2026-11-11`).
