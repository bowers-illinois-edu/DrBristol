# Changelog

## DrBristol (Version 0.0.1.3001)

- Cleaned up e-value sensitivity helpers to use the `sens_obs_*` APIs
  throughout (docs, tests, and runtime code) and forward
  `rival_obs`/`weights` arguments for parity with the p-scale helpers.
- Regenerated documentation after removing stale `sens_urn` references
  and ensured `devtools::document()` runs without warnings.
- Added explicit [`return()`](https://rdrr.io/r/base/function.html)
  statements across e-value helpers to keep control flow clear.

## DrBristol (Version 0.0.1.3000)

- Handles multiple kinds of evidence against multiple rivals including
  overlapping pieces of evidence.
