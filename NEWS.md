# DrBristol (Version 0.0.1.3001)

* Cleaned up e-value sensitivity helpers to use the `sens_obs_*` APIs throughout (docs, tests, and runtime code) and forward `rival_obs`/`weights` arguments for parity with the p-scale helpers.
* Regenerated documentation after removing stale `sens_urn` references and ensured `devtools::document()` runs without warnings.
* Added explicit `return()` statements across e-value helpers to keep control flow clear.

# DrBristol (Version 0.0.1.3000)

* Handles multiple kinds of evidence against multiple rivals including overlapping pieces of evidence.

# DrBristol (Version: 0.0.1.1000)

* Adding ability to specify size of rival urn to enable omnibus tests of working versus any rival including sensitivity analysis.

# DrBristol (Version: 0.0.1.0000)

* Alpha version with full functionality as of Lopez and Bowers (2025)
