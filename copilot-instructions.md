# DrBristol Copilot Instructions

## Package Purpose & Layout

- Statistical R package for Fisher-style case-study inference using
  biased and unbiased urn models; all exported functions live in `R/`,
  documentation is roxygen-driven with outputs in `man/`.
- Two major workflows: binary evidence vs. multiple rivals
  (`R/p_binary.R`, `R/p_multiple_maxp.R`, `R/p_multiple_mv.R`) and
  derived sensitivity analyses/e-values (`R/sens_binary.R`,
  `R/sens_multi.R`, `R/evalues.R`).
- Tests are under `tests/testthat/` and mirror the main entry points;
  keep new functionality covered there before running package checks.

## Key Statistical Pipelines

- [`find_p_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/find_p_two_types.md)
  computes conservative p-values for supportive vs. rival evidence using
  [`BiasedUrn::dFNCHypergeo`](https://rdrr.io/pkg/BiasedUrn/man/BiasedUrn-2-Univariate.html);
  callers such as
  [`find_e_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/find_e_two_types.md)
  and
  [`sens_obs_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/sens_obs_two_types.md)
  depend on its exact argument contract (`weights` length equals
  `obs_support`, `obs_support >= total_obs/2`).
- Multi-rival paths
  ([`find_p_multi_max_p()`](https://bowers-illinois-edu.github.io/DrBristol/reference/find_p_multi_max_p.md),
  [`find_p_multi_mv()`](https://bowers-illinois-edu.github.io/DrBristol/reference/find_p_multi_mv.md))
  either take maxima of rival-specific urn tests or evaluate the
  multivariate hypergeometric via
  [`BiasedUrn::dMFNCHypergeo`](https://rdrr.io/pkg/BiasedUrn/man/BiasedUrn-3-Multivariate.html);
  [`generate_compositions()`](https://bowers-illinois-edu.github.io/DrBristol/reference/generate_compositions.md)
  enumerates rival evidence allocations when pro-rival observations
  exist.
- E-value helpers
  ([`e_from_p()`](https://bowers-illinois-edu.github.io/DrBristol/reference/e_from_p.md),
  [`find_e_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/find_e_two_types.md),
  [`sens_urn_evalue()`](https://bowers-illinois-edu.github.io/DrBristol/reference/sens_urn_evalue.md))
  always start from a p-value upper bound, then apply deterministic
  calibrations; keep calibration choices (`inverse`, `kappa`) explicit
  so downstream text interpretations stay consistent.

## Sensitivity Functions

- Sensitivity helpers are
  [`sens_obs_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/sens_obs_two_types.md)
  and
  [`sens_obs_multi()`](https://bowers-illinois-edu.github.io/DrBristol/reference/sens_obs_multi.md)
  (see `R/sens_binary.R` and `R/sens_multi.R`); there is no legacy
  `sens_urn` function, so wrap new odds-ratio solvers around these
  implementations.
- Both helpers root-find via
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html) over
  `find_odds_*()` shims; keep return lists with `w` (odds) and `p` to
  satisfy existing tests (`tests/testthat/test-sensitivity-analysis.R`).

## Coding Patterns & Conventions

- Stick to base R, roxygen comments (`#'`) above each function, and
  explicit argument validation via
  [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) or
  [`stop()`](https://rdrr.io/r/base/stop.html) with informative
  messages.
- `interpretation = TRUE` options are expected to return either
  `list(thep=..., interp=...)` or print human-readable summaries; keep
  message wording stable for regression tests (see
  `tests/testthat/test_thep.R`).
- Default `weights` are `rep(1, obs_support)`; when allowing custom
  weights ensure they remain numeric, length-matched, and sum to at
  least `obs_support` (mirrors current guards).
- Multi-rival evidence sometimes collapses to the Max-P shortcut when
  `obs_support` vectors are uniform and `rival_obs` absent; respect the
  `check_evidence/messages` flags to avoid over-counting evidence.

## Build & Test Workflow

- Use `make dependencies`, `make test`, `make check`, `make document`,
  and `make build` to drive devtools commands (see `Makefile`); these
  call `R -q -e "devtools::FUNCTION()"` behind the scenes.
- For quick iteration inside R, run `devtools::load_all()` followed by
  `devtools::test()`; `testthat` edition 3 is configured via
  `DESCRIPTION`.
- Before submitting changes, ensure `R CMD check` passes (via
  `make check`) because exported functions are used by pkgdown
  (`_pkgdown.yml`) and the GitHub Pages site.

## External Dependencies & Integration Points

- Core probability calculations rely on `BiasedUrn` and `partitions`;
  confirm these packages are installed when adding new functionality or
  tests.
- The package exposes calibration utilities (`e_from_p`) meant to be
  reusable by external analysts, so avoid breaking function signatures
  referenced in `NAMESPACE`.
- pkgdown site uses Bootstrap 5; if you add articles or vignettes, keep
  the configuration in `_pkgdown.yml` synchronized.

## Common Pitfalls

- [`sens_urn_evalue()`](https://bowers-illinois-edu.github.io/DrBristol/reference/sens_urn_evalue.md)
  should ultimately call
  [`sens_obs_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/sens_obs_two_types.md);
  avoid referencing non-existent helpers named `sens_urn`.
- When extending multi-rival logic, remember that
  [`find_p_multi_mv()`](https://bowers-illinois-edu.github.io/DrBristol/reference/find_p_multi_mv.md)
  requires `rival_obs` length `k` and evidence matrices with only 0/1
  entries; invalid shapes will trigger `stopifnot` guards.
- Tests frequently assert monotonic relationships between odds and
  p-values; preserve these invariants when refactoring numerical
  routines.
