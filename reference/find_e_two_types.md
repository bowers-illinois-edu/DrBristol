# E-values for the +1 hypergeometric urn model

Compute e-values for the DrBristol +1 hypergeometric urn model by first
obtaining the conservative upper bound on the p-value under the rival
theory and then calibrating that p-value into an e-value.

Concretely, this function:

1.  Calls
    [`find_p_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/find_p_two_types.md)
    with `interpretation = FALSE` to obtain the upper bound
    \\p\_{\max}\\ on the p-value under the +1 urn model.
    [oai_citation:4‡p_value_process_tracing.pdf](file-service://file-QZ1mjQ3FjfNcsuGnfUiwoe)

2.  Applies
    [`e_from_p()`](https://bowers-illinois-edu.github.io/DrBristol/reference/e_from_p.md)
    to \\p\_{\max}\\ using the chosen calibration (inverse or
    κ-calibrator).

Because \\p\_{\max}\\ is conservative, the resulting e-value is likewise
conservative: it is the *minimum* e-value consistent with the +1 urn
model and the chosen calibrator.

## Usage

``` r
find_e_two_types(
  obs_support,
  total_obs,
  weights = NULL,
  odds = 1,
  method = c("inverse", "kappa"),
  kappa = 0.5,
  cap = Inf,
  interpretation = FALSE
)
```

## Arguments

- obs_support:

  An integer representing the number of observations in favor of the
  working hypothesis. Must be less than or equal to `total_obs`.

- total_obs:

  An integer representing the total number of observations made. It can
  be greater than or equal to the `obs_support`.

- weights:

  Double vector. Default is equal weight for each observation when
  weights=NULL is `rep(1,obs_support)`. To indicate that one observation
  should have twice the weight of any other one might use
  `rep(c(2,1),c(1,obs_support-1))`

- odds:

  The odds of observing a rival versus working-theory observation. This
  can be interpreted as "bias" in observation. Or "relative ease" of
  observation.

- method:

  Character; passed to
  [`e_from_p()`](https://bowers-illinois-edu.github.io/DrBristol/reference/e_from_p.md)
  (either `"inverse"` or `"kappa"`).

- kappa:

  Numeric; κ parameter for the `"kappa"` method.

- cap:

  Upper cap for the returned e-value; passed to
  [`e_from_p()`](https://bowers-illinois-edu.github.io/DrBristol/reference/e_from_p.md).

- interpretation:

  Logical. If `FALSE` (default), return a numeric e-value. If `TRUE`,
  return a list with the e-value and a human-readable character string
  summarizing the result.

## Value

If `interpretation = FALSE`, a single numeric e-value. If
`interpretation = TRUE`, a list with components:

- `e`: the e-value (numeric scalar).

- `interp`: a character string describing the result.

## See also

[`find_p_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/find_p_two_types.md),
[`e_from_p()`](https://bowers-illinois-edu.github.io/DrBristol/reference/e_from_p.md),
[`sens_urn_evalue()`](https://bowers-illinois-edu.github.io/DrBristol/reference/sens_urn_evalue.md)

## Examples

``` r
# Basic: 7 supportive observations out of 10 total
find_e_two_types(obs_support = 7, total_obs = 10)
#> [1] 53.625

# With textual interpretation
res <- find_e_two_types(
    obs_support = 7, total_obs = 10,
    interpretation = TRUE
)
res$e
#> [1] 53.625
res$interp
#> [1] "Under the +1 hypergeometric urn model, the upper bound on the p-value is p <= 0.01865. Using method='inverse', this corresponds to an e-value of e = 53.62."

# Using κ-calibration
find_e_two_types(
    obs_support = 7, total_obs = 10,
    method = "kappa", kappa = 0.5
)
#> [1] 3.661455
```
