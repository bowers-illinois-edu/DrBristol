# Sensitivity analysis in the e-value scale

Wrapper around
[`sens_obs_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/sens_obs_two_types.md)
that works on the e-value scale. For a given e-value threshold
\\e\_\star\\, this function finds the odds ratio \\\omega\\ at which the
conservative e-value (as returned by
[`find_e_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/find_e_two_types.md))
would be equal to \\e\_\star\\.

Because the underlying calibration is via \\p\_{\max}\\ and
[`e_from_p()`](https://bowers-illinois-edu.github.io/DrBristol/reference/e_from_p.md),
this is equivalent to asking for the odds ratio at which the p-value
upper bound would be \\\alpha\_\star = 1 / e\_\star\\, and then calling
[`sens_obs_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/sens_obs_two_types.md)
with `p_threshold = alpha_star`.
[oai_citation:5‡p_value_process_tracing.pdf](file-service://file-QZ1mjQ3FjfNcsuGnfUiwoe)

## Usage

``` r
sens_urn_evalue(
  obs_support,
  total_obs,
  rival_obs = NULL,
  weights = NULL,
  e_threshold
)
```

## Arguments

- obs_support:

  An integer representing the number of observations in favor of the
  working hypothesis. Must be less than or equal to the total.

- total_obs:

  An integer representing the total number of observations

- rival_obs:

  Optional. The number of observations in the urn that do not support
  the working theory.

- weights:

  A vector of numeric weights representing the differential evidentiary
  weight of the working theory supporting observations.

- e_threshold:

  Numeric scalar \\\> 0\\. Target e-value at which to assess
  sensitivity.

## Value

Whatever
[`sens_obs_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/sens_obs_two_types.md)
returns (typically a list including the odds-ratio \\\omega\\); the only
difference is that you parameterize the problem by an e-value threshold
rather than a p-value threshold.

## Examples

``` r
# Suppose we observed 7 supportive pieces of evidence out of 10.
# How large must the observation bias be for the e-value to drop to ~2
# (i.e. roughly p >= 0.5)?
sens_urn_evalue(obs_support = 7, total_obs = 10, e_threshold = 2)
#> $w
#> [1] 10.84011
#> 
#> $p
#> [1] 0.4999997
#> 
```
