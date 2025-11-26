# Calculate the difference between the p-value and a p-value threshold for the two types of observation model

A helper function for
[`sens_obs_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/sens_obs_two_types.md)
see
[`find_p_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/find_p_two_types.md)
for details.

## Usage

``` r
find_odds_two_types(
  omega,
  obs_support,
  total_obs,
  rival_obs,
  weights,
  p_threshold
)
```

## Arguments

- omega:

  Candidate odds ratio being evaluated by the root finder.

- obs_support:

  An integer representing the number of observations in favor of the
  working hypothesis. Must be less than or equal to `total_obs`.

- total_obs:

  An integer representing the total number of observations made. It can
  be greater than or equal to the `obs_support`.

- rival_obs:

  Optional. An integer representing the number of observations in the
  urn that do not support the working theory.

- weights:

  Double vector. Default is equal weight for each observation when
  weights=NULL is `rep(1,obs_support)`. To indicate that one observation
  should have twice the weight of any other one might use
  `rep(c(2,1),c(1,obs_support-1))`

- p_threshold:

  A number between 0 and 1 indicating the critical value of the test.
