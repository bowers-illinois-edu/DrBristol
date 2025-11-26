# Calculate the difference between the p-value and a p-value threshold for the multiple types of information model

A helper function for
[`sens_obs_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/sens_obs_two_types.md)
see
[`find_p_multi_mv()`](https://bowers-illinois-edu.github.io/DrBristol/reference/find_p_multi_mv.md)
for details.

## Usage

``` r
find_odds_multi(omega, obs_support, rival_obs, weights, p_threshold)
```

## Arguments

- omega:

  Candidate odds ratio being evaluated by the root finder.

- obs_support:

  A vector of integers representing the number of observations made in
  favor of the working hypothesis (anti-rival). Each element corresponds
  to evidence against a specific rival.

- rival_obs:

  Optional. A vector of integers representing the number of observations
  actually made that support each rival. Must be the same length as
  obs_support. If NULL, assumes zero pro-rival observations.

- weights:

  Not used yet.

- p_threshold:

  A number between 0 and 1 indicating the critical value of the test.
