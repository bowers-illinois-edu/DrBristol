# Sensitivity analysis for urn models with two types of information

A function that allows the researcher to calculate the odds of drawing a
working theory supporting observation over a rival theory supporting
observation required to increase a p-value to above a certain value.

## Usage

``` r
sens_obs_two_types(
  obs_support,
  total_obs,
  rival_obs = NULL,
  weights = NULL,
  p_threshold = 0.05
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

- p_threshold:

  A decimal (Double). The p-value threshold. Default is p=.05.

## Value

A list with two elements:

1.  in `w` the differential odds of observing a working theory
    supporting observation over a rival theory supporting observation.

2.  in `p` The p-value given the found `w`.

## Examples

``` r
# What is the odds that would bring our p=.02 to p \approx .05
find_p_two_types(obs_support = 7, total_obs = 10, odds = 1)
#> [1] 0.01864802
sens_obs_two_types(obs_support = 7, total_obs = 10, p_threshold = .05)$w
#> [1] 1.58928
# Notice that this is correct:
find_p_two_types(obs_support = 7, total_obs = 10, odds = 1.58928)
#> [1] 0.04999995
```
