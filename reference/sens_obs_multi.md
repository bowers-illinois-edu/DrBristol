# Sensitivity analysis for urn models with two types of information

A function that allows the researcher to calculate the odds of drawing a
working theory supporting observation over a rival theory supporting
observation required to increase a p-value to above a certain value.

## Usage

``` r
sens_obs_multi(
  obs_support,
  rival_obs = NULL,
  weights = NULL,
  p_threshold = 0.05
)
```

## Arguments

- obs_support:

  An integer representing the number of observations in favor of the
  working hypothesis. Must be less than or equal to the total.

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
find_p_multi_mv(obs_support = c(4, 3, 2, 1), odds = 1)
#> [1] 5.098773e-07
find_p_multi_mv(obs_support = c(4, 3, 2, 1), odds = 2)
#> [1] 2.036757e-05
sens_obs_multi(obs_support = c(4, 3, 2, 1), p_threshold = .05)$w
#> [1] 22.53507
# Notice that this is correct:
find_p_multi_mv(obs_support = c(4, 3, 2, 1), odds = 22.535)
#> [1] 0.04999969
sens_obs_multi(obs_support = c(4, 3, 2, 1), rival_obs = c(1, 1, 0, 0), p_threshold = .05)$w
#> [1] 8.168784
sens_obs_multi(obs_support = c(4, 4, 4, 4), p_threshold = .05)
#> $w
#> [1] 2.596286
#> 
#> $p
#> [1] 0.04999972
#> 
find_p_multi_mv(obs_support = c(4, 4, 4, 4), odds = 2.6)
#> It looks like you have only one kind of evidence that is inconsistent with multiple rivals. You would over-state your evidence against the rivals if we used a multivariate null-model by repeating the same number of anti-Rival observations. We are reporting here the p-value from the find_p_multi_max_p command. If you actually do have multiple types of observations but happen to have the same numbers of them, then you should try this command again but set check_evidence=FALSE and messages=FALSE
#> [1] 0.05011704
```
