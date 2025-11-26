# Find a p-value given a certain number of observations in favor of the working hypothesis among a total number of observations when there are two types of observations

When an observation can either support a working theory or a rival
theory, this function returns the p-value summarizing the minimum
evidence against the rival theory provided by the working theory. That
is the function returns p=.01 we should take this as p\<=.01.

## Usage

``` r
find_p_two_types(
  obs_support,
  total_obs,
  rival_obs = NULL,
  odds = 1,
  weights = NULL,
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

- rival_obs:

  Optional. An integer representing the number of observations in the
  urn that do not support the working theory.

- odds:

  The odds of observing a rival versus working-theory observation. This
  can be interpreted as "bias" in observation. Or "relative ease" of
  observation.

- weights:

  Double vector. Default is equal weight for each observation when
  weights=NULL is `rep(1,obs_support)`. To indicate that one observation
  should have twice the weight of any other one might use
  `rep(c(2,1),c(1,obs_support-1))`

- interpretation:

  Logical. TRUE if the function returns text helping to interpret the
  result, FALSE (default option) to returns only the p-value

## Value

Either a p-value (numeric, scalar) or a list containing the p-value and
text containing an interpretation

## Details

This function accommodates urns where working theory supporting
observations are systematically easier or more difficulty to observe
that rival supporting observations via the `odds` argument. And it also
supports designs where one observation out of the total supporting the
working theory is particularly compelling (a "smoking gun"). For
example, if observation 1 is worth 2 other observations, the urn can
reflect this by increasing the number of observations supporting the
rival theory in the urn, accounting for the weight of that smoking gun
observation using the `weights` argument.

The function allows the total number of observations (`total_obs`) to be
larger than the number supporting the working theory (`obs_support`).
This is a design element.

## Examples

``` r
# Equal probability
find_p_two_types(obs_support = 7, total_obs = 10)
#> [1] 0.01864802
# Equal probability with interpretation printed
find_p_two_types(obs_support = 7, total_obs = 10, interpretation = TRUE)
#> The maximum probability of drawing 7 observations which support the working theory from an urn model supporting a rival theory, where the odds of observing working theory information is odds=1 and evidentiary weights are (1,1,1,1,1,1,1), is p <=0.0186.
#> $thep
#> [1] 0.01864802
#> 
#> $interp
#> [1] "The maximum probability of drawing 7 observations which support the working theory from an urn model supporting a rival theory, where the odds of observing working theory information is odds=1 and evidentiary weights are (1,1,1,1,1,1,1), is p <=0.0186."
#> 
# Equal probability where total obs is equal to the number of observations #
# supporting the working theory. Notice that this is less conservative than
# the above.
find_p_two_types(obs_support = 7, total_obs = 7, interpretation = FALSE)
#> [1] 0.0001554002
# Unequal probability, 2 kinds of evidence with interpretation printed
find_p_two_types(obs_support = 7, total_obs = 10, interpretation = TRUE, odds = .5)
#> The maximum probability of drawing 7 observations which support the working theory from an urn model supporting a rival theory, where the odds of observing working theory information is odds=0.5 and evidentiary weights are (1,1,1,1,1,1,1), is p <=0.003.
#> $thep
#> [1] 0.002989537
#> 
#> $interp
#> [1] "The maximum probability of drawing 7 observations which support the working theory from an urn model supporting a rival theory, where the odds of observing working theory information is odds=0.5 and evidentiary weights are (1,1,1,1,1,1,1), is p <=0.003."
#> 
find_p_two_types(obs_support = 7, total_obs = 10, interpretation = TRUE, odds = 2)
#> The maximum probability of drawing 7 observations which support the working theory from an urn model supporting a rival theory, where the odds of observing working theory information is odds=2 and evidentiary weights are (1,1,1,1,1,1,1), is p <=0.0761.
#> $thep
#> [1] 0.07612251
#> 
#> $interp
#> [1] "The maximum probability of drawing 7 observations which support the working theory from an urn model supporting a rival theory, where the odds of observing working theory information is odds=2 and evidentiary weights are (1,1,1,1,1,1,1), is p <=0.0761."
#> 
# Equal probability, Unequal evidentiary weight, 2 kinds of evidence
find_p_two_types(
  obs_support = 7, total_obs = 10, weights = rep(1, 7),
  interpretation = TRUE, odds = 1
)
#> The maximum probability of drawing 7 observations which support the working theory from an urn model supporting a rival theory, where the odds of observing working theory information is odds=1 and evidentiary weights are (1,1,1,1,1,1,1), is p <=0.0186.
#> $thep
#> [1] 0.01864802
#> 
#> $interp
#> [1] "The maximum probability of drawing 7 observations which support the working theory from an urn model supporting a rival theory, where the odds of observing working theory information is odds=1 and evidentiary weights are (1,1,1,1,1,1,1), is p <=0.0186."
#> 
find_p_two_types(
  obs_support = 7, total_obs = 10,
  weights = rep(c(2, 1), c(1, 7 - 1)), interpretation = TRUE, odds = 1
)
#> The maximum probability of drawing 7 observations which support the working theory from an urn model supporting a rival theory, where the odds of observing working theory information is odds=1 and evidentiary weights are (2,1,1,1,1,1,1), is p <=0.0105.
#> $thep
#> [1] 0.01048951
#> 
#> $interp
#> [1] "The maximum probability of drawing 7 observations which support the working theory from an urn model supporting a rival theory, where the odds of observing working theory information is odds=1 and evidentiary weights are (2,1,1,1,1,1,1), is p <=0.0105."
#> 
```
