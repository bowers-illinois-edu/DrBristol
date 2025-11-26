# Multi-rival testing: Max-P approach

This is a test of the composite or null hypothesis that at least one
rival theory is more consistent with the data than the working theory:
or Rival Theory 1 is true OR Rival Theory 2 is true OR ... Rival k is
true.

## Usage

``` r
find_p_multi_max_p(
  obs_support,
  total_obs,
  rival_obs = NULL,
  odds = 1,
  weights = NULL
)
```

## Arguments

- obs_support:

  A vector of integers representing the number of observations in favor
  of the working hypothesis. Each element must be less than or equal to
  the corresponding element in `total_obs`.

- total_obs:

  An vector of integers representing the total number of observations
  made. Its elements can be can be greater than or equal to the
  `obs_support`.

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

## Value

A numeric p-value equal to the maximum across the rival-specific tests.

## Details

We reject this composite hypothesis if we can reject all of the
individual hypotheses. There are two main approaches to this problem:
(1) Do the individual tests and use the maximum p-value (TODO cite
Berger on why the intersection-union test has controlled FWER) or (2)
represent this test as a single test using a multivariate distribution.
This function implements the first option by calling
[`find_p_two_types()`](https://bowers-illinois-edu.github.io/DrBristol/reference/find_p_two_types.md)
for each element of the vectors provided below. This approach tends to
be very conservative.

## Examples

``` r
# Example 1:
# One kind of working theory supporting information that argues against multiple rivals where
# each rival has the same amount of information.
# Notice that we will get the same answer as if we used `find_p_two_types()` directly.
# But we present this here to illustrate.
# 4 rivals, 10 observations of one kind of working theory supporting
# observation, 10 total observations made
find_p_multi_max_p(obs_support = rep(10, 4), total_obs = rep(10, 4))
#> [1] 2.835142e-06
find_p_two_types(obs_support = 10, total_obs = 10)
#> [1] 2.835142e-06
# Example 2:
# 4 kinds of working theory supporting observations, each of which is
# inconsistent with a single rival, only working theory supporting observations
# made. Notice the max p:
find_p_multi_max_p(obs_support = c(4, 3, 2, 1), total_obs = c(4, 3, 2, 1))
#> [1] 0.3333333
find_p_two_types(obs_support = 4, total_obs = 4)
#> [1] 0.007936508
find_p_two_types(obs_support = 3, total_obs = 3)
#> [1] 0.02857143
find_p_two_types(obs_support = 2, total_obs = 2)
#> [1] 0.1
find_p_two_types(obs_support = 1, total_obs = 1)
#> [1] 0.3333333
```
