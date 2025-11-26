# Multi-rival testing: Multivariate Approach

This is a test of the composite null hypothesis that at least one rival
theory is more consistent with the data than the working theory: Rival
Theory 1 is true OR Rival Theory 2 is true OR ... Rival k is true. This
function tests only INFORMATIVE observations — those that distinguish
between working theory and rival theories. If you have
neutral/uninformative observations, exclude them before calling this
function. The test is: "Given n informative observations, how likely is
this observed pattern if at least one rival theory is correct?"

## Usage

``` r
find_p_multi_mv(
  obs_support,
  rival_obs = NULL,
  evidence_matrix = NULL,
  weights = NULL,
  odds = 1,
  interpretation = FALSE,
  check_evidence = TRUE,
  messages = TRUE
)
```

## Arguments

- obs_support:

  A vector of integers representing the number of observations made in
  favor of the working hypothesis (anti-rival). Each element corresponds
  to evidence against a specific rival.

- rival_obs:

  Optional. A vector of integers representing the number of observations
  actually made that support each rival. Must be the same length as
  obs_support. If NULL, assumes zero pro-rival observations.

- evidence_matrix:

  Optional. A matrix where rows are evidence types and columns are
  rivals. Entry i,j = 1 if evidence type i argues against rival j, 0
  otherwise. If provided, obs_support is interpreted as counts of each
  evidence type.

- weights:

  Not used yet.

- odds:

  The odds ratio for sampling (default 1 for central hypergeometric).
  Currently a scalar indicating odds of seeing any working theory
  observations versus any rivals. To complicate in future versions.

- interpretation:

  Logical. If TRUE, returns interpretation text with p-value.

- check_evidence:

  Logical. If TRUE, checks if obs_support are identical across all
  rivals and suggests using find_p_multi_max_p() instead. By pass this
  message, set `check_evidence=FALSE` and `messages=FALSE`

- messages:

  Logical. If TRUE, prints out a long message if `obs_support` is
  identical across all rivals and `rival_obs=NULL`. If FALSE, doesn't
  print that message.

## Value

Either a numeric p-value (scalar) or a list with p-value and
interpretation.

## Details

We reject this composite hypothesis if we can reject all of the
individual hypotheses. There are two main approaches to this problem:
(1) Do the individual tests and use the maximum p-value (see Berger on
intersection-union tests and FWER control) or (2) represent this test as
a single test using a multivariate distribution. This function
implements the second option using the multivariate hypergeometric
distribution.

The rejection region includes all outcomes with:

- Anti-rival evidence \>= observed levels for ALL rivals (componentwise)

- Total pro-rival observations = observed total (or zero if none
  observed)

- All possible allocations of pro-rival observations across rival types

On interpretation:

- If p \> alpha: "We cannot reject the union null hypothesis. The
  observed evidence pattern is not sufficiently unlikely under the
  hypothesis that at least one rival theory is correct. Therefore, we do
  not have strong enough evidence to rule out all rival theories."

- If p \<= alpha: "We reject the union null hypothesis. The observed
  evidence pattern would occur by chance only 100\*p% of the time if at
  least one rival theory were actually correct. This provides strong
  statistical evidence that all rival theories are wrong and the working
  theory is the correct explanation."

We assume a conservative urn model where the number of pro-rival
observations in the population is obs_support + 1 for each rival
(following Lopez and Bowers 2026).

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
find_p_multi_mv(obs_support = c(10, 10, 10, 10))
#> It looks like you have only one kind of evidence that is inconsistent with multiple rivals. You would over-state your evidence against the rivals if we used a multivariate null-model by repeating the same number of anti-Rival observations. We are reporting here the p-value from the find_p_multi_max_p command. If you actually do have multiple types of observations but happen to have the same numbers of them, then you should try this command again but set check_evidence=FALSE and messages=FALSE
#> [1] 2.835142e-06
find_p_two_types(obs_support = 10, total_obs = 10)
#> [1] 2.835142e-06
find_p_multi_max_p(obs_support = rep(10, 4), total_obs = rep(10, 4), odds = 2)
#> [1] 7.707336e-05
find_p_multi_mv(obs_support = c(10, 10, 10, 10), odds = 2, messages = FALSE)
#> [1] 7.707336e-05
# Example 2: No pro-rival observations made, different anti-rival evidence levels
find_p_multi_mv(obs_support = c(4, 3, 2, 1))
#> [1] 5.098773e-07

# Example 3: With pro-rival observations
find_p_multi_mv(obs_support = c(4, 3, 2, 1), rival_obs = c(1, 1, 0, 0))
#> [1] 3.36519e-05

# Example 4: With pro-rival observations and non-uniform odds
find_p_multi_mv(obs_support = c(4, 3, 2, 1), rival_obs = c(1, 1, 0, 0), odds = 2)
#> [1] 0.0007486185
find_p_multi_mv(obs_support = c(4, 3, 2, 1), rival_obs = c(1, 1, 0, 0), odds = .5)
#> [1] 7.310727e-07
evidence_patterns <- rbind(
  c(1, 1, 0), # antiMiasma and_antiFood
  c(1, 0, 0), # antiMiasma_only
  c(0, 0, 1) # antiAnimal_only
)
colnames(evidence_patterns) <- c("Miasma", "Food", "Animal")

observed_counts <- c(4, 1, 2) # 4 overlap, 1 Miasma-only, 2 Animal-only
result <- find_p_multi_mv(
  obs_support = observed_counts,
  evidence_matrix = evidence_patterns,
  interpretation = TRUE
)

print(result$interpretation)
#> [1] "Evidence pattern (overlapping):\n  4 observations argue against: Miasma & Food\n  1 observations argue against: Miasma\n  2 observations argue against: Animal\n\nNet evidence against each rival:\n  Miasma: 5 pieces\n  Food: 4 pieces\n  Animal: 2 pieces\n\nP-value: 0.0004\n\nInterpretation: We reject the union null hypothesis.\n"
print(result$p)
#> [1] 0.0003869969
```
