# Test the p creation function

## The next lines are for use when creating the tests. Change interactive<-FALSE for production
interactive <- FALSE
if (interactive) {
  library(devtools)
  load_all() ## use  this during debugging
}

test_that("It gives the correct p-value", {
  res <- find_p_two_types(obs_support = 7, total_obs = 10)
  expect_true(all.equal(res, 0.018648018648018651472))
})

test_that("It prints the correct interpretation", {
  res <- find_p_two_types(obs_support = 7, total_obs = 10, interpretation = TRUE)
  expect_true(all.equal(res[["thep"]], 0.018648018648018651472))
  expect_true(all.equal(res[["interp"]], "The probability of drawing 7 observations which support the working theory from an urn model supporting a rival theory, where the odds of observing working theory information is odds=1, is p=0.0186"))
})


test_that("P-values with odds < 1 are smaller than p-values with odds > 2", {
  res_odds_half <- find_p_two_types(obs_support = 7, total_obs = 10, odds = .5, interpretation = FALSE)
  res_odds_equal <- find_p_two_types(obs_support = 7, total_obs = 10, odds = 1, interpretation = FALSE)
  res_odds_double <- find_p_two_types(obs_support = 7, total_obs = 10, odds = 2, interpretation = FALSE)
  expect_lt(res_odds_half, res_odds_equal)
  expect_lt(res_odds_equal, res_odds_double)
})

test_that("Warnings work", {
  expect_error(find_p_two_types(obs_support = 10, total_obs = 5))
  expect_error(find_p_two_types(obs_support = 2, total_obs = 10))
})



find_odds <- function(omega, m1, m2, n, x, alpha_thresh = .05, alpha_adjust = 1) {
  # m1= number of pieces of evidence supporting the working theory
  # m2 = number of pieces of evidence supporting the rival theory
  # n = number of pieces drawn from the urn
  # odds = odds of drawing evidence supporting the working theory versus rival theory
  # x = number of pieces of evidence supporting the working theory in the sample of size n
  p_found <- dFNCHypergeo(x = x, m1 = m1, m2 = m2, n = n, odds = omega)
  critical_value <- alpha_thresh / alpha_adjust
  return(p_found - critical_value)
}

found_odds_ex1 <- uniroot(
  f = find_odds, m1 = 2, m2 = 3, n = 3, x = 2,
  interval = c(.0001, 10), trace = 2, extendInt = "upX"
)$root

the_found_dens_ex1 <- dFNCHypergeo(c(0, 1, 2), m1 = 2, m2 = 3, n = 3, odds = found_odds_ex1)


library(partitions)
# Define the urn composition
urn <- c(2, 3) # 2 red, 3 black
# Enumerate possible draws without replacement
draws <- S(urn, 2) # Draw 2 balls
print(draws)


obs_working <- 3
obs_sampled <- 4
rival_obs <- 1

obs_weights <- c(10, 1, 1)

## An observation of working obs 1 is worth 10 of either obs 2 or obs 3 in
## regards evidence against the rival. It is as if if we draw working obs 1
## (w1) we are extra surprised to see it under the rival theory. Like the
## number of rival items is **larger** than we thought. So, maybe if our set of
## 4 sampled includes w2 or w3 then the rival size is still 4, but if the set
## of 4 sampled includes w1, then the urn model includes more rival --- like
## max(c(sum(obs_weights),obs_working+1,rival_obs)). So, here it would have 12
## obs rather than 7.

## So we can imagine this model this as two steps: First draw from an urn representing whether
## you will draw from an urn containing w1 versus not w1. And second draw from
## the chosen urn with rival=7 or 12 depending on whether or not it contains w1
## or not respectively. An issue with the two step idea: If you are asking probabability of seeing 3 and you
## actually observed 3, then you have to have the possibility of sampling 3
## from the urn. You cannot exclude any from the urn. If you choose an urn
## containing w1: then you can sample 4 from that urn If you choose an urn
## without w1: then you can never see 3 working theory supporting obs.

## I think it is easier to think of obs_weights as saying that seeing all of these
## three including w1 would be more surprising than if they were all equally
## weighted.

## I think it is ok to have many pieces of information that have weights less
## than 1, but I think we don't want rival size to be less than working size
## otherwise it doesn't feel like a test of the rival. So, we can print out a
## warning for such "straw in the wind" style of situations. Really, the
## ability to have multiple items with less than 1 weight is to counteract one
## or a few observations with very high evidentiary weight.

stopifnot(length(obs_weights) == obs_working)

## We are twice as likely to observe working theory supporting information
working_odds <- 2

rival_obs <- max(rival_obs, obs_working + 1)
total_obs <- obs_working + rival_obs

## samples of size 4 from total 7 where 3 are working obs but where working obs
## 1 has weight 10. All working theory obs are twice as easy to observe working
## theory supporting evidence compared to the rival: equally easy to observe
## any working theory item compared to any rival theory item in this binary
## example. To change the odds I'd suggest we focus on "item type" in the
## multivariate model so each type gets an odds.

## So is this:

## (1) an Urn with 10+1+1 working theory items and 4 rival items? I
## don't think so. It is not a test of the rival in this case --- it will be
## very probable to see 3 working theory items in a set of 4 sampled.


dFNCHypergeo(
  x = obs_working, m1 = obs_working, m2 = rival_obs,
  n = obs_sampled, odds = working_odds
)
dFNCHypergeo(
  x = obs_working, m1 = obs_working, m2 = sum(obs_weights),
  n = obs_sampled, odds = working_odds
)

## Compare when odds=1
dFNCHypergeo(
  x = obs_working, m1 = obs_working, m2 = rival_obs,
  n = obs_sampled, odds = 1
)
dFNCHypergeo(
  x = obs_working, m1 = obs_working, m2 = sum(obs_weights),
  n = obs_sampled, odds = 1
)
