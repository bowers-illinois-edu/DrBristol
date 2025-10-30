# Test the p creation function

## The next lines are for use when creating the tests. Change interactive<-FALSE for production
## interactive <- FALSE
## if (interactive) {
##   devtools::load_all() ## use  this during debugging
## }

test_that("It gives the correct p-value", {
  res <- find_p_two_types(obs_support = 7, total_obs = 10)
  expect_true(all.equal(res, 0.018648018648018651472))
})

test_that("It prints the correct interpretation", {
  res <- find_p_two_types(obs_support = 7, total_obs = 10, interpretation = TRUE)
  expect_true(all.equal(res[["thep"]], 0.018648018648018651472))
  expect_true(all.equal(res[["interp"]], "The maximum probability of drawing 7 observations which support the working theory from an urn model supporting a rival theory, where the odds of observing working theory information is odds=1 and evidentiary weights are (1,1,1,1,1,1,1), is p <=0.0186."))
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

test_that("P-values with a smoking gun observation are smaller than p-values with with equal weights", {
  res_smoking_gun_2 <- find_p_two_types(obs_support = 7, total_obs = 10, weights = rep(c(2, 1), c(1, 7 - 1)), odds = 1, interpretation = FALSE)
  res_smoking_gun_4 <- find_p_two_types(obs_support = 7, total_obs = 10, weights = rep(c(4, 1), c(1, 7 - 1)), odds = 1, interpretation = FALSE)
  res_equal_wt <- find_p_two_types(obs_support = 7, total_obs = 10, odds = 1, interpretation = FALSE)
  expect_lt(res_smoking_gun_2, res_equal_wt)
  expect_lt(res_smoking_gun_4, res_smoking_gun_2)
})

test_that("We can specify numbers of rival_obs greater than working obs + 1", {
  res_collapsed_multi_urn <- find_p_two_types(obs_support = 7, total_obs = 7, rival_obs = 4 * (7 + 1))
  res_one_urn <- find_p_two_types(obs_support = 7, total_obs = 7)
  expect_lt(res_collapsed_multi_urn, res_one_urn)
})

## test_that("Multi p runs without error",{
##
## res <- multi_urn_p(
##   n_working = 6, total_n_drawn, urn_tots = c(), odds_vec = rep(1, 3),
##   list_of_possible_rivals,
##   evidence_wts = NULL
## )
##
## })
