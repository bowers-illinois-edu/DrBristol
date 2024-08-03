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

