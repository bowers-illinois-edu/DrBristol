## Tests of the sensitivity analysis code

## The next lines are for use when creating the tests. Change interactive<-FALSE for production
interactive <- FALSE
if (interactive) {
  library(devtools)
  load_all() ## use  this during debugging
}

test_that("Sensitivity analysis function gives same answer as the by hand version", {
  ## Define the by hand functions
  find_odds_test <- function(omega, m1, m2, n, x, alpha_thresh = .05, alpha_adjust = 1) {
    # m1= number of pieces of evidence supporting the working theory
    # m2 = number of pieces of evidence supporting the rival theory
    # n = number of pieces drawn from the urn
    # odds = odds of drawing evidence supporting the working theory versus rival theory
    # x = number of pieces of evidence supporting the working theory in the sample of size n
    p_found <- dFNCHypergeo(x = x, m1 = m1, m2 = m2, n = n, odds = omega)
    critical_value <- alpha_thresh / alpha_adjust
    return(p_found - critical_value)
  }

  set.seed(12345)
  found_odds_ex1 <- uniroot(
    f = find_odds_test, m1 = 2, m2 = 3, n = 3, x = 2,
    interval = c(.Machine$double.eps, 10 * 3), trace = 2, extendInt = "upX"
  )$root

  found_p_ex1 <- dFNCHypergeo(2, m1 = 2, m2 = 3, n = 3, odds = found_odds_ex1)
  found_p_ex2 <- find_p_two_types(obs_support = 2, total_obs = 3, odds = found_odds_ex1)

  set.seed(12345)
  sens_ex1 <- sens_urn(obs_support = 2, total_obs = 3)
  expect_equal(found_odds_ex1, sens_ex1$w)
  expect_equal(found_p_ex1, sens_ex1$p)

  set.seed(12345)
  sens_ex2 <- sens_urn(obs_support = 7, total_obs = 10)
  ## The solution when we solve for w from the non-central/biased hypergeometric with 7 obs
  p_fun_x7 <- function(w) {
    (8 * w^5) / (3 + 40 * w + 140 * w^2 + 168 * w^3 + 70 * w^4 + 8 * w^5)
  }

  expect_equal(p_fun_x7(sens_ex2$w), sens_ex2$p)
})
