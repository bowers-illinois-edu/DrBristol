#' Multi-rival testing: Max-P approach
#'
#' @description
#' This is a test of the composite or null hypothesis that at
#' least one rival theory is more consistent with the data than the working
#' theory: or Rival Theory 1 is true OR Rival Theory 2 is true OR ... Rival k
#' is true.
#'

#' @details We reject this composite hypothesis if we can reject all of the
#' individual hypotheses. There are two main approaches to this problem: (1) Do
#' the individual tests and use the maximum p-value (TODO cite Berger on why
#' the intersection-union test has controlled FWER) or (2) represent this test
#' as a single test using a multivariate distribution. This function implements
#' the first option by calling `find_p_two_types()` for each element of the
#' vectors provided below. This approach tends to be very conservative.
#'

#' @param obs_support A vector of integers representing the number of
#' observations in favor of the working hypothesis. Each element must be less
#' than or equal to the corresponding element in `total_obs`.

#' @param total_obs An vector of integers representing the total number of observations
#' made. Its elements can be can be greater than or equal to the `obs_support`.

#' @param rival_obs Optional. An integer representing the number of
#' observations in the urn that do not support the working theory.

#' @param odds The odds of observing a rival versus working-theory observation.
#' This can be interpreted as "bias" in observation. Or "relative ease" of observation.

#' @param weights Double vector. Default is equal weight for each observation when
#' weights=NULL is `rep(1,obs_support)`. To indicate that one observation
#' should have twice the weight of any other one might use
#' `rep(c(2,1),c(1,obs_support-1))`

#' @param interpretation Logical. TRUE if the function returns text helping
#' to interpret the result, FALSE (default option) to returns only the p-value

#' @return Either a p-value (numeric, scalar) or a list containing the p-value
#' and text containing an interpretation

#' @examples
#' # Example 1:
#' # One kind of working theory supporting information that argues against multiple rivals where
#' # each rival has the same amount of information.
#' # Notice that we will get the same answer as if we used `find_p_two_types()` directly.
#' # But we present this here to illustrate.
#' # 4 rivals, 10 observations of one kind of working theory supporting observation, 10 total observations made
#' find_p_multi_max_p(obs_support = rep(10, 4), total_obs = rep(10, 4))
#' find_p_two_types(obs_support = 10, total_obs = 10)

#' # Example 2:
#' # 4 kinds of working theory supporting observations, each of which is
#' # inconsistent with a single rival, only working theory supporting observations
#' # made. Notice the max p:
#' find_p_multi_max_p(obs_support = c(4, 3, 2, 1), total_obs = c(4, 3, 2, 1))
#' find_p_two_types(obs_support = 4, total_obs = 4)
#' find_p_two_types(obs_support = 3, total_obs = 3)
#' find_p_two_types(obs_support = 2, total_obs = 2)
#' find_p_two_types(obs_support = 1, total_obs = 1)

#' @export
find_p_multi_max_p <- function(obs_support, total_obs, rival_obs = NULL, odds = 1, weights = NULL) {
  k <- length(obs_support)
  stopifnot(k == length(total_obs))

  if (odds == 1) {
    odds <- rep(1, k)
  }
  if (is.null(weights)) {
    weights <- rep(NULL, k)
  }
  if (is.null(rival_obs)) {
    rival_obs <- rep(NULL, k)
  }

  atom_ps <- sapply(seq_len(k), function(i) {
    atom_p <- find_p_two_types(
      obs_support = obs_support[i],
      total_obs = total_obs[i],
      rival_obs = rival_obs[i],
      odds = odds[i],
      weights = weights[i]
    )
    return(atom_p)
  })

  res_p <- max(atom_ps)
  return(res_p)
}


#' Multi-rival testing: Multivariate Approach
#'
#' @description
#' This is a test of the composite or null hypothesis that at
#' least one rival theory is more consistent with the data than the working
#' theory: or Rival Theory 1 is true OR Rival Theory 2 is true OR ... Rival k
#' is true.
#'

#' @details We reject this composite hypothesis if we can reject all of the
#' individual hypotheses. There are two main approaches to this problem: (1) Do
#' the individual tests and use the maximum p-value (TODO cite Berger on why
#' the intersection-union test has controlled FWER) or (2) represent this test
#' as a single test using a multivariate distribution. This function implements
#' the second option.
#'

#' @param obs_support A vector of integers representing the number of
#' observations in favor of the working hypothesis. Each element must be less
#' than or equal to the corresponding element in `total_obs`.

#' @param total_obs An integer representing the total number of observations
#' made. It can be greater than or equal to the sum of `obs_support`.

#' @param rival_obs Optional. An integer representing the number of
#' observations in the urn that do not support the working theory.

#' @param odds The odds of observing a rival versus working-theory observation.
#' This can be interpreted as "bias" in observation. Or "relative ease" of observation.

#' @param interpretation Logical. TRUE if the function returns text helping
#' to interpret the result, FALSE (default option) to returns only the p-value

#' @return Either a p-value (numeric, scalar) or a list containing the p-value
#' and text containing an interpretation

#' @examples
#' # Example 1:
#' # One kind of working theory supporting information that argues against multiple rivals where
#' # each rival has the same amount of information.
#' # Notice that we will get the same answer as if we used `find_p_two_types()` directly.
#' # But we present this here to illustrate.
#' # 4 rivals, 10 observations of one kind of working theory supporting observation, 10 total observations made
#' find_p_multi_max_p(obs_support = rep(10, 4), total_obs = rep(10, 4))
#' find_p_multi_mv(obs_support = rep(10, 4), total_obs = 40)
#' find_p_two_types(obs_support = 10, total_obs = 10)

#' # Example 2:
#' # 4 kinds of working theory supporting observations, each of which is
#' # inconsistent with a single rival, only working theory supporting observations
#' # made. Notice the max p:
#' find_p_multi_mv(obs_support = c(4, 3, 2, 1), total_obs = NULL)

#' @export

find_p_multi_mv <- function(obs_support, total_obs = NULL, rival_obs = NULL, odds = 1) {
  ## TODO, allow irrelevant or neutral observations
  ## TODO allow evidence weight

  k <- length(obs_support)

  if (is.null(total_obs)) {
    total_obs <- sum(obs_support)
  }

  if (is.null(rival_obs)) {
    obs_oppose <- pmax(total_obs - obs_support, obs_support + 1)
  } else {
    stopifnot(length(rival_obs) == k)
    obs_oppose <- rival_obs
  }

  x_vec <- c(obs_support, rep(0, length(obs_oppose)))

  if (odds == 1) {
    odds <- rep(1, length(x_vec))
  }

  res_p <- dMFNCHypergeo(x = x_vec, m = c(obs_support, obs_oppose), n = total_obs, odds = odds)



  return(res_p)
}
