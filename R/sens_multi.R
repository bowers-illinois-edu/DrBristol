#' Sensitivity analysis for urn models with two types of information
#'
#'
#' A function that allows the researcher to calculate the odds of
#' drawing a working theory supporting observation over a rival theory supporting observation
#' required to increase a p-value to above a certain value.
#'
#' @param obs_support An integer representing the number of observations
#' in favor of the working hypothesis. Must be less than or equal to the total.

#' @param rival_obs Optional. The number of observations in the urn that do not support the working theory.

#' @param weights A vector of numeric weights representing the differential
#' evidentiary weight of the working theory supporting observations.

#' @param p_threshold A decimal (Double). The p-value threshold. Default is p=.05.

#' @return A list with two elements:
#'
#' 1. in `w` the differential odds of observing a working theory
#'  supporting observation over a rival theory supporting observation.
#' 2. in `p` The p-value given the found \code{w}.
#'

#' @examples
#' # What is the odds that would bring our p=.02 to p \approx .05
#' find_p_multi_mv(obs_support = c(4, 3, 2, 1), odds = 1)
#' find_p_multi_mv(obs_support = c(4, 3, 2, 1), odds = 2)
#' sens_obs_multi(obs_support = c(4, 3, 2, 1), p_threshold = .05)$w
#' # Notice that this is correct:
#' find_p_multi_mv(obs_support = c(4, 3, 2, 1), odds = 22.535)
#' sens_obs_multi(obs_support = c(4, 3, 2, 1), rival_obs = c(1, 1, 0, 0), p_threshold = .05)$w
#' sens_obs_multi(obs_support = c(4, 4, 4, 4), p_threshold = .05)
#' find_p_multi_mv(obs_support = c(4, 4, 4, 4), odds = 2.6)

#' @importFrom stats uniroot
#' @export
sens_obs_multi <- function(obs_support, rival_obs = NULL, weights = NULL, p_threshold = .05) {
  k <- length(obs_support)

  ## Set up urn composition and observations
  if (is.null(rival_obs)) {
    rival_obs <- rep(0, k)
    urn_rival <- obs_support + 1 # Conservative: pop has obs+1 for each rival
  } else {
    stopifnot(length(rival_obs) == k)
    stopifnot(all(rival_obs >= 0))
    urn_rival <- pmax(obs_support + 1, rival_obs) # At least obs+1 or observed
  }

  total_obs <- sum(obs_support) + sum(rival_obs)

  theodds <- uniroot(
    f = find_odds_multi,
    obs_support = obs_support,
    rival_obs = rival_obs,
    weights = weights,
    p_threshold = p_threshold,
    interval = c(.Machine$double.eps, total_obs * 10), trace = 2, extendInt = "upX"
  )$root

  thep_at_theodds <- find_p_multi_mv(
    obs_support = obs_support, rival_obs = rival_obs,
    weights = weights, odds = theodds, interpretation = FALSE, messages = FALSE
  )
  return(list(w = theodds, p = thep_at_theodds))
}



#' Calculate the difference between the p-value and a p-value threshold for the multiple types of information model
#'
#' A helper function for `sens_obs_two_types()` see `find_p_multi_mv()` for details.
#'
#' @inheritParams find_p_multi_mv
#' @param omega Candidate odds ratio being evaluated by the root finder.
#' @param p_threshold A number between 0 and 1 indicating the critical value of the test.
#'
#' @export
find_odds_multi <- function(omega, obs_support,
                            rival_obs, weights, p_threshold) {
  p_found <- find_p_multi_mv(
    obs_support = obs_support, rival_obs = rival_obs,
    weights = weights, odds = omega, interpretation = FALSE, messages = FALSE
  )
  ## For multiple testing later
  ## critical_value <- alpha_thresh / alpha_adjust
  return(p_found - p_threshold)
}
