#' Sensitivity analysis for urn models with two types of information
#'
#'
#' A function that allows the researcher to calculate the odds of
#' drawing a working theory supporting observation over a rival theory supporting observation
#' required to increase a p-value to above a certain value.
#'
#' @param obs_support An integer representing the number of observations
#' in favor of the working hypothesis. Must be less than or equal to the total.

#' @param total_obs An integer representing the total number of observations

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
#' find_p_two_types(obs_support = 7, total_obs = 10, odds = 1)
#' sens_obs_two_types(obs_support = 7, total_obs = 10, p_threshold = .05)$w
#' # Notice that this is correct:
#' find_p_two_types(obs_support = 7, total_obs = 10, odds = 1.58928)

#' @importFrom stats uniroot
#' @export
sens_obs_two_types <- function(obs_support, total_obs, rival_obs = NULL, weights = NULL, p_threshold = .05) {
  theodds <- uniroot(
    f = find_odds_two_types,
    obs_support = obs_support,
    total_obs = total_obs,
    rival_obs = rival_obs,
    weights = weights, p_threshold = p_threshold,
    interval = c(.Machine$double.eps, total_obs * 10), trace = 2, extendInt = "upX"
  )$root

  thep_at_theodds <- find_p_two_types(
    obs_support = obs_support, total_obs = total_obs, rival_obs = rival_obs,
    weights = weights, odds = theodds, interpretation = FALSE
  )
  return(list(w = theodds, p = thep_at_theodds))
}

#' Calculate the difference between the p-value and a p-value threshold for the two types of observation model
#'
#' A helper function for `sens_obs_two_types()` see `find_p_two_types()` for details.
#'
#' @inheritParams find_p_two_types
#' @param p_threshold A number between 0 and 1 indicating the critical value of the test.
#'
#' @export
find_odds_two_types <- function(omega, obs_support,
                                total_obs, rival_obs, weights, p_threshold) {
  p_found <- find_p_two_types(
    obs_support = obs_support, total_obs = total_obs, rival_obs = rival_obs,
    weights = weights, odds = omega, interpretation = FALSE
  )
  ## For multiple testing later
  ## critical_value <- alpha_thresh / alpha_adjust
  return(p_found - p_threshold)
}
