#' Sensitivity analysis for simple urn models
#'
#'
#' A function that allows the researcher to calculate the p-value arising from an urn
#' where the probability of drawing a rival supporting observation is not equal to the
#' probability of drawing a working-theory supporting observation.
#'
#'
#'
#' @param obs_support An integer representing the number of observations
#' in favor of the working hypothesis. Must be less than or equal to the total.
#' @param obs_oppose An integer representing the number of observations in against
#' the working hypothesis. Must be less than or equal to the total.
#' @param total_obs An integer representing the total number of observations
#' @param thep A decimal (Double). The p-value threshold. Default is p=.05.
#' @return The p-value for a given amount of bias in drawing observations from the urn.
#' @importFrom BiasedUrn dFNCHypergeo
#' @importFrom stats uniroot
#' @export
sens_urn <- function(obs_support, obs_oppose, p_threshold = .05) {
  find_odds <- function(omega, m1, m2, n, x, alpha_thresh, alpha_adjust) {
    p_found <- dFNCHypergeo(x = x, m1 = m1, m2 = m2, n = n, odds = omega)
    critical_value <- alpha_thresh / alpha_adjust
    return(p_found - critical_value)
  }

  urn_total <- max(obs_support + obs_oppose + 1, obs_oppose)
  theodds <- uniroot(
    f = find_odds,
    m1 = obs_support, m2 = obs_oppose, n = urn_total, x = obs_support,
    alpha_thresh = alpha_thresh, alpha_adjust = alpha_adjust,
    interval = c(.0001, n * 10), trace = 2, extendInt = "upX"
  )$root
  the_found_dens <- dFNCHypergeo(seq(0, obs_support),
    m1 = obs_support, m2 = obs_oppose, n = total_obs, odds = found_odds
  )
  return(c(theodds, the_found_dens))
}

#' @param m1 Integer. number of pieces of evidence supporting the working theory
#' @param m2 Integer. number of pieces of evidence supporting the rival theory
#' @param n Integer. number of pieces drawn from the urn
#' @param odds Double. odds of drawing evidence supporting the working theory versus rival theory
#' @param x Integer. number of pieces of evidence supporting the working theory in the sample of size n
#' @importFrom BiasedUrn dFNCHypergeo
#' @export
find_odds <- function(omega, m1, m2, n, x, alpha_thresh = .05, alpha_adjust = 1) {
  p_found <- dFNCHypergeo(x = x, m1 = m1, m2 = m2, n = n, odds = omega)
  critical_value <- alpha_thresh / alpha_adjust
  return(p_found - critical_value)
}
