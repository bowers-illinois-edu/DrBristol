#' Find a p-value given a certain number of observations in favor of the
#' working hypothesis among a total number of observations
#'
#'
#'
#' @param obs_support An integer representing the number of observations in favor of the working hypothesis. Must be less than or equal to the total.
#' @param total_obs An integer representing the total number of observations
#' @param odds The odds of seeing
#' @param interpretation TRUE if the function returns text helping to interpret the result, FALSE (default option) to return just the p-value
#' @return Either a p-value (numeric, scalar) or a list containing the p-value and text containing an interpretation
#' @export
find_p_two_types <- function(obs_support, total_obs, odds = 1, interpretation = FALSE) {
  ## Test to make sure that obs_support is less than or equal to total_obs
  stopifnot("The number of observations in favor of the working hypothesis must be less than or equal to the total number of observations" = obs_support <= total_obs)
  obs_oppose <- obs_support + 1
  stopifnot("Observations are already compatible with the null. The number of observations in favor of the working hypothesis must be greater than or equal to half of the total number of observations" = obs_support >= (total_obs / 2))
  ## We assume odds=1 here
  thep <- dFNCHypergeo(
    x = obs_support, m1 = obs_support, m2 = obs_oppose,
    n = total_obs, odds = odds
  )
  if (!interpretation) {
    return(thep)
  } else {
    interp <- paste0("The probability of drawing ", obs_support, " observations which support the working theory from an urn model supporting a rival theory, where the odds of observing working theory information is odds=", odds, ", is p=", round(thep, 4))
    message(interp)
    return(list(thep = thep, interp = interp))
  }
}
#' @examples
#' ...
#' # Equal probability, 2 kinds of evidence
#' find_p_two_types(obs_support = 7, total_obs = 10)
#' # Equal probability, 2 kinds of evidence with interpretation printed
#' find_p_two_types(obs_support = 7, total_obs = 10, interpretation = TRUE)
#' # Unequal probability, 2 kinds of evidence with interpretation printed
#' find_p_two_types(obs_support = 7, total_obs = 10, interpretation = TRUE, odds = .5)
#' find_p_two_types(obs_support = 7, total_obs = 10, interpretation = TRUE, odds = 2)
