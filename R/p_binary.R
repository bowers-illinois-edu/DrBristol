#' Find a p-value given a certain number of observations in favor of the
#' working hypothesis among a total number of observations when there are two types of observations
#'
#' When an observation can either support a working theory or a rival theory, this function returns the p-value
#' summarizing the evidence against the rival theory provided by the working theory.
#'
#' This function accomodates urns where working theory supporting observations are systematically easier or more difficulty to observe that rival supporting observations (\code{odds}). And it currently also supports designs where one observation out of the total supporting the working theory is  particularly compelling (a "smoking gun"). For example, if observation 1 is worth 2 other observations, the urn can reflect this by increasing the number of observations supporting the rival theory in the urn, accounting for the weight of that smoking gun observation.
#'
#' @param obs_support An integer representing the number of
#' observations in favor of the working hypothesis. Must be less than or equal to the total.
#' @param total_obs An integer representing the total number of observations made
#' @param odds The odds of observing a rival versus working-theory observation.
#' This can be interpreted as "bias" in observation. Or "relative ease" of observation.
#' @param weights Double. Default is equal weight for each observation when weights=NULL is \code{rep(1,obs_support)}. To indicate that one observation should have twice the weight of any other one might use \code{rep(c(2,1),c(1,obs_support-1))}
#' @param interpretation Logical. TRUE if the function returns text helping
#' to interpret the result, FALSE (default option) to returns only the p-value
#' @return Either a p-value (numeric, scalar) or a list containing the p-value and text containing an interpretation
#' @examples
#' # Equal probability, 2 kinds of evidence
#' find_p_two_types(obs_support = 7, total_obs = 10)
#' # Equal probability, 2 kinds of evidence with interpretation printed
#' find_p_two_types(obs_support = 7, total_obs = 10, interpretation = TRUE)
#' # Unequal probability, 2 kinds of evidence with interpretation printed
#' find_p_two_types(obs_support = 7, total_obs = 10, interpretation = TRUE, odds = .5)
#' find_p_two_types(obs_support = 7, total_obs = 10, interpretation = TRUE, odds = 2)
#' # Equal probability, Unequal evidentiary weight, 2 kinds of evidence
#' find_p_two_types(
#'   obs_support = 7, total_obs = 10, weights = rep(1, 7),
#'   interpretation = TRUE, odds = 1
#' )
#' find_p_two_types(
#'   obs_support = 7, total_obs = 10,
#'   weights = rep(c(2, 1), c(1, 7 - 1)), interpretation = TRUE, odds = 1
#' )
#' @export
find_p_two_types <- function(obs_support, total_obs, odds = 1, weights = NULL, interpretation = FALSE) {
  if (is.null(weights)) {
    weights <- rep(1, obs_support)
  }
  ## The weights vector has to be as long as the total number of observations supporting the working theory
  stopifnot(length(weights) == obs_support)
  ## For now should we require that the total weights of the working theory obs add up to at least as much as the total working theory observations observed? TODO
  stopifnot(sum(weights) >= obs_support)
  obs_oppose <- max(c(total_obs - obs_support, sum(weights) + 1, obs_support + 1))
  ## Test to make sure that obs_support is less than or equal to total_obs
  stopifnot("The number of observations in favor of the working hypothesis must be less than or equal to the total number of observations" = obs_support <= total_obs)
  stopifnot("Observations are already compatible with the null. The number of observations in favor of the working hypothesis must be greater than or equal to half of the total number of observations" = obs_support >= (total_obs / 2))

  thep <- dFNCHypergeo(
    x = obs_support, m1 = obs_support, m2 = obs_oppose,
    n = total_obs, odds = odds
  )
  if (!interpretation) {
    return(thep)
  } else {
    interp <- paste0(
      "The maximum probability of drawing ", obs_support,
      " observations which support the working theory from an urn model supporting a rival theory, where the odds of observing working theory information is odds=", odds,
      " and evidentiary weights are (", paste0(weights, collapse = ","), "), is p <=", round(thep, 4), "."
    )
    message(interp)
    return(list(thep = thep, interp = interp))
  }
}
