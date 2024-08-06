# Functions for drawing from urn models


#' Simulate an urn model
#'
#' A function that allows the researcher to simulate draws from urn models that
#' do not have nice analytical expressions. For example, urn models where each
#' ball has a different probability of being drawn. Given a set of draws, we
#' can calculate probabilities.
#'
#'
#' @title Simulate draws from an urn model
#' @param urn_size Total number of items in the urn
#' @param total_obs An integer representing the total number of observations
#' @param thep The p-value threshold
#' @return A vector of length n
#' @importFrom BiasedUrn dFNCHypergeo
#' @importFrom stats uniroot
#' @export

sim_urn <- function(urn_size, total_obs, item_probs, thep) {
  res <- sample(x = distinct_items, prob = item_probs, replace = FALSE)
  return(the_draw)
}
