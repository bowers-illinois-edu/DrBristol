#' A function to generate p-values from a multivariate Urn model with optional
#' sensitivity analysis (unequal odds of observation) and weighting (unequal
#' probative weight)
#'
#' If a set of observations provides evidence against more than one rival
#' theory, this function will provide a p-value for the hypothesis the
#' observation made did not come from any of the rivals.

#' @param n_working The number of items supporting the working theory observed

#' @param tot_n_drawn The total number of items drawn from the urn

#' @param urn_tots A vector of the total number of items of each type in the
#' urn. For example, \code{c(6,7,7,7)} would mean that the urn contains 6 items of
#' type 1, and 7 items of each of the other three types.

#' @param odds_vec A vector with the weight assigned to each type of item in
#' the urn. For example, c(4,3,2,1) would mean that items of type 1 are 4 times
#' as likely to be drawn as items of type 4, and 4/2=2 or twice as likely to be
#' drawn as items of type 3.

#' @param list_of_possible_rivals A list of possible numbers of items drawn
#' from each rival for example list(0:2,0:3,0:1) would mean that between 0 and
#' 2 items could be drawn from the first rival, 0 and 3 from the second rival,
#' and 0 or 1 item could be drawn from the third rival

#' @param evidence_wts Not yet implemented.

#' @examples
#' # 9 items observed with 6 of them implausible from perspective of rival 1, 7
#' # of them them implausible from perspective of rival 2, and 8 of them
#' # implausible from perspective of rival 3.
#'
#' @import BiasedUrn
#' @export

multi_urn_p <- function(n_working, tot_n_drawn, urn_tots, odds_vec, list_of_possible_rivals, evidence_wts) {
  ## This next is brute force method and will cause problems if the list of
  ## possible rivals is long or has many elements because of expand.grid() below. We should either warn or
  ## come up with a better approach. This works for now.

  rival_vectors <- expand.grid(list_of_possible_rivals)
  rival_vectors$tot <- rowSums(rival_vectors)
  rival_vectors1 <- rival_vectors[with(rival_vectors, tot == (tot_n_drawn - n_working)), ]
  atomic_ps <- apply(rival_vectors, 1, function(thevec) {
    thexvec <- c(n_working, thevec)
    thep <- dMFNCHypergeo(x = thexvec, m = urn_tots, n = tot_n_drawn, odds = odds_vec)
    return(thep)
  })
  thep <- max(atomic_ps)
  return(thep)
}
