##' Find odds
##'
##'  TODO Description
##'
##'
##' @title Find odds given ideas about unequally easy evidence
##' @param obs_support An integer representing the number of observations in favor of the working hypothesis. Must be less than or equal to the total.
##' @param total_obs An integer representing the total number of observations
##' @param thep The p-value threshold
##' @return A vector representing a big value to state that x>0
##' @importFrom BiasedUrn dFNCHypergeo
##' @importFrom stats uniroot 
##' @export

  obs_oppose <- obs_support+1

sens_analysis <- function(obs_support, obs_oppose, total_obs, thep=.05) {
  find_odds <- function(x, thep = thep) {
    ## thep is the desired pvalue
    ## x is the odds
    if (x < 0) {
      return(999)
    }
    res0 <- dFNCHypergeo(seq(0, obs_support), m1 = obs_support, m1 = obs_oppose, n = total_obs, odds = x)
    return(res0[3] - thep)
  }

  theodds <- uniroot(f = find_odds, interval = c(.0001, n * 10), trace = 2, extendInt = "yes")
  found_odds <- theodds$root
  the_found_dens <- dFNCHypergeo(seq(0, obs_support), m1 = obs_support, m1 = obs_oppose, n = total_obs, odds = found_odds)
  return(the_found_dens)
}
