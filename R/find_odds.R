##' Find odds
##'
##'  A function that allows the researcher to input the likelihood of data or to find how mush bias in datacollection would be nescessary to obtain a p>0.05 or p>0.10 result using the same data.
##'
##'
##' @title Find odds given ideas about unequally easy evidence
##' @param obs_support An integer representing the number of observations in favor of the working hypothesis. Must be less than or equal to the total.
##' @param total_obs An integer representing the total number of observations
##' @param thep The p-value threshold
##' @return
##' @importFrom BiasedUrn dFNCHypergeo
##' @importFrom stats uniroot
##' @export

sens_analysis <- function(obs_support, obs_oppose, total_obs, thep = .05) {
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
