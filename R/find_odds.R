##' Find odds
##'
##'  TODO Description
##'
##'
##' @title Find odds given ideas about unequally easy evidence
##' @param m1 TODO
##' @param m2 TODO
##' @param n TODO
##' @param thep TODO
##' @return A vector representing TODO
##' @importFrom BiasedUrn dFNCHypergeo
##' @importFrom stats uniroot 
##' @export
sens_analysis <- function(m1, m2, n, thep=.05) {
  find_odds <- function(x, thep = thep) {
    ## thep is the desired pvalue
    ## x is the odds
    if (x < 0) {
      return(999)
    }
    res0 <- dFNCHypergeo(seq(0, m1), m1 = m1, m2 = m2, n = n, odds = x)
    return(res0[3] - thep)
  }

  theodds <- uniroot(f = find_odds, interval = c(.0001, n * 10), trace = 2, extendInt = "yes")
  found_odds <- theodds$root
  the_found_dens <- dFNCHypergeo(seq(0, m1), m1 = m1, m2 = m2, n = n, odds = found_odds)
  return(the_found_dens)
}
