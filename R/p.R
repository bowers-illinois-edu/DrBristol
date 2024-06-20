##' Find a p-value given a certain number of observations in favor of the working hypothesis among a total number of observations
##' 
##' @param obs_support An integer representing the number of observations in favor of the working hypothesis. Must be less than or equal to the total.
##' @param total_obs An integer representing the total number of observations
##' @export
find_p <- function(obs_support,total_obs){
  ## Test to make sure that obs_support is less than or equal to total_obs
  stopifnot("The number of observations in favor of the working hypothesis must be less than or equal to the total number of observations"=obs_support<=total_obs)
  obs_oppose <- obs_support+1
  stopifnot("Observations are already compatible with the null. The number of observations in favor of the working hypothesis must be greater than or equal to half of the total number of observations"=obs_support >= (total_obs/2))
  ## We assume odds=1 here
 thep <- dFNCHypergeo(x=obs_support, m1 = obs_support, m2 = obs_oppose,
                      n = total_obs, odds = 1)
 return(thep)
}
