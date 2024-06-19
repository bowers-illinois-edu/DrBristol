##' Find a p-value given opposing and supporting information
##' 
##' @param num_support An integer representing the number of pieces of information in favor of the hypothesis. Must be less than or equal to the total.
##' @param total_info An integer representing the total number of pieces of information observed
##' @export
find_p <- function(num_support,total_info){
  ## Test to make sure that num_support is less than or equal to total_info
  stopifnot("Number of pieces of supporting info must be less than or equal to the total number of observations"=num_support<=total_info)
  num_oppose <- num_support+1
  stopifnot("Number of pieces of supporting info must be greater than or equal to half of the total number of observations"=num_support >= (total_info/2))
  ## We assume odds=1 here
 thep <- dFNCHypergeo(x=num_support, m1 = num_support, m2 = num_oppose,
                      n = total_info, odds = 1)
 return(thep)
}