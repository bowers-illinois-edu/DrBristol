
#' A function to generate p-values from a multivariate Urn model with optional
#' sensitivity analysis (unequal odds of observation) and weighting (unequal
#' probative weight)
#'
#' If a set of observations provides evidence against more than one rival
#' theory, this function will provide a p-value for the hypothesis the
#' observation made did not come from any of the rivals.

#' @param n_working The number of items supporting the working theory against
#' each of 2 or more rivals.

#' @param tot_n_drawn The total number of items drawn from the urn (should be no less than \code{sum(n_working)} but can be larger if some of the items observed support one or more rivals.)

#' @param urn_tots A vector of the total number of items of each type in the
#' urn. For example, \code{c(6,4,3)} would mean that the urn contains 6 items of
#' type 1, and 4 items of type 2 and 3 of type 3.

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
#' # 10 items observed with 5 of them implausible from perspective of rival 1, 3
#' # of them them implausible from perspective of rival 2, and 2 of them
#' # implausible from perspective of rival 3.
#'
#' @import BiasedUrn
#' @export

## Ex 1: We only observe obs that support the working theory against rivals
obs_working <- c(5, 3, 2)
rival_obs <- obs_working + 1
tot_n_drawn <- sum(obs_working)
odds_vec <- rep(1, 6)
## The urn contains both the working theory supporting observations and the rival theory supporting obs
urn_tots <- c(obs_working, rival_obs)
xvec <- c(obs_working, c(0, 0, 0))
thep <- dMFNCHypergeo(x = xvec, m = urn_tots, n = tot_n_drawn, odds = odds_vec)

dMFNCHypergeo(x = c(5, 0, 0), m = c(8, 4, 2), n = 5, odds = 1)
dMFNCHypergeo(x = c(4, 0, 0), m = c(8, 4, 2), n = 4, odds = 1)


mat <- matrix(c(5, 0, 0, 5, 0, 0), nrow = 3, ncol = 2)
dMFNCHypergeo(x = mat, m = c(8, 4, 2), n = 5, odds = 1)


# Parameters
m <- c(30, 25, 20) # urn composition: 3 colors
odds <- c(1, 1, 1) # Fisher odds for the 3 colors
n <- 15 # total draws

# Build x as a c x K matrix: one COLUMN per observation, columns sum to n
x <- cbind(
  c(5, 6, 4), # observation 1
  c(10, 3, 2), # observation 2
  c(0, 7, 8) # observation 3
)
# sanity checks (optional but helpful):
stopifnot(nrow(x) == length(m), all(colSums(x) == n), all(x >= 0), all(x <= m))

# Vectorized call: returns a length-K vector of probabilities
pmf <- dMFNCHypergeo(x, m = m, n = n, odds = odds) # (columns are observations)
pmf



## Ex 2: We observe some observations that support one or more rivals.
## We collapse over or marginalize over the numbers of rivals.
tot_n_drawn <- sum(obs_working) + 5
## Now xvec

## Ex 3: The 5 working obs count against rival 1; 4 of them also against rival
## 2; 3 obs is against rival 3; and 2 obs is against rival 4

## Approach 1: Union-null max-p
### The Union-null is (either model A or model B)
### 	•	We are comparing two mutually exclusive simple urns:
### 	•	H_{0A}: 5 Blue, 6 Green
### 	•	H_{0B}: 4 Blue-Plus (which are Blue), 5 Orange
### 	•	For the union $H_0 = H_{0A}\cup H_{0B}$, the valid single p-value is
### $p_{\text{union}}=\max\{p_A,\,p_B\}$.
### 	•	This remains valid regardless of “Plus ⊂ Blue,” because each $p_i$ is computed under its own simple model.


#' @export
find_p_multi_mv <- function(n_working, tot_n_drawn, urn_tots, odds_vec, list_of_possible_rivals, evidence_wts) {
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

## The Cauchy combination method (Liu and Xie 2019)
## Cauchy Combination Test: A Powerful Test With Analytic p-Value Calculation Under Arbitrary Dependency Structures
## I think this is actually best used for the global null or intersection null: all of the rivals are true.
## This function goes kind of crazy at 0 and 1 so add or substract a tiny amount from either side if needed
acat_pvalue <- function(p_values) {
  if (any(p_values == 0 | p_values == 1, na.rm = TRUE)) {
    #      stop("Input p-values must lie strictly inside (0,1).")
    p_values[p_values == 0] <- p_values[p_values == 0] + .Machine$double.eps
    p_values[p_values == 1] <- p_values[p_values == 1] - .Machine$double.eps
  }
  ## pi is pi FYI
  ## Since this procedure creates weird results when p is very near 1 or p very near 0, we create
  ## the two sided p-values using one-sided tests and then double the minimum.
  p_1 <- p_values / 2
  t_stat <- mean(tan((0.5 - p_1) * pi)) # equal weights
  upper_p <- 0.5 - atan(t_stat) / pi # final ACAT p-value
  ## We want a 2 sided test
  return(2 * min(c(upper_p, 1 - upper_p)))
}
