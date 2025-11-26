#' Multi-rival testing: Multivariate Approach
#'
#' @description
#' This is a test of the composite or null hypothesis that at
#' least one rival theory is more consistent with the data than the working
#' theory: or Rival Theory 1 is true OR Rival Theory 2 is true OR ... Rival k
#' is true.

#' @details We reject this composite hypothesis if we can reject all of the
#' individual hypotheses. There are two main approaches to this problem: (1) Do
#' the individual tests and use the maximum p-value (TODO cite Berger on why
#' the intersection-union test has controlled FWER) or (2) represent this test
#' as a single test using a multivariate distribution. This function implements
#' the second option.
#'
#' On interpretation: If we see a p>\alpha, then we would say, "We cannot
#' reject the union null hypothesis. The observed evidence pattern is not
#' sufficiently unlikely under the hypothesis that at least one rival theory is
#' correct. Therefore, we do not have strong enough evidence to rule out all
#' rival theories. The data are consistent with at least one rival being a
#' valid explanation."

#' If we see a p <= \alpha, then we would say, 'We reject the union null
#' hypothesis. The observed evidence pattern would occur by chance only 100*p%
#' of the time if at least one rival theory were actually correct. This
#' provides strong statistical evidence that all rival theories are wrong and
#' the working theory is the correct explanation. We have simultaneously ruled
#' out all rivals as viable alternatives."

#' We assume more anti-rival observations are made than pro-rival observations.
#' And, following Lopez and Bowers (2026), we specify conservative urn model(s)
#' in which the number of pro-rival observations is just one more than the
#' observed anti-rival observations.

#' @param obs_support A vector of integers representing the number of
#' observations made in favor of the working hypothesis. Each element must be less
#' than or equal to the corresponding element in `total_obs`.

#' @param neutral_obs Optional. An integer representing the number of observations
#' made that are neither pro-working theory nor anti-rival.

#' @param rival_obs Optional. An integer representing the number of
#' observations actually made that support one of the rivals. Should be the
#' same length as obs_support since each observation supporting the working
#' theory should oppose or be inconsistent with one rival.

#' @param odds The odds of observing a rival versus working-theory observation.
#' This can be interpreted as "bias" in observation. Or "relative ease" of observation.

#' @param interpretation Logical. TRUE if the function returns text helping
#' to interpret the result, FALSE (default option) to returns only the p-value (TODO implement)

#' @param check_evidence Logical. TRUE if the function checks to see if it
#' looks like each element in the `obs_support` are actually copies of the same
#' set of observations. For example, if obs_support=c(4,4,4,4) then we wonder
#' whether we actually have 4 different kinds of observations, each of which
#' are inconsistent with a different rival or whether we have 4 observations in
#' total and they all are inconsistent with each of 4 rivals. In the later
#' case, we should use the `find_p_multi_max_p()` which, in turn, will just use
#' `find_p_two_types()` since we have just two kinds of information in the urn,
#' anti-Rival (here 4 items) and pro-Rival (here 4+1 items).

#' @return Either a p-value (numeric, scalar) or a list containing the p-value
#' and text containing an interpretation

#' @examples
#' # Example 1:
#' # One kind of working theory supporting information that argues against multiple rivals where
#' # each rival has the same amount of information.
#' # Notice that we will get the same answer as if we used `find_p_two_types()` directly.
#' # But we present this here to illustrate.
#' # 4 rivals, 10 observations of one kind of working theory supporting observation, 10 total observations made
#' find_p_multi_max_p(obs_support = rep(10, 4), total_obs = rep(10, 4))
#' find_p_multi_mv(obs_support = c(10, 10, 10, 10))
#' find_p_two_types(obs_support = 10, total_obs = 10)

#' # Example 2:
#' # 4 kinds of working theory supporting observations, each of which is
#' # inconsistent with a single rival, only working theory supporting observations
#' # made.
#' find_p_multi_mv(obs_support = c(4, 3, 2, 1))

#' # Example 3:
#' # 4 kinds of working theory supporting observations, each of which is
#' # inconsistent with a single rival. But some pro-rival observations made.
#' # Here we have to provide a vector the same length as the obs_support for rival_obs
#' # since each entry in the obs_support is anti-one particular rival
#' find_p_multi_mv(obs_support = c(4, 3, 2, 1), rival_obs=c(1,1,0,0))

#' @export

find_p_multi_mv_old <- function(obs_support, neutral_obs = 0, rival_obs = NULL, odds = 1,
                                interpretation = FALSE, check_evidence = TRUE) {
  ## TODO allow for some proRival observations
  ## TODO, allow irrelevant or neutral observations
  ## TODO allow evidence weight

  k <- length(obs_support)
  stopifnot(neutral_obs >= 0)

  ## If it looks like we have exactly the same amount of information against
  ## all rivals, then we should just return the max_p version. Including a
  ## message to make sure that we are doing the right thing.

  if (check_evidence) {
    ## If all the obs_support is the same

    unique_obs_support <- unique(obs_support)

    ## And no rivals are provided (such that the user wants to use
    ## obs_support+1 as the rival_obs, which means identical rival_obs

    if (length(unique_obs_support) == 1 && is.null(rival_obs)) {
      message(strwrap("It looks like you have only one kind of evidence that is
        inconsistent with multiple rivals. You would over-state your evidence
        against the rivals we used a multivariate null-model by repeating the
        same number of anti-Rival observations. We are reporting here the
        p-value from the find_p_multi_max_p command. If you actually do have
        multiple types of observations but happen to have the same numbers of
        them, then you should try this command again but set
        check_evidence=FALSE"))

      res_p <- find_p_multi_max_p(
        obs_support = obs_support,
        total_obs = rep(unique_obs_support, length(obs_support)), rival_obs = NULL, odds = odds
      )
      return(res_p)
    }
  }

  ## If we actually only observe working theory supporting items, then the
  ## total size of the draw from the urn is just the sum of the obs_support.
  ## Otherwise it is the sum of the other types of observations.
  if (is.null(rival_obs)) {
    total_obs <- sum(c(obs_support, neutral_obs))
    urn_obs_oppose <- obs_support + 1
    ## No pro rival obs actually observed
    obs_oppose <- rep(0, length(obs_support))
  } else {
    stopifnot(length(rival_obs) == k)
    all_obs <- c(obs_support, rival_obs, neutral_obs)
    total_obs <- sum(all_obs)
    urn_obs_oppose <- rival_obs
  }

  x_vec <- c(obs_support, obs_oppose, rep(1, neutral_obs))

  if (odds == 1) {
    odds <- rep(1, length(x_vec))
  }

  if (neutral_obs == 0) {
    num_neutral_obs <- NULL
  } else {
    num_neutral_obs <- neutral_obs
  }

  if (is.null(rival_obs) && neutral_obs == 0) {
    urn_obs <- c(obs_support, urn_obs_oppose, num_neutral_obs)
    res_p <- dMFNCHypergeo(x = x_vec, m = urn_obs, n = total_obs, odds = odds)
  }
  ## Now generate the ways that we can have anti-Rival evidence


  ## And sum the probabilities of seeing this evidence

  return(res_p)
}
