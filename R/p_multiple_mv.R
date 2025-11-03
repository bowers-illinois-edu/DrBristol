#' Multi-rival testing: Multivariate Approach
#'
#' @description
#' This is a test of the composite null hypothesis that at least one rival
#' theory is more consistent with the data than the working theory:
#' Rival Theory 1 is true OR Rival Theory 2 is true OR ... Rival k is true.
#' This function tests only INFORMATIVE observations --- those that distinguish
#' between working theory and rival theories. If you have neutral/uninformative
#' observations, exclude them before calling this function. The test is:
#' "Given n informative observations, how likely is this observed pattern if at least
#' one rival theory is correct?"
#'
#' @details
#' We reject this composite hypothesis if we can reject all of the
#' individual hypotheses. There are two main approaches to this problem:
#' (1) Do the individual tests and use the maximum p-value (see Berger on
#' intersection-union tests and FWER control) or (2) represent this test
#' as a single test using a multivariate distribution. This function implements
#' the second option using the multivariate hypergeometric distribution.
#'
#' The rejection region includes all outcomes with:
#' - Anti-rival evidence >= observed levels for ALL rivals (componentwise)
#' - Total pro-rival observations = observed total (or zero if none observed)
#' - All possible allocations of pro-rival observations across rival types
#'
#' On interpretation:
#' - If p > alpha: "We cannot reject the union null hypothesis. The observed
#'   evidence pattern is not sufficiently unlikely under the hypothesis that at
#'   least one rival theory is correct. Therefore, we do not have strong enough
#'   evidence to rule out all rival theories."
#'
#' - If p <= alpha: "We reject the union null hypothesis. The observed evidence
#'   pattern would occur by chance only 100*p% of the time if at least one rival
#'   theory were actually correct. This provides strong statistical evidence that
#'   all rival theories are wrong and the working theory is the correct explanation."
#'
#' We assume a conservative urn model where the number of pro-rival observations
#' in the population is obs_support + 1 for each rival (following Lopez and Bowers 2026).
#'
#' @param obs_support A vector of integers representing the number of
#'   observations made in favor of the working hypothesis (anti-rival).
#'   Each element corresponds to evidence against a specific rival.
#'
#' @param rival_obs Optional. A vector of integers representing the number of
#'   observations actually made that support each rival. Must be the same length
#'   as obs_support. If NULL, assumes zero pro-rival observations.

#' @param evidence_matrix Optional. A matrix where rows are evidence types and
#'   columns are rivals. Entry i,j = 1 if evidence type i argues against
#'   rival j, 0 otherwise. If provided, obs_support is interpreted as counts
#'   of each evidence type.

#' @param weights Not used yet.
#'

#' @param odds The odds ratio for sampling (default 1 for central
#' hypergeometric). Currently a scalar indicating odds of seeing any working
#' theory observations versus any rivals. To complicate in future versions.

#'
#' @param interpretation Logical. If TRUE, returns interpretation text with p-value.
#'

#' @param check_evidence Logical. If TRUE, checks if obs_support are identical
#' across all rivals and suggests using find_p_multi_max_p() instead. By pass
#' this message, set `check_evidence=FALSE` and `messages=FALSE`
#'

#' @param messages Logical. If TRUE, prints out a long message if `obs_support`
#' is identical across all rivals and `rival_obs=NULL`. If FALSE, doesn't print
#' that message.

#' @return
#' Either a numeric p-value (scalar) or a list with p-value and interpretation.
#'
#' @examples
#' # Example 1:
#' # One kind of working theory supporting information that argues against multiple rivals where
#' # each rival has the same amount of information.
#' # Notice that we will get the same answer as if we used `find_p_two_types()` directly.
#' # But we present this here to illustrate.
#' # 4 rivals, 10 observations of one kind of working theory supporting
#' # observation, 10 total observations made
#' find_p_multi_max_p(obs_support = rep(10, 4), total_obs = rep(10, 4))
#' find_p_multi_mv(obs_support = c(10, 10, 10, 10))
#' find_p_two_types(obs_support = 10, total_obs = 10)
#' find_p_multi_max_p(obs_support = rep(10, 4), total_obs = rep(10, 4), odds = 2)
#' find_p_multi_mv(obs_support = c(10, 10, 10, 10), odds = 2, messages = FALSE)

#' # Example 2: No pro-rival observations made, different anti-rival evidence levels
#' find_p_multi_mv(obs_support = c(4, 3, 2, 1))
#'
#' # Example 3: With pro-rival observations
#' find_p_multi_mv(obs_support = c(4, 3, 2, 1), rival_obs = c(1, 1, 0, 0))
##'
##' # Example 4: With pro-rival observations and non-uniform odds
#' find_p_multi_mv(obs_support = c(4, 3, 2, 1), rival_obs = c(1, 1, 0, 0), odds = 2)
#' find_p_multi_mv(obs_support = c(4, 3, 2, 1), rival_obs = c(1, 1, 0, 0), odds = .5)


# Example 5: Overlapping evidence
#' evidence_patterns <- rbind(
#'   c(1, 1, 0), # antiMiasma and_antiFood
#'   c(1, 0, 0), # antiMiasma_only
#'   c(0, 0, 1) # antiAnimal_only
#' )
#' colnames(evidence_patterns) <- c("Miasma", "Food", "Animal")
#'
#' observed_counts <- c(4, 1, 2) # 4 overlap, 1 Miasma-only, 2 Animal-only
#' result <- find_p_multi_mv(
#'   obs_support = observed_counts,
#'   evidence_matrix = evidence_patterns,
#'   interpretation = TRUE
#' )
#'
#' print(result$interpretation)
#' print(result$p)

#' @importFrom BiasedUrn dMFNCHypergeo
#' @export

find_p_multi_mv <- function(obs_support, rival_obs = NULL,
                            evidence_matrix = NULL,
                            weights = NULL,
                            odds = 1, interpretation = FALSE,
                            check_evidence = TRUE, messages = TRUE) {
  k <- length(obs_support)
  stopifnot(all(obs_support >= 0))
  stopifnot("For now, odds must be scalar reflecting different chances of
            drawing working versus rival obs" = length(odds) == 1)

  ## Check if using same evidence against all rivals
  if (check_evidence && (is.null(rival_obs) || all(rival_obs == 0))) {
    unique_obs_support <- unique(obs_support)

    if (length(unique_obs_support) == 1) {
      if (messages) {
        message(strwrap("It looks like you have only one kind of evidence that is
        inconsistent with multiple rivals. You would over-state your evidence
        against the rivals if we used a multivariate null-model by repeating the
        same number of anti-Rival observations. We are reporting here the
        p-value from the find_p_multi_max_p command. If you actually do have
        multiple types of observations but happen to have the same numbers of
        them, then you should try this command again but set
        check_evidence=FALSE and messages=FALSE", prefix = " ", initial = ""))
      }

      res_p <- find_p_multi_max_p(
        obs_support = obs_support,
        total_obs = rep(unique_obs_support, k),
        rival_obs = NULL,
        odds = odds
      )
      return(res_p)
    }
  }

  # If evidence_matrix provided, handle overlapping evidence
  if (!is.null(evidence_matrix)) {
    # obs_support now represents counts of each EVIDENCE TYPE
    n_evidence_types <- length(obs_support)
    n_rivals <- ncol(evidence_matrix)

    stopifnot(nrow(evidence_matrix) == n_evidence_types)
    stopifnot(all(evidence_matrix %in% c(0, 1)))

    # Compute net evidence against each rival
    net_evidence_against <- as.numeric(obs_support %*% evidence_matrix)

    # Set up urn composition
    # For each evidence TYPE, urn has obs+1
    urn_evidence_types <- obs_support + 1

    # For rival observations, we need pro-rival evidence types
    # Create mirror evidence types for pro-rival
    urn_pro_rival_types <- urn_evidence_types # Same abundance for pro-rival

    total_obs <- sum(obs_support) + ifelse(is.null(rival_obs), 0, sum(rival_obs))

    # Urn composition by EVIDENCE TYPE
    urn_pop <- c(urn_evidence_types, urn_pro_rival_types)

    # Observed vector by EVIDENCE TYPE
    if (is.null(rival_obs)) {
      x_obs <- c(obs_support, rep(0, n_evidence_types))
      total_rival_obs <- 0
    } else {
      x_obs <- c(obs_support, rival_obs)
      total_rival_obs <- sum(rival_obs)
    }

    # Generate rejection region
    if (total_rival_obs == 0) {
      rejection_outcomes <- matrix(x_obs, nrow = 1)
    } else {
      # Generate allocations of pro-rival observations
      rival_allocations <- generate_compositions(total_rival_obs, n_evidence_types)

      n_allocations <- nrow(rival_allocations)
      rejection_outcomes <- matrix(0, nrow = n_allocations, ncol = length(x_obs))

      for (i in 1:n_allocations) {
        rejection_outcomes[i, ] <- c(obs_support, rival_allocations[i, ])
      }

      # Filter to respect urn constraints
      valid_outcomes <- apply(rejection_outcomes, 1, function(x) all(x <= urn_pop))
      rejection_outcomes <- rejection_outcomes[valid_outcomes, , drop = FALSE]
    }

    # Set up odds
    if (odds == 1) {
      odds_vec <- rep(1, length(urn_pop))
    } else {
      odds_vec <- c(rep(odds, n_evidence_types), rep(1, n_evidence_types))
    }

    # Compute probabilities
    probs <- apply(rejection_outcomes, 1, function(x) {
      return(dMFNCHypergeo(x = x, m = urn_pop, n = total_obs, odds = odds_vec))
    })

    res_p <- sum(probs)

    if (interpretation) {
      obs_prob <- dMFNCHypergeo(x = x_obs, m = urn_pop, n = total_obs, odds = odds_vec)

      # Report net evidence against each rival
      interpretation_text <- sprintf(
        "Evidence pattern (overlapping):\n"
      )

      for (i in 1:n_evidence_types) {
        rivals_affected <- colnames(evidence_matrix)[evidence_matrix[i, ] == 1]
        interpretation_text <- paste0(
          interpretation_text,
          sprintf(
            "  %d observations argue against: %s\n",
            obs_support[i], paste(rivals_affected, collapse = " & ")
          )
        )
      }

      interpretation_text <- paste0(
        interpretation_text,
        sprintf("\nNet evidence against each rival:\n")
      )

      for (j in 1:n_rivals) {
        interpretation_text <- paste0(
          interpretation_text,
          sprintf(
            "  %s: %d pieces\n", colnames(evidence_matrix)[j],
            net_evidence_against[j]
          )
        )
      }

      interpretation_text <- paste0(
        interpretation_text,
        sprintf("\nP-value: %.4f\n", res_p),
        sprintf("Probability of observed pattern: %.6f\n", obs_prob)
      )

      if (res_p <= 0.05) {
        interpretation_text <- paste0(
          interpretation_text,
          "\nInterpretation: We reject the union null hypothesis.\n"
        )
      } else {
        interpretation_text <- paste0(
          interpretation_text,
          "\nInterpretation: We cannot reject the union null hypothesis.\n"
        )
      }

      return(list(
        p_value = res_p,
        observed_prob = obs_prob,
        net_evidence_against = net_evidence_against,
        n_outcomes_rejection_region = nrow(rejection_outcomes),
        interpretation = interpretation_text
      ))
    }

    return(res_p)
  }

  ### The rest of the function is for the non-overlapping approach
  ## Set up urn composition and observations
  if (is.null(rival_obs)) {
    rival_obs <- rep(0, k)
    urn_rival <- obs_support + 1 # Conservative: pop has obs+1 for each rival
  } else {
    stopifnot(length(rival_obs) == k)
    stopifnot(all(rival_obs >= 0))
    urn_rival <- pmax(obs_support + 1, rival_obs) # At least obs+1 or observed
  }

  total_obs <- sum(obs_support) + sum(rival_obs)

  ## Urn composition: [anti-rival for each rival, pro-rival for each rival, neutral]
  urn_pop <- c(obs_support, urn_rival)

  ## Observed vector
  x_obs <- c(obs_support, rival_obs)

  ## Set up odds
  if (odds == 1) {
    odds <- rep(odds, length(urn_pop))
  } else {
    ## Set the odds of the rival at 1 and then enable lower or higher odds for the observed data
    odds <- c(rep(odds, length(obs_support)), rep(1, length(urn_rival)))
  }

  ## Generate rejection region outcomes
  ## Rejection region: all outcomes with anti-rival >= observed AND
  ## total pro-rival = sum(rival_obs)

  total_rival_obs <- sum(rival_obs)

  if (total_rival_obs == 0) {
    ## Simple case: no pro-rival observations
    ## Only one outcome in rejection region: the observed outcome
    rejection_outcomes <- matrix(x_obs, nrow = 1)
  } else {
    ## Complex case: generate all ways to allocate total_rival_obs across k rivals
    ## Use compositions to enumerate all non-negative integer solutions

    # Generate all k-way partitions of total_rival_obs
    rival_allocations <- generate_compositions(total_rival_obs, k)
    ## TODO we can do this next loop stuff faster.
    # For each allocation, create full outcome vector
    n_allocations <- nrow(rival_allocations)
    rejection_outcomes <- matrix(0,
      nrow = n_allocations,
      ncol = length(x_obs)
    )

    for (i in 1:n_allocations) {
      rejection_outcomes[i, ] <- c(obs_support, rival_allocations[i, ])
    }

    # Filter to respect urn population constraints
    valid_outcomes <- apply(rejection_outcomes, 1, function(x) {
      return(all(x <= urn_pop))
    })

    rejection_outcomes <- rejection_outcomes[valid_outcomes, , drop = FALSE]

    if (nrow(rejection_outcomes) == 0) {
      stop("No valid outcomes in rejection region given urn constraints")
    }
  }

  ## Compute probability for each outcome in rejection region
  probs <- apply(rejection_outcomes, 1, function(x) {
    a_p <- dMFNCHypergeo(
      x = x, m = urn_pop, n = total_obs, odds = odds
    )
    return(a_p)
  })

  ## P-value is sum of probabilities
  res_p <- sum(probs)

  ## Optional: detailed output
  if (interpretation) {
    obs_prob <- dMFNCHypergeo(
      x = x_obs, m = urn_pop, n = total_obs,
      odds = odds
    )

    interpretation_text <- sprintf(
      "P-value: %.4f\nProbability of observed outcome: %.6f\nNumber of outcomes in rejection region: %d\n\n",
      res_p, obs_prob, nrow(rejection_outcomes)
    )

    if (res_p <= 0.05) {
      interpretation_text <- paste0(
        interpretation_text,
        "Interpretation: We reject the union null hypothesis (p = ",
        round(res_p, 4), "). ",
        "The observed evidence pattern would occur by chance only ",
        round(res_p * 100, 1), "% of the time if at least one rival theory ",
        "were actually correct. This provides strong statistical evidence that ",
        "all ", k, " rival theories are wrong."
      )
    } else {
      interpretation_text <- paste0(
        interpretation_text,
        "Interpretation: We cannot reject the union null hypothesis (p = ",
        round(res_p, 4), "). ",
        "The observed evidence pattern is not sufficiently unlikely under ",
        "the hypothesis that at least one rival theory is correct."
      )
    }

    return(list(
      p_value = res_p,
      observed_prob = obs_prob,
      n_outcomes_rejection_region = nrow(rejection_outcomes),
      rejection_outcomes = rejection_outcomes,
      outcome_probs = probs,
      interpretation = interpretation_text
    ))
  }

  return(res_p)
}


#' Generate all compositions of n into k parts
#'
#' @description This is a helper function for `find_p_multi_mv()`
#'
#' @param n Integer to partition
#' @param k Number of parts
#'
#' @return Matrix where each row is a k-way composition of n
#' @importFrom partitions compositions
#' @export
generate_compositions <- function(n, k) {
  # partitions::compositions returns columns as compositions, we want rows
  comps <- partitions::compositions(n, k, include.zero = TRUE)

  # Transpose so each row is a composition
  return(t(comps))
}
