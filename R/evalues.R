#' Calibrate p-values into e-values
#'
#' @description
#' Convert one or more p-values into e-values using simple, standard
#' p-to-e calibration rules from the e-value literature
#' (e.g. Vovk & Wang, 2021; Ramdas et al., 2025). An e-value is a
#' nonnegative statistic with expectation at most 1 under the null; large
#' values indicate evidence against the null. [oai_citation:2‡Project Euclid](https://projecteuclid.org/journals/annals-of-statistics/volume-49/issue-3/E-values-Calibration-combination-and-applications/10.1214/20-AOS2020.pdf?utm_source=chatgpt.com)
#'
#' This helper is primarily used internally by DrBristol but is exported
#' for convenience.
#'
#' @param p Numeric vector of p-values in (0,1]. Values \eqn{\le 0} or
#'   \eqn{> 1} are not allowed and will trigger an error.
#' @param method Character scalar specifying the calibration rule:
#'   \itemize{
#'     \item `"inverse"`: returns \eqn{e = 1/p}. This is often used
#'       informally as an "e-like" evidence measure. Note that for a
#'       generic valid p-variable this may not be a *mathematically*
#'       valid e-variable in the strict Ramdas–Vovk sense; it is,
#'       however, a monotone transform with the usual interpretation
#'       that larger values indicate more evidence against the null.
#'     \item `"kappa"`: Vovk–Wang admissible κ-calibrator
#'       \eqn{e = \kappa p^{\kappa-1}}, with \eqn{0 < \kappa < 1}.
#'       This does yield a proper e-variable when applied to a valid
#'       p-variable. [oai_citation:3‡Wikipedia](https://en.wikipedia.org/wiki/E-values?utm_source=chatgpt.com)
#'   }
#' @param kappa Numeric scalar in (0,1). Only used when
#'   \code{method = "kappa"}.
#' @param cap Optional numeric upper cap for the returned e-values
#'   (e.g., to avoid overflow when \code{p} is extremely small). Default
#'   \code{Inf} means no capping.
#'
#' @return
#' A numeric vector of e-values of the same length as \code{p}.
#'
#' @examples
#' e_from_p(0.05) # 1/p style
#' e_from_p(c(0.1, 0.01)) # vectorised
#' e_from_p(0.05, method = "kappa", kappa = 0.5)
#'
#' @export
e_from_p <- function(p,
                     method = c("inverse", "kappa"),
                     kappa = 0.5,
                     cap = Inf) {
    method <- match.arg(method)

    if (!is.numeric(p)) {
        stop("`p` must be numeric.", call. = FALSE)
    }

    if (any(!is.finite(p))) {
        stop("All p-values must be finite numbers in (0, 1].", call. = FALSE)
    }

    if (any(p <= 0 | p > 1)) {
        stop("All p-values must lie strictly in (0, 1].", call. = FALSE)
    }

    if (!is.numeric(cap) || length(cap) != 1L || cap <= 0) {
        stop("`cap` must be a positive scalar.", call. = FALSE)
    }

    if (method == "inverse") {
        e <- 1 / p
    } else { # method == "kappa"
        if (!is.numeric(kappa) || length(kappa) != 1L || kappa <= 0 || kappa >= 1) {
            stop("`kappa` must be a scalar in (0, 1) when method = 'kappa'.",
                call. = FALSE
            )
        }
        # Vovk–Wang κ-calibrator: e = κ p^{κ-1}
        e <- kappa * p^(kappa - 1)
    }

    e[e > cap] <- cap
    return(e)
}


#' E-values for the +1 hypergeometric urn model
#'
#' @description
#' Compute e-values for the DrBristol +1 hypergeometric urn model by
#' first obtaining the conservative upper bound on the p-value under the
#' rival theory and then calibrating that p-value into an e-value.
#'
#' Concretely, this function:
#' \enumerate{
#'   \item Calls [find_p_two_types()] with \code{interpretation = FALSE}
#'     to obtain the upper bound \eqn{p_{\max}} on the p-value under the
#'     +1 urn model. [oai_citation:4‡p_value_process_tracing.pdf](file-service://file-QZ1mjQ3FjfNcsuGnfUiwoe)
#'   \item Applies [e_from_p()] to \eqn{p_{\max}} using the chosen
#'     calibration (inverse or κ-calibrator).
#' }
#'
#' Because \eqn{p_{\max}} is conservative, the resulting e-value is
#' likewise conservative: it is the *minimum* e-value consistent with
#' the +1 urn model and the chosen calibrator.
#'
#' @inheritParams find_p_two_types
#' @param method Character; passed to [e_from_p()] (either `"inverse"`
#'   or `"kappa"`).
#' @param kappa Numeric; κ parameter for the `"kappa"` method.
#' @param cap Upper cap for the returned e-value; passed to
#'   [e_from_p()].
#' @param interpretation Logical. If \code{FALSE} (default), return a
#'   numeric e-value. If \code{TRUE}, return a list with the e-value and
#'   a human-readable character string summarizing the result.
#'
#' @return
#' If \code{interpretation = FALSE}, a single numeric e-value.
#' If \code{interpretation = TRUE}, a list with components:
#' \itemize{
#'   \item \code{e}: the e-value (numeric scalar).
#'   \item \code{interp}: a character string describing the result.
#' }
#'
#' @seealso
#' [find_p_two_types()], [e_from_p()], [sens_urn_evalue()]
#'
#' @examples
#' # Basic: 7 supportive observations out of 10 total
#' find_e_two_types(obs_support = 7, total_obs = 10)
#'
#' # With textual interpretation
#' res <- find_e_two_types(
#'     obs_support = 7, total_obs = 10,
#'     interpretation = TRUE
#' )
#' res$e
#' res$interp
#'
#' # Using κ-calibration
#' find_e_two_types(
#'     obs_support = 7, total_obs = 10,
#'     method = "kappa", kappa = 0.5
#' )
#'
#' @export
find_e_two_types <- function(obs_support,
                             total_obs,
                             weights = NULL,
                             odds = 1,
                             method = c("inverse", "kappa"),
                             kappa = 0.5,
                             cap = Inf,
                             interpretation = FALSE) {
    method <- match.arg(method)

    # We rely on find_p_two_types() as implemented elsewhere in DrBristol.
    p_max <- find_p_two_types(
        obs_support = obs_support,
        total_obs = total_obs,
        weights = weights,
        odds = odds,
        interpretation = FALSE
    )

    if (!is.numeric(p_max) || length(p_max) != 1L || !is.finite(p_max)) {
        stop("find_p_two_types() did not return a finite numeric scalar p-value.",
            call. = FALSE
        )
    }

    e_val <- e_from_p(
        p      = p_max,
        method = method,
        kappa  = kappa,
        cap    = cap
    )

    if (!interpretation) {
        return(e_val)
    }

    # Build a descriptive message
    weight_str <- if (is.null(weights)) {
        "all evidentiary weights equal to 1"
    } else {
        paste0("evidentiary weights ", deparse(weights), collapse = "")
    }

    interp <- sprintf(
        paste0(
            "Under the +1 hypergeometric urn model, the upper bound on the ",
            "p-value is p <= %.4g. Using method='%s'%s, this corresponds to ",
            "an e-value of e = %.4g."
        ),
        p_max,
        method,
        if (method == "kappa") sprintf(" with kappa=%.3f", kappa) else "",
        e_val
    )

    return(list(
        e      = e_val,
        interp = interp
    ))
}


#' Sensitivity analysis in the e-value scale
#'
#' @description
#' Wrapper around [sens_obs_two_types()] that works on the e-value scale. For a
#' given e-value threshold \eqn{e_\star}, this function finds the odds
#' ratio \eqn{\omega} at which the conservative e-value
#' (as returned by [find_e_two_types()]) would be equal to \eqn{e_\star}.
#'
#' Because the underlying calibration is via \eqn{p_{\max}} and
#' [e_from_p()], this is equivalent to asking for the odds ratio at which
#' the p-value upper bound would be \eqn{\alpha_\star = 1 / e_\star},
#' and then calling [sens_obs_two_types()] with \code{p_threshold = alpha_star}. [oai_citation:5‡p_value_process_tracing.pdf](file-service://file-QZ1mjQ3FjfNcsuGnfUiwoe)
#'
#' @inheritParams sens_obs_two_types
#' @param e_threshold Numeric scalar \eqn{> 0}. Target e-value at which
#'   to assess sensitivity.
#'
#' @return
#' Whatever [sens_obs_two_types()] returns (typically a list including the
#' odds-ratio \eqn{\omega}); the only difference is that you parameterize
#' the problem by an e-value threshold rather than a p-value threshold.
#'
#' @examples
#' # Suppose we observed 7 supportive pieces of evidence out of 10.
#' # How large must the observation bias be for the e-value to drop to ~2
#' # (i.e. roughly p >= 0.5)?
#' sens_urn_evalue(obs_support = 7, total_obs = 10, e_threshold = 2)
#'
#' @export
sens_urn_evalue <- function(obs_support,
                            total_obs,
                            rival_obs = NULL,
                            weights = NULL,
                            e_threshold) {
    if (!is.numeric(e_threshold) ||
        length(e_threshold) != 1L ||
        !is.finite(e_threshold) ||
        e_threshold <= 0) {
        stop("`e_threshold` must be a positive finite scalar.", call. = FALSE)
    }

    # e = 1 / alpha => alpha = 1 / e
    p_threshold <- 1 / e_threshold

    return(sens_obs_two_types(
        obs_support = obs_support,
        total_obs   = total_obs,
        rival_obs   = rival_obs,
        weights     = weights,
        p_threshold = p_threshold
    ))
}
