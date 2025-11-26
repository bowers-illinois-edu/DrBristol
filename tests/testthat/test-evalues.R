test_that("e_from_p basic behaviour is correct", {
    # inverse method
    expect_equal(e_from_p(0.5), 2)
    expect_equal(e_from_p(0.25), 4)

    # vectorisation
    ps <- c(0.1, 0.01)
    es <- e_from_p(ps)
    expect_equal(es, 1 / ps)

    # kappa method (κ-calibrator)
    kappa <- 0.5
    p <- 0.05
    e_k <- e_from_p(p, method = "kappa", kappa = kappa)
    expect_equal(e_k, kappa * p^(kappa - 1))

    # input checks
    expect_error(e_from_p(-0.1), "in \\(0, 1\\]")
    expect_error(e_from_p(1.1), "in \\(0, 1\\]")
    expect_error(e_from_p("0.1"), "numeric")
    expect_error(e_from_p(0.1, cap = -1), "positive scalar")
    expect_error(
        e_from_p(0.1, method = "kappa", kappa = 1.5),
        "kappa.*\\(0, 1\\)"
    )
})

test_that("find_e_two_types agrees with find_p_two_types for inverse calibration", {
    # Use an example from the paper: 7 supportive out of 10 total
    p_max <- find_p_two_types(obs_support = 7, total_obs = 10)
    e_val <- find_e_two_types(
        obs_support = 7, total_obs = 10,
        method = "inverse"
    )

    expect_equal(e_val, 1 / p_max)

    # Check interpretation option
    res <- find_e_two_types(
        obs_support = 7,
        total_obs = 10,
        method = "inverse",
        interpretation = TRUE
    )

    expect_type(res, "list")
    expect_true(is.numeric(res$e))
    expect_true(is.character(res$interp))
    expect_match(res$interp, "hypergeometric urn model")
})

test_that("sens_urn_evalue matches sens_obs_two_types on the p-scale", {
    # Choose some moderate configuration
    obs_support <- 7
    total_obs <- 10
    e_threshold <- 2 # corresponds to p_threshold = 0.5

    out_e <- sens_urn_evalue(
        obs_support = obs_support,
        total_obs   = total_obs,
        e_threshold = e_threshold
    )

    out_p <- sens_obs_two_types(
        obs_support = obs_support,
        total_obs   = total_obs,
        p_threshold = 1 / e_threshold
    )

    # Both helpers should return lists; if they both have a `w`
    # component (odds ratio), they should agree.
    expect_true(is.list(out_e))
    expect_true(is.list(out_p))

    if (!is.null(out_e$w) && !is.null(out_p$w)) {
        expect_equal(out_e$w, out_p$w)
    }
})
