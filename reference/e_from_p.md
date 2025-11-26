# Calibrate p-values into e-values

Convert one or more p-values into e-values using simple, standard p-to-e
calibration rules from the e-value literature (e.g. Vovk & Wang, 2021;
Ramdas et al., 2025). An e-value is a nonnegative statistic with
expectation at most 1 under the null; large values indicate evidence
against the null. [oai_citation:2‡Project
Euclid](https://projecteuclid.org/journals/annals-of-statistics/volume-49/issue-3/E-values-Calibration-combination-and-applications/10.1214/20-AOS2020.pdf?utm_source=chatgpt.com)

This helper is primarily used internally by DrBristol but is exported
for convenience.

## Usage

``` r
e_from_p(p, method = c("inverse", "kappa"), kappa = 0.5, cap = Inf)
```

## Arguments

- p:

  Numeric vector of p-values in (0,1\]. Values \\\le 0\\ or \\\> 1\\ are
  not allowed and will trigger an error.

- method:

  Character scalar specifying the calibration rule:

  - `"inverse"`: returns \\e = 1/p\\. This is often used informally as
    an "e-like" evidence measure. Note that for a generic valid
    p-variable this may not be a *mathematically* valid e-variable in
    the strict Ramdas–Vovk sense; it is, however, a monotone transform
    with the usual interpretation that larger values indicate more
    evidence against the null.

  - `"kappa"`: Vovk–Wang admissible κ-calibrator \\e = \kappa
    p^{\kappa-1}\\, with \\0 \< \kappa \< 1\\. This does yield a proper
    e-variable when applied to a valid p-variable.
    [oai_citation:3‡Wikipedia](https://en.wikipedia.org/wiki/E-values?utm_source=chatgpt.com)

- kappa:

  Numeric scalar in (0,1). Only used when `method = "kappa"`.

- cap:

  Optional numeric upper cap for the returned e-values (e.g., to avoid
  overflow when `p` is extremely small). Default `Inf` means no capping.

## Value

A numeric vector of e-values of the same length as `p`.

## Examples

``` r
e_from_p(0.05) # 1/p style
#> [1] 20
e_from_p(c(0.1, 0.01)) # vectorised
#> [1]  10 100
e_from_p(0.05, method = "kappa", kappa = 0.5)
#> [1] 2.236068
```
