# 0.6.0

## Update

This is an update of the CRAN version 0.4.0.1. Version 0.5.0 was developed but
never submitted, so this release also carries its changes. The full list is in
NEWS.md; the points most relevant to the check results are:

* The SEM likelihood, previously implemented in C++ via `Rcpp` and
  `RcppArmadillo`, is now implemented in R and differentiated with automatic
  differentiation provided by `RTMB`. The optimization uses exact gradients
  instead of finite differences, and standard errors are computed from the
  exact Hessian. Values of the likelihood at given parameters are unchanged;
  optimized parameters and standard errors may differ slightly and are more
  accurate.
* Consequently the package no longer contains compiled code: the `src`
  directory has been removed, and the `Rcpp`, `RcppArmadillo`, `rootSolve`
  and `optimbase` dependencies have been dropped in favour of
  `RTMB (>= 1.6)`. This also resolves the installed package size NOTE
  reported on r-oldrel-macos-x86_64 for 0.4.0.
* S3 classes and methods were added for the objects returned by `bma()` and
  `optim_model_space()` (`print()`, `summary()`, `coef()`, `plot()`).
* The minimum required R version was raised from 3.5 to 4.4. `RTMB` depends
  on `TMB`, which imports `Matrix`, and `Matrix` requires R (>= 4.4); the
  previous declaration could not be satisfied in practice.
* The bundled example datasets were regenerated with the current code.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

  This NOTE reflects the check machine being unable to reach the external
  time service used to detect future-dated files. It is unrelated to the
  package.

## Downstream dependencies

There are no downstream dependencies on CRAN.

# 0.4.0.1

## Patch release

Addresses two issues found during additional CRAN checks on 0.4.0:

* Added `-DARMA_NO_DEBUG` compiler flag to reduce compiled library size,
  addressing installed package size NOTE on r-oldrel-macos-x86_64.
* Relaxed numerical tolerance in test for `optim_model_space_params` to
  accommodate differences across BLAS/LAPACK implementations (ATLAS, MKL).
  The optimization involves matrix inversions and determinants via
  RcppArmadillo that can produce slightly different results depending
  on the BLAS backend.

## R CMD check results

0 errors | 0 warnings | 1 note

# 0.4.0

## New submission

This package was previously published on CRAN as `bdsm` (versions 0.1.0 through 0.3.0).
It has been renamed to `badp` (Bayesian Averaging for Dynamic Panels).
We kindly request that the old `bdsm` package be archived.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

# 0.3.0

## Resubmission

* Reimplemented SEM likelihood computation in C++ for improved performance.


# 0.2.2

# Resubmission

* Modified the method for selecting beta coefficient rows in the `bma` function for improved robustness and compatibility.
* Updated tests to align with the upcoming ggplot2 release (v4.0.0).

# 0.2.1

# Resubmission

Added only vignette as it was causing issues for auto check in previous version.
The PDF file size shouldn't be a problem, but auto check claims it can be vastly reduced, which does not seem to be the case.

# 0.2.0

# Resubmission

Re-factored functions for calling the BMA summary.
Expanded package documentation and README as preparing for the publication.

# 0.1.0

# Resubmission

Some tests were marked as skip on CRAN as they were failing due to 
different seeding methods.

The description was modified to be more precise and to include the 
references describing the methods implemented in the package.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
