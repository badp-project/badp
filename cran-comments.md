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
The PDF file size shouldn't be a problem, but auto check claims it can be 
vastly reduced, which does not seem to be the case.

# 0.2.0

# Resubmission

Re-factored functions for calling the BSM summary.
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
