# 0.7.0

## Update

This is a minor update of the CRAN version 0.6.1. It reworks the user-facing
interface in response to a review of the manuscript submitted to the Journal
of Statistical Software. Several changes are breaking; they are listed in
NEWS.md and summarized here:

* Nine accessor generics were added (`bma_table()`, `pip()`, `pmp()`,
  `model_size_table()`, `regressors()`, `n_models()`, `weighting()`,
  `learning_rate()`, `convergence()`), so that results no longer have to be
  extracted by position or by component name.
* `best_models()` returns a list of `badp_model` objects carrying numeric
  coefficients, standard errors and p-values, rather than nine tables already
  formatted for display. The new classes have `print()`, `summary()`,
  `coef()`, `plot()` and `[` methods.
* `model_sizes()`, `model_pmp()`, `coef_hist()` and `posterior_dens()` no
  longer draw as a side effect. They return a classed collection whose
  `print()` method draws one figure.
* The residual degrees of freedom were removed. The estimator is maximum
  likelihood with standard errors from the observed information, so the Wald
  statistics are referred to the standard normal rather than to a t
  distribution.
* The first argument of six functions was renamed from `bma_list` to `x`,
  for consistency with the accessors and with the S3 methods.
* `join_lagged_col()` is no longer marked as internal and now appears in the
  package index.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

  This NOTE reflects the check machine being unable to reach the time server
  and is unrelated to the package.

Checked locally with `R CMD check --as-cran`, including the CRAN incoming
checks and the PDF manual, against R 4.4.1 on macOS (aarch64-apple-darwin20),
and via the GitHub Actions R-CMD-check workflow on Linux, macOS and Windows
across release, oldrel and devel.

The reference manual builds without error, and the CRAN incoming feasibility
check reports no problems.

## Downstream dependencies

There are no downstream dependencies on CRAN.

# 0.6.1

## Update

This is a patch update of the CRAN version 0.6.0. It contains no changes to
code, documentation or data; the only changes are to package metadata:

* The `LICENSE` and `LICENSE.md` files now attribute the copyright to
  "badp authors" over the year range 2021-2026, rather than to a single
  author and a single year.
* An e-mail address was added for the author Mariusz Szczepanczyk in
  `Authors@R`.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking HTML version of manual ... NOTE
  Skipping checking HTML validation: 'tidy' doesn't look like recent enough
  HTML Tidy

  This NOTE reflects the version of HTML Tidy installed on the check machine
  and is unrelated to the package.

Checked locally with `R CMD check --as-cran` against R 4.5.1 on
macOS (aarch64-apple-darwin20), and via the GitHub Actions R-CMD-check
workflow on Linux, macOS, and Windows across release, oldrel, and devel.

## Downstream dependencies

There are no downstream dependencies on CRAN.

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
  and `optimbase` dependencies have been dropped in favor of
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

# 0.5.0

## Minor release

This release adds S3 classes and methods for `bma()` and `optim_model_space()`
output (for JSS compliance), tightens the public API, and ships new example
datasets. See `NEWS.md` for the full list of changes. Notable user-visible
changes:

* `bma()` now returns an object of class `badp_bma` with `print()`, `summary()`, `coef()`, and `plot()` methods. `optim_model_space()` returns a
  `badp_model_space` object with a `print()` method.
* Breaking change: `best_models()` now takes a character `prior` argument
  ("binomial" / "beta") in place of the integer `criterion` argument, aligning
  it with `summary.badp_bma()`.
* Breaking change: the `dil.Par` parameter has been renamed to `omega` for
  consistency with the statistical literature.
* New `migration_data` dataset and two example model space objects
  (`migration_model_space`, `migration_model_space_nonnested`) from
  Afonso, Alves, & Beck (2025).
* Removed `ggpubr` dependency; plot arrangement now uses `patchwork`.
* Documentation, internal naming, and spelling cleanups throughout.

## R CMD check results

0 errors | 0 warnings | 0 notes

Checked locally with `R CMD check --as-cran` against R 4.5.1 on
macOS (aarch64-apple-darwin20) and via the GitHub Actions
R-CMD-check workflow on Linux, macOS, and Windows across release,
oldrel, and devel.

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
