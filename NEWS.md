# badp 0.6.0

* Replaced the C++ (Rcpp/RcppArmadillo) SEM likelihood implementation with an
  R implementation differentiated via `RTMB` (automatic differentiation).
  Optimization now uses gradients obtained by automatic differentiation
  instead of finite differences, and standard errors are computed from the
  Hessian and per-entity score vectors of the same tape instead of
  finite-difference approximations. Optimized parameters and standard errors
  therefore differ from those of earlier versions; likelihood values at given
  parameters are unchanged. The difference is small for the parameter
  estimates but can be substantial for the robust standard errors, which
  depend on the derivatives twice over. The finite-difference
  `hessian()` function was removed. The `Rcpp`, `RcppArmadillo`, `rootSolve`
  and `optimbase` dependencies were dropped in favour of `RTMB`.
* `init_value` (in `optim_model_space()` and related functions) now also
  accepts a generator function of one argument `n` returning `n` starting
  values (e.g. `function(n) runif(n, 0.1, 1)`), enabling randomized
  multi-start experiments, or a single number used as the starting value for
  every parameter, as before (`init_value = 0.5` is equivalent to
  `function(n) rep(0.5, n)`), so code written against 0.4.0 and 0.5.0
  continues to work. Passing `0`, a vector of length greater than one, or
  anything that is neither a function nor a single finite number now fails
  with an informative error, as does a generator that draws exactly `0` for
  an included parameter; zero is rejected either way because it is reserved
  to mark a parameter excluded from a given model.
* Per-model optimization is more robust to the harder cases automatic
  differentiation now makes tractable to explore:
    * BFGS is restarted from its own solution until the log-likelihood value
      stops improving by more than `restart_tol` (`max_restarts` and
      `restart_tol` arguments of `optim_model_space()`); with
      `max_restarts = 0`, `optim()`'s own convergence flag is used directly.
    * A starting point at which the likelihood is undefined, and an
      automatic-differentiation error encountered mid-optimization, no
      longer abort the procedure: the offending point is treated as a
      rejected step, letting the line search shrink it, or, if it was the
      initial point, redrawn from `init_value` (up to `max_init_attempts`
      times).
    * A model whose observed information matrix is not positive definite,
      or not finite, is re-optimized from a freshly drawn starting point (up
      to `max_reoptimizations` times) instead of being reported as a
      failure.
    * Per-model convergence diagnostics - converged flag, `optim` code,
      number of restarts, number of initial draws, and final gradient norm -
      are stored in the new `convergence` element of `badp_model_space`
      objects. Non-converged models trigger a warning in
      `optim_model_space()` and `bma()` and are reported by
      `summary()`/`print()`; they are deliberately not excluded from the
      analysis.

* The model space statistics gained a row holding the numerical rank of
  `J = sum_i s_i s_i'`, the outer product of the entity-level scores from
  which the robust ("sandwich") standard errors are built. Because the
  per-entity scores share a component identical across entities, and because
  they sum to zero at the maximum, `J` is unchanged by centring them and
  depends only on the variation of the scores across entities. Parameters
  entering the log-likelihood solely through terms common to every entity
  contribute nothing, so `J` is rank deficient however many entities are
  observed. In the bundled model spaces the scores span between 8% and 29%
  of the parameter directions.

  The robust standard deviations that `bma()` reports are unaffected in the
  sense that they remain well defined: `J` vanishes outside the spanned
  block, so the sandwich restricted to that block equals the profile
  sandwich obtained by profiling the remaining parameters out. What the
  construction discards is the score covariance involving those remaining
  directions, which is set to zero rather than estimated. The likelihood,
  the posterior means, the posterior inclusion probabilities and the
  Hessian-based standard errors `PSD` and `PSDcon` do not involve `J`.

  `summary()` of a model space now reports the fraction of parameter
  directions spanned.
* `bma()` gained an `eta` argument taking the learning rate directly, which
  overrides `weighting`. `eta = 1` reproduces `"mb2012"` and `eta = 1/N`
  reproduces `"mb2016"`, so the sensitivity of any conclusion to the rate can
  be examined without re-estimation.
* The `"curvature"` weighting, which estimated the learning rate by the
  magnitude ("omnibus") adjustment for misspecified likelihoods, has been
  withdrawn before release. The adjustment assumes `J` estimates the
  variance of the score, and `J` is rank deficient for this likelihood, so
  the rate would be calibrated on the small part of the parameter space the
  entity-level scores span, with no way to assess what the remainder
  contributes. Use `eta` to set a rate explicitly instead. The ingredients,
  `tr(H^-1 J)`, `dim(theta)` and `rank(J)`, remain stored with every fitted
  model space, so the rate can still be computed and inspected directly.
* `bma()` gained a `weighting` argument selecting the approximation to the
  marginal likelihood used to weight the models. Writing
  `A_j = loglik_j - (k_j/2)*log(N*T)`, three of the four options are the same
  construction with different learning rates `eta`, `log w_j = eta * A_j`:
    * `"mb2016"` (default, `eta = 1/N`) is unchanged behaviour, the
      approximation computed by the implementation accompanying Moral-Benito
      (2016), and the option that reproduces the posterior inclusion
      probabilities and posterior moments published there;
    * `"mb2012"` (`eta = 1`) is the Schwarz criterion exactly as stated in
      equations (24)-(30) of Moral-Benito (2012);
    * `"nt"` (`eta = 1/(N*T)`) averages over entity-periods rather than
      entities, the scaling that would be internally consistent with the
      `log(N*T)` penalty.

  The fourth option, `"uip"`, instead alters the penalty, using
  `exp(loglik - (k/2)*log(N))` with the entity as the unit of information, as
  implied by the unit information prior of Kass and Wasserman (1995) given
  that the likelihood factorises over entities.

  Any `eta != 1` gives a tempered (power) posterior over models, which lies
  outside the approximation that motivates the criterion. A rate held fixed
  as the sample grows only rescales the log weights, so the posterior still
  concentrates, more slowly for `eta < 1`. A rate that shrinks with the
  sample is different in kind: under `eta = 1/N` the log weights converge to
  constants and the posterior never concentrates. Conversely `eta = 1`
  concentrates sharply and can place nearly all posterior mass on a single
  model. The choice can therefore change posterior inclusion
  probabilities materially, and users are encouraged to check the sensitivity
  of their conclusions. Switching between the options requires no
  re-estimation, as all are recovered from the same fitted model space.
* The selected weighting and the realised learning rate are recorded in the
  new `weighting` and `eta` elements (slots 18 and 19) of `badp_bma` objects.
  Existing slots 1-17 are unchanged, so numeric indexing of earlier elements
  continues to work.
* Model weights are now formed on the log scale and shifted before
  exponentiation, which prevents overflow when log Bayes factors are large.

* `sem_sigma_matrix()` builds variances with multiplication rather than
  `^2`, avoiding a `NaN` second derivative that automatic differentiation
  would otherwise produce when a variance parameter is exactly zero.
* `sem_C_matrix()` validates that `phi_1` is supplied with the same length
  as `beta`.
* `RTMB (>= 1.6)` is now required, for automatic-differentiation support in
  `chol()` and `determinant()`.
* The minimum required R version was raised from 3.5 to 4.4, to match the
  requirement of `Matrix`, on which `RTMB` depends through `TMB`. The
  previous declaration could not be satisfied in practice.
* Regenerated every bundled model space (`small_model_space`,
  `full_model_space`, `model_space_nonnested`, `migration_model_space` and
  `migration_model_space_nonnested`) and `full_bma_results` with the fixed
  automatic differentiation pipeline. `model_space_nonnested` had no
  generating script; `data-raw/model_space_nonnested.R` now provides one.

# badp 0.5.0

* **Breaking change**: `best_models()` now takes a character `prior` argument
  in place of the integer `criterion` argument. Use `prior = "binomial"`
  (default) instead of `criterion = 1`, and `prior = "beta"` instead of
  `criterion = 2`. This brings the API in line with `summary.badp_bma()`,
  which already used `prior = "binomial" | "beta"`.
* **Breaking change**: Renamed `dil.Par` parameter to `omega` for clarity and consistency with statistical literature.
* Added S3 classes and methods for JSS compliance:
    * `bma()` now returns an object of class `badp_bma` (previously unclassed list).
    * `optim_model_space()` now returns an object of class `badp_model_space`.
    * Implemented S3 methods for `badp_bma` objects:
        * `print.badp_bma()` - Clean, informative console output.
        * `summary.badp_bma()` - Detailed statistical summary with highlighted important variables. Enhanced to display BMA statistics for both binomial and binomial-beta priors simultaneously.
        * `coef.badp_bma()` - Extract coefficients with optional standard errors and PIPs.
        * `plot.badp_bma()` - Default visualization with dispatch to existing plot functions.
    * Implemented `print.badp_model_space()` for model space objects.
    * Fixed component names in `bma()` output: removed spaces, duplicates, and typos; all names are now valid R identifiers (e.g., `uniform_table`, `random_table`, `reg_names`, `dilution`, `alphas`).
    * **Compatibility note**: Numeric indexing (`results[[3]]`) and helper functions (`best_models()`, `jointness()`, etc.) are fully preserved. Named access is available via the new identifiers (e.g., `results$reg_names`), but code using the previous long component names must be updated.
    * Added comprehensive tests for S3 methods and for preserved numeric-indexing/helper-function compatibility (125 new tests).
* Improved documentation: Added `@keywords internal` to hide helper and implementation functions from user-facing help documentation.
* Replaced `sem_likelihood` example: use the bundled `economic_growth` dataset instead of small random data that could produce `NA` or invalid positive values on some platforms.
* Removed `ggpubr` dependency; plotting functions now use `patchwork` for plot arrangement.
* Added `migration_data` dataset with migration flows data from Afonso, Alves, & Beck (2025).
* Added `migration_model_space` and `migration_model_space_nonnested` example model space objects.
* Fixed `feature_standardization` function to handle tibble input correctly.
* Exported `join_lagged_col` function.
* Standardized internal variable naming to R-idiomatic conventions (e.g., `n_` prefix for counts, `df_free` for degrees of freedom).
* Fixed spelling mistakes and grammar in documentation.
* Added a `devbox`-based reproducible development environment (`devbox.json`) and a `justfile` with shortcuts for common development tasks (`just test`, `just check`, `just document`, etc.).

# badp 0.4.0

* Renamed package from `bdsm` to `badp` (Bayesian Averaging for Dynamic Panels).
* Removed the `df` argument from the `bma` function; data is no longer required at the BMA stage.
* Added `posterior_dens` function for plotting posterior densities of coefficients.
* Added weighted coefficient histograms in `coef_hist` via the `weight` parameter (based on posterior model probabilities).
* Exported `extract_names` function.
* Recomputed bundled datasets to be consistent with updated `optim_model_space`.

# bdsm 0.3.0

* Reimplemented SEM likelihood computation in C++.

# bdsm 0.2.2

* Modified the method for selecting beta coefficient rows in the `bma` function for improved robustness and compatibility.
* Updated tests to align with changes in the upcoming ggplot2 release (v4.0.0), ensuring compatibility and future-proofing the package.

# bdsm 0.2.1

* Added a vignette explaining Bayesian model averaging for dynamic panels with weakly exogenous regressors

# bdsm 0.2.0

* Added GitHub Actions Workflows:
    * .github/workflows/R-CMD-check-develop.yaml: A workflow for R CMD checks on the develop branch.
    * .github/workflows/R-CMD-check-main.yaml: A workflow for R CMD checks across multiple operating systems and R versions on the main branch.
* Updated .Rbuildignore:
    * Ignored the .github directory.
* Updated .gitignore:
    * Added rules to ignore R-specific temporary files, build outputs, and vignettes.
* Updated DESCRIPTION:
    * Added rmarkdown and pbapply to Suggested and Imports, respectively.
    * Updated the dependency on R to version >= 3.5.
* Updated NAMESPACE:
    * Adjusted function exports to follow naming conventions (e.g., SEM_* functions renamed to sem_*).
* Re-factored R Functions:
    * Renamed SEM_* functions to sem_* in multiple files for consistency.
* Removed R/SEM_bma.R:
    * The file R/SEM_bma.R was deleted, indicating major re-factoring or deprecation of related functionality.
* Added progress bar for computationally intensive functions
* Changed naming convention and broadened the meaning of a model space.
Now it is a list containing two named elements:
parameters (params) of all considered models
and statistics (stats) computed using these parameters. 
This is a much more comprehensible naming convention than the previous one, where only the parameters were considered as the model space. 
Along with that change, some re-factoring and modifications were introduced:
    * all functions relating to the model space are now stored in R/model_space.R
    * initialize_model_space was renamed to init_model_space_params
    * likelihoods_summary was renamed to compute_model_space_stats
    * optimal_model_space was renamed to optim_model_space_params
    * a wrapper function optim_model_space, which returns the entire model space (both parameters and statistics), was introduced
    * data objects released with the package were re-factored, recomputed, and renamed. Two example model spaces computed with the new optim_model_space function are provided: small_model_space and full_model_space.
* Simplified the framework for data preparation. 
A single function feature_standardization is provided, which allows flexible and simple options for data preparation. 
See the vignette and function manual for more details. 

# bdsm 0.1.0

* Initial CRAN submission.
