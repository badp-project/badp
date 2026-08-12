# badp 0.6.0

* `optim_model_space()` now warns when a model has at least as many
  parameters as there are entities. The robust ("sandwich") standard errors
  are built from `J = sum_i s_i s_i'`, the outer product of the `N`
  entity-level score vectors, so `J` has rank at most `N` and is singular in
  that case. The `PSDR` and `PSDRcon` columns of `bma()` are then not
  identified: some linear combinations of the parameters are assigned zero
  estimated variance and the remaining entries depend appreciably on how the
  derivatives are obtained. This is the too-few-clusters problem of
  cluster-robust inference, with entities in the role of clusters, and it is
  a property of the design rather than of the estimation. The likelihood,
  the posterior means, the posterior inclusion probabilities and the
  Hessian-based standard errors `PSD` and `PSDcon` are unaffected.

  This applies to the bundled `full_model_space`, in which all 512 models
  have between 88 and 106 parameters against 73 entities, and hence to the
  robust standard deviations reported for the economic growth data
  throughout the literature that this package reproduces. Posterior means
  and inclusion probabilities for that dataset continue to match the
  published values to 0.002 and 0.4 percentage points respectively.
* `bma(weighting = "curvature")` warns for the same reason, since the
  adjusted learning rate `dim(theta) / tr(H^-1 J)` uses the same `J`. An
  estimate above one is a symptom of the deficiency rather than a meaningful
  value.
* `bma()` gained a `weighting` argument selecting the approximation to the
  marginal likelihood used to weight the models. Writing
  `A_j = loglik_j - (k_j/2)*log(N*T)`, four of the five options are the same
  construction with different learning rates `eta`, `log w_j = eta * A_j`:
    * `"mb2016"` (default, `eta = 1/N`) is unchanged behaviour, the
      approximation computed by the implementation accompanying Moral-Benito
      (2016), and the option that reproduces the posterior inclusion
      probabilities and posterior moments published there;
    * `"mb2012"` (`eta = 1`) is the Schwarz criterion exactly as stated in
      equations (24)-(30) of Moral-Benito (2012);
    * `"nt"` (`eta = 1/(N*T)`) averages over entity-periods rather than
      entities, the scaling that would be internally consistent with the
      `log(N*T)` penalty;
    * `"curvature"` estimates `eta` from the data by the magnitude
      ("omnibus") adjustment used for misspecified and composite likelihoods
      (Chandler and Bate 2007; Ribatet, Cooley and Davison 2012), rather than
      fixing it a priori. The rate is `dim(theta) / tr(H^-1 J)`, which equals
      one exactly when the information matrix equality holds.
* The model space statistics gained two rows, holding `tr(H^-1 J)` and the
  dimension of the parameter vector for each model. Both matrices are already
  formed by the automatic differentiation used to compute the standard errors,
  so the curvature adjustment above is evaluated exactly rather than
  approximated. The new rows are appended after the existing ones, so code
  indexing the likelihood, weight or standard-error rows is unaffected. Model
  spaces fitted with earlier versions do not carry them; `bma()` then falls
  back to an approximation based on the ratio of robust to Hessian-based
  standard errors and emits a message.

  The fifth option, `"uip"`, instead alters the penalty, using
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

* `init_value` accepts either a generator function of one argument `n`
  returning `n` starting values, or a single number used as the starting
  value for every parameter; `init_value = 0.5` is equivalent to
  `function(n) rep(0.5, n)`. Support for a single number was inadvertently
  lost during development of this release and has been restored, so code
  written against 0.4.0 and 0.5.0 continues to work. Passing `0`, a vector of
  length greater than one, or anything that is neither a function nor a
  single finite number now fails with an informative error instead of an
  error about an unrelated internal call. Zero is rejected because it is
  reserved to mark a parameter excluded from a given model.
* `optim_model_space()` gained `max_init_attempts` and `max_reoptimizations`.
  A starting point that falls in a region where the likelihood is undefined
  is now redrawn from `init_value` (up to `max_init_attempts` times), and a
  model whose observed information matrix is not positive definite is
  re-optimized from a fresh starting point (up to `max_reoptimizations`
  times) instead of being reported as a failure.
* The `convergence` element of `badp_model_space` objects gained an
  `n_init_draws` row, recording how many starting points were drawn before
  one at which the likelihood is defined was found.
* Automatic differentiation errors encountered during optimization no longer
  abort the procedure; the line search corrects the step size instead, which
  is the intended behaviour.
* `optim()`'s own convergence flag is now honored when `max_restarts = 0`.
* Non-finite observed-information matrices are rejected in
  `usable_solution()` rather than propagating `NaN` standard errors.
* Fixed `sem_sigma_matrix()` to use multiplication instead of `^2`, avoiding
  an automatic-differentiation artifact at zero.
* Fixed `sem_C_matrix()` to validate the length of `phi_1` against `beta`.
* Fixed `init_value()`-drawn zeros being treated as excluded parameters.
* `RTMB (>= 1.6)` is now required, for automatic-differentiation support in
  `chol()` and `determinant()`.
* The minimum required R version was raised from 3.5 to 4.4, to match the
  requirement of `Matrix`, on which `RTMB` depends through `TMB`. The
  previous declaration could not be satisfied in practice.
* Regenerated the bundled `small_model_space`, `migration_model_space` and
  `migration_model_space_nonnested` objects with the fixed automatic
  differentiation pipeline.

# badp 0.5.0

* Replaced the C++ (Rcpp/RcppArmadillo) SEM likelihood implementation with an
  R implementation differentiated via `RTMB` (automatic differentiation).
  Optimization now uses exact gradients instead of finite differences, and
  standard errors are computed from the exact Hessian and per-entity score
  vectors instead of finite-difference approximations. Optimized parameters
  and standard errors may therefore differ slightly (and are more accurate);
  likelihood values at given parameters are unchanged. The finite-difference
  `hessian()` function was removed. The `Rcpp`, `RcppArmadillo`, `rootSolve`
  and `optimbase` dependencies were dropped in favour of `RTMB`.
* `init_value` (in `optim_model_space()` and related functions) now also
  accepts a generator function of one argument `n` returning `n` starting
  values (e.g. `function(n) runif(n, 0.1, 1)`), enabling randomized
  multi-start experiments. Passing a single number behaves as before.
* Per-model optimization is now restarted until the log-likelihood value
  stops improving (`max_restarts` and `restart_tol` arguments of
  `optim_model_space()`), and per-model convergence diagnostics (converged
  flag, `optim` code, number of restarts, final gradient norm) are stored in
  the new `convergence` element of `badp_model_space` objects. Non-converged
  models trigger a warning in `optim_model_space()` and `bma()` and are
  reported by `summary()`/`print()`; they are deliberately not excluded from
  the analysis.
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
