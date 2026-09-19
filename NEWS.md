# badp (development version)

## Extracting results from a model space

* New accessor `model_table()` returns one row per model of a
  `badp_model_space`: the included regressors, their number, the maximized
  log-likelihood and the convergence flag. `sort_by` orders the models by
  log-likelihood or size and `top` keeps the first rows, so that ranking the
  estimated models no longer requires indexing the parameter and statistics
  matrices.
* New `logLik()` methods for `badp_model_space` (one model, selected by its
  position) and for the individual models returned by `best_models()`. They
  return standard `"logLik"` objects with `df` and `nobs` attributes, so that
  `AIC()` and `BIC()` apply. The value is the exact log-likelihood at the
  estimates.
* `bma()` results gained three components, `loglik`, `n_params` and `nobs`,
  appended after the existing ones so that no name or position changes.
  `best_models()` passes them to each model, together with the model's
  position in the model space (`model`), and `print()` on a single model now
  reports its log-likelihood.

# badp 0.7.0

This release reworks the user-facing interface in response to the review of the
manuscript submitted to the Journal of Statistical Software. Several changes are
breaking; they are marked below.

## The best models are now objects

* **Breaking change**: `best_models()` returns a list of individual models
  rather than nine tables already formatted for display. Each element is an object of the new
  class `badp_model` carrying that model's regressors, posterior model
  probability, coefficients, standard errors, robust standard errors and
  p-values as numbers, where these were previously pasted into display strings
  and discarded. Individual models are reached by position, and each has
  `print()`, `summary()` and `coef()` methods.
* **Breaking change**: the `estimate` and `robust` arguments of `best_models()`
  were removed. They selected which table was drawn on auto-print; the choice
  now belongs to the `print()`, `summary()` and `plot()` methods, which take a
  `robust` argument where it applies.
* `coef()` on a `badp_model` gained a `digits` argument. The values are
  returned as estimated by default; `digits` rounds them, which is a
  convenience when the coefficient table is being displayed rather than
  computed with.
* `badp_best_models` objects gained `print()`, `summary()`, `plot()` and `[`
  methods. `print()` writes an inclusion table to the console, where the
  previous method drew to the graphics device and produced no console output at
  all. `summary()` returns its own classed object with the formatted estimates,
  and `plot()` draws either table.

## Inference in the best-models tables

* **Breaking change**: the p-values reported for individual models are now Wald
  p-values referred to the standard normal distribution, where they were
  previously referred to a t distribution with `NT - k` degrees of freedom. The
  estimator is maximum likelihood with standard errors from the observed
  information, so no exact small-sample distribution applies and there are no
  residual degrees of freedom to count. The likelihood factorizes over entities
  rather than over entity-periods, so `NT` counted observations the likelihood
  never treats as independent; `N - k` is not an alternative, being negative for
  the model spaces shipped with the package, in which the parameter vector is
  longer than the number of entities. The column is now labeled `Pr(>|z|)`.
  The practical effect is small: the two-sided five percent critical value moves
  from 1.968 to 1.960.
* **Breaking change**: the `df_free` element was removed from `badp_bma`
  objects, which now hold 18 elements rather than 19. It recorded the degrees of
  freedom described above and is no longer used anywhere in the package.
  Elements after it shift down one position; access components by name.

## Printing and plotting

* `model_sizes()` and `model_pmp()` gained a `type` argument. `"line"` keeps
  the existing display; `"histogram"` draws the prior and the posterior as
  side-by-side bars, as in the companion package `rmsBMA`. Bars read well
  when few models or model sizes are shown -- a small `top`, or a handful of
  regressors -- while the lines stay legible when there are many. The
  argument passes through `plot()`, so
  `plot(results, which = "model_pmp", top = 5, type = "histogram")` works.
* Both functions now build their six graphs from a single internal builder
  rather than repeating the same `ggplot` specification once per prior and
  once per dilution setting, so the line and bar forms cannot drift apart.

* `print()` on a `badp_bma` object no longer delegates to `summary()`. It now
  gives a compact overview: the size of the model space, the prior and weighting
  settings, and the regressors ranked by posterior inclusion probability.
  `summary()` continues to return a classed object and to print the full tables
  under both model priors, so it now says strictly more than `print()`.
* `model_sizes()`, `model_pmp()`, `coef_hist()` and `posterior_dens()` return
  objects of the new class `badp_plots` with named elements, and no longer print
  a plot as a side effect. A bare list of plots is auto-printed one element at a
  time, so on an interactive device each plot replaced the one before it; the
  new `print()` method draws a single figure instead, the combined plot where
  there is one and otherwise all of them arranged together. Individual plots are
  still reached by name or position.
* `plot()` on a `badp_bma` object now draws exactly one graphic for every value
  of `which` and returns invisibly, rather than relying on auto-printing
  whatever the underlying helper returned.

## Accessors

* Added `bma_table()`, `pip()`, `pmp()`, `model_size_table()`, `regressors()`,
  `n_models()`, `weighting()`, `learning_rate()` and `convergence()`. Every
  quantity previously reached by indexing into the object, by name or by
  position, now has a documented accessor. They are generics with methods, so
  they are listed by `methods(class = "badp_bma")` and
  `methods(class = "badp_model_space")`.

## Argument naming

* **Breaking change**: the first argument of `best_models()`, `jointness()`,
  `model_pmp()`, `model_sizes()`, `coef_hist()` and `posterior_dens()` was
  renamed from `bma_list` to `x`. The object these functions consume is of
  class `badp_bma`, not a list, and the name contradicted both the accessors
  (which all take `x`) and the argument of this release. Calls that pass the
  object as the first, unnamed argument, which is the documented usage
  everywhere, are unaffected; calls written as `bma_list = ` must be updated.

## Removed

* `print.badp_drawable_grob()` and the internal `as_drawable_grob()` helper
  were removed. Nothing in the package created an object of class
  `badp_drawable_grob`, so the method was registered for a class that never
  existed at run time. They were left over from the previous implementation of
  `best_models()`, which returned grobs.

## Bug fixes

* `plot()` on a `badp_bma` object with `which = "best_models"` handed the whole
  of `...` to `best_models()`, which selects the models, although the drawing
  is done afterwards by `plot()` on the result. A plotting argument therefore
  reached a function that does not have it, and
  `plot(x, which = "best_models", robust = TRUE)` failed before anything was
  drawn. The arguments are now split between the two by name.

* `optim_model_space_params()` required `nested` to be supplied explicitly,
  although `optim_model_space()` and `compute_model_space_stats()` both
  default it to `TRUE`. It now defaults to `TRUE` as well, so the three
  functions agree and the documented call works as written.

* `model_pmp()` reset `top` to the number of *regressors* when the requested
  number exceeded the size of the model space, so asking for more models than
  exist silently plotted too few of them. It is now capped at the size of the
  model space, as `pmp()` and `best_models()` already were. The message it
  prints was reworded to match those two, and no longer claims that `top` is a
  count of regressors.

## Internals

* The package no longer addresses its own objects by position. Every internal
  read of a `badp_bma` or `badp_model_space` object now uses the component
  name: `bma_list$R` rather than `bma_list[[4]]`, `model_space$params` rather
  than `model_space[[1]]`, and so on across `bma()`, `best_models()`,
  `jointness()`, `model_sizes()`, `model_pmp()`, `coef_hist()` and
  `posterior_dens()` (36 call sites). Removing `df_free` from position 15 in
  this release was safe only because nothing happened to index past it; that
  will no longer be a matter of luck.
* The test suite no longer asserts that positional access works. It pins the
  names of the components returned by `bma()` instead, which is what callers
  and the accessors actually depend on.
* The plot collections returned by `coef_hist()` and `posterior_dens()` are
  named after the variables they show, so the vignette reaches an individual
  plot with `coef_plots[["gdp_lag"]]` rather than `coef_plots[[1]]`.

## Documentation and dependencies

* The package vignette now demonstrates every accessor rather than only
  `bma_table()` and `model_size_table()`, and no longer refers to components of
  the fitted objects by position ("the first element of the list", "19
  elements"). It also gained a short subsection on `convergence()`, which was
  previously undocumented outside the help pages.
* The README was rewritten against the current interface. Its examples still
  used the interface from before 0.7.0: they indexed the fitted object by position
  (`bma_results[[16]]`, which after the removal of `df_free` no longer points
  at the model-size table), and called `top3_binom[[6]]`, which does not exist
  now that `best_models()` returns one object per model. The README could not
  have been re-knitted. It now uses the accessors and the methods.
* All help page titles were rewritten in title style.
* Every public help page now carries a working example. Thirteen had none,
  almost all of them the methods introduced in this release.
* `join_lagged_col()` is no longer marked `@keywords internal` and now appears
  in the package index. It was already exported, already listed in the pkgdown
  reference under data preparation, and is demonstrated in both vignettes, so
  hiding it from `help(package = "badp")` was inconsistent. Its help page now
  also says when the function is needed: only for data that store the dependent
  variable twice, in levels and lagged, which is a property of the source data
  and not a step every analysis takes.
* `knitr` moved from `Imports` to `Suggests`. The package no longer calls
  it: the `knitr::kable()` renderings were one of the three redundant
  display formats that `best_models()` used to return. It is still
  required to build the vignette.

# badp 0.6.1

* The license files now attribute the copyright to "badp authors" over the
  range 2021-2026.

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
  and `optimbase` dependencies were dropped in favor of `RTMB`.
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
  they sum to zero at the maximum, `J` is unchanged by centering them and
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
    * `"mb2016"` (default, `eta = 1/N`) is unchanged behavior, the
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
  that the likelihood factorizes over entities.

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
* The selected weighting and the realized learning rate are recorded in the
  new `weighting` and `eta` elements (slots 18 and 19) of `badp_bma` objects.
  Existing slots 1-17 are unchanged, so numeric indexing of earlier elements
  continues to work.
* Model weights are now formed on the log scale and shifted before
  exponentiation, which prevents overflow when log Bayes factors are large.
* `coef_hist()` labels the y axis "Density" rather than "Frequency", matching
  what the histograms actually show.
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
