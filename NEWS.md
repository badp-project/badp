# badp 0.4.1

* Added S3 classes and methods for JSS compliance:
    * `bma()` now returns an object of class `badp_bma` (previously unclassed list).
    * `optim_model_space()` now returns an object of class `badp_model_space`.
    * Implemented S3 methods for `badp_bma` objects:
        * `print.badp_bma()` - Clean, informative console output.
        * `summary.badp_bma()` - Detailed statistical summary with highlighted important variables.
        * `coef.badp_bma()` - Extract coefficients with optional standard errors and PIPs.
        * `plot.badp_bma()` - Default visualization with dispatch to existing plot functions.
    * Implemented `print.badp_model_space()` for model space objects.
    * Fixed component names in `bma()` output: removed spaces, duplicates, and typos; all names are now valid R identifiers (e.g., `uniform_table`, `random_table`, `reg_names`, `dilution`, `alphas`).
    * **Backward compatibility**: All existing code continues to work - numeric indexing (`results[[3]]`) and helper functions (`best_models()`, `jointness()`, etc.) are fully preserved. Named access is now also available (`results$reg_names`).
    * Added comprehensive tests for S3 methods and backward compatibility (125 new tests).
* Replaced `sem_likelihood` example: use the bundled `economic_growth` dataset instead of small random data that could produce `NA` or invalid positive values on some platforms.
* Removed `ggpubr` dependency; plotting functions now use custom arrangement with `gridExtra`.

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

* Added vignette explaining Bayesian model averaging for dynamic panels with weakly exogenous regressors

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
Now it's a list containing two named elements:
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
