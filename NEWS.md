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
