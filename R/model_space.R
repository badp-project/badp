#' Initialize model space matrix
#'
#' This function builds a representation of the model space, by creating a
#' dataframe where each column represents values of the parameters for a given
#' model. Real value means that the parameter is included in the model. A
#' parameter not present in the model is marked as \code{NA}.
#'
#' Currently the set of features is assumed to be all columns which remain after
#' excluding \code{timestamp_col}, \code{entity_col} and \code{dep_var_col}.
#'
#' A power set of all possible exclusions of linear dependence on the given
#' feature is created, i.e. if there are 4 features we end up with 2^4 possible
#' models (for each model we independently decide whether to include or not a
#' feature).
#'
#' @param df Data frame with data for the SEM analysis.
#' @param timestamp_col Column which determines time periods. For now only
#' natural numbers can be used as timestamps
#' @param entity_col Column which determines entities (e.g. countries, people)
#' @param dep_var_col Column with dependent variable
#' @param init_value Initial value for parameters present in the model. Default
#' is \code{1}.
#'
#' @return matrix of model parameters
#'
#' @examples
#' library(magrittr)
#'
#' data_prepared <- bdsm::economic_growth[, 1:5] %>%
#'   bdsm::feature_standardization(
#'     excluded_cols = c(country, year, gdp)
#'   ) %>%
#'   bdsm::feature_standardization(
#'     group_by_col  = year,
#'     excluded_cols = country,
#'     scale         = FALSE
#'   )
#'
#' init_model_space_params(data_prepared, year, country, gdp)
#' @export
init_model_space_params <- function(df, timestamp_col, entity_col,
                                    dep_var_col, init_value = 1) {
  regressors <- df %>%
    regressor_names(timestamp_col = {{ timestamp_col }},
                    entity_col = {{ entity_col }},
                    dep_var_col = {{ dep_var_col }})
  regressors_n <- length(regressors)

  counts <- df %>% dplyr::select({{ timestamp_col }}, {{ entity_col }}) %>%
    sapply(function(x) dplyr::n_distinct(x))

  timestamps_n <- counts[[1]] - 1
  entities_n <- counts[[2]]

  regressors_subsets_matrix <-
    (rje::powerSetMat(regressors_n) * init_value)

  linear_params_matrix <-
    t(cbind(regressors_subsets_matrix, regressors_subsets_matrix))

  rownames(linear_params_matrix) <-
    c(paste('beta', regressors, sep="_"), paste('phi_1', regressors, sep="_"))

  dep_var_matrix <-
    t(matrix(init_value, nrow = 2^regressors_n, ncol = 3 + timestamps_n))
  rownames(dep_var_matrix) <- c(c('alpha', 'phi_0', 'err_var'),
                                paste('dep_var', 1:timestamps_n, sep = '_'))

  phis_n <- regressors_n * (timestamps_n - 1)
  psis_n <- regressors_n * (timestamps_n - 1) * timestamps_n / 2

  psis_phis_matrix <-
    matrix(init_value, nrow = phis_n + psis_n, ncol = 2^regressors_n)

  . <- NULL
  rbind(dep_var_matrix, linear_params_matrix, psis_phis_matrix) %>%
    replace(. == 0, NA)
}


#' Helper function to extract names from a vector defining a model
#'
#' For now it is assumed that we can only exclude linear relationships between
#' regressors and the dependent variable.
#'
#' The vector needs to have named rows, i.e. it is assumed it comes from a
#' model space (see \link[bdsm]{init_model_space_params} for details).
#'
#' @param params a vector with parameters describing the model
#'
#' @return
#' Names of regressors which are assumed to be linearly connected with dependent
#' variable within the model described by the \code{params} vector.
#'
#' @examples
#' params <- c(alpha = 1, beta_gdp = 1, beta_gdp_lagged = 1, phi_0 = 1, err_var = 1)
#' regressor_names_from_params_vector(params)
#'
#' @export
regressor_names_from_params_vector <- function(params) {
  regressors_subset <-
    t(params %>% stats::na.omit()) %>% as.data.frame() %>%
    dplyr::select(tidyselect::matches('beta'))

  names(regressors_subset) <- gsub("beta_", "", names(regressors_subset))
  names(regressors_subset)
}

#' Helper-function - finds parameters minimizing log-likelihood function
#' for the nested version of the SEM setup, using BFGS method
#'
#' @param params Vector of the initial parameters
#' @param df Data frame with data for the SEM analysis.
#' @param timestamp_col Column which determines time periods. For now only
#' natural numbers can be used as timestamps
#' @param entity_col Column which determines entities (e.g. countries, people)
#' @param dep_var_col Column with dependent variable
#' @param data List of SEM setup matrices shared along the models
#' @param exact_value Whether the exact value of the likelihood should be
#' computed (\code{TRUE}) or just the proportional part (\code{FALSE}). Check
#' \link[bdsm]{sem_likelihood} for details.
#' @param control a list of control parameters for the optimization which are
#' passed to \link[stats]{optim}. Default is
#' \code{list(trace = 2, maxit = 10000, fnscale = -1, REPORT = 100, scale = 0.05)}.
#'
#' @returns List (or matrix) of parameters describing analyzed models.
#' @export
#'
nested_optimization_wrapper <- function(
    params,
    df,
    timestamp_col,
    entity_col,
    dep_var_col,
    data,
    exact_value,
    control
    ) {
  regressors_subset <- regressor_names_from_params_vector(params)

  model_specific_matrices <- df %>%
    matrices_from_df(timestamp_col = {{ timestamp_col }},
                     entity_col = {{ entity_col }},
                     dep_var_col = {{ dep_var_col }},
                     lin_related_regressors = regressors_subset,
                     which_matrices = c("cur_Y2", "cur_Z"))

  data$cur_Z <- model_specific_matrices$cur_Z
  data$cur_Y2 <- model_specific_matrices$cur_Y2

  params_no_na <- stats::na.omit(params)

  # Adjust the optimization control parameters.
  control$parscale <- control$scale * params_no_na
  control$scale <- NULL

  optimized <- stats::optim(params_no_na, sem_likelihood, data = data,
                            exact_value = exact_value,
                            method = "BFGS",
                            control = control)

  params[!is.na(params)] <- optimized[[1]]
  params
}


#' Helper-function - finds parameters minimizing log-likelihood function
#' for the non-nested version of the SEM setup, using BFGS method
#'
#' @param params Vector of the initial parameters
#' @param df Data frame with data for the SEM analysis.
#' @param timestamp_col Column which determines time periods. For now only
#' natural numbers can be used as timestamps
#' @param entity_col Column which determines entities (e.g. countries, people)
#' @param dep_var_col Column with dependent variable
#' @param exact_value Whether the exact value of the likelihood should be
#' computed (\code{TRUE}) or just the proportional part (\code{FALSE}). Check
#' \link[bdsm]{sem_likelihood} for details.
#' @param control a list of control parameters for the optimization which are
#' passed to \link[stats]{optim}. Default is
#' \code{list(trace = 2, maxit = 10000, fnscale = -1, REPORT = 100, scale = 0.05)}.
#'
#' @returns List (or matrix) of parameters describing analyzed models.
#' @export
#'
non_nested_optimization_wrapper <- function(
    params,
    df,
    timestamp_col,
    entity_col,
    dep_var_col,
    exact_value,
    n_all_regressors,
    n_timestamp,
    control
  ) {
  # derive the set of all matrices needed, based on reduced df
  regressors_subset <- regressor_names_from_params_vector(params)
  # reduced data-frame: it will not work if regressors_subset empty

  df_loc <- df %>% dplyr::select(
    {{ timestamp_col }},
    {{ entity_col }},
    {{ dep_var_col }},
    dplyr::all_of(regressors_subset)
  )

  data <- df_loc %>%
    matrices_from_df(
      timestamp_col = {{ timestamp_col }},
      entity_col = {{ entity_col }},
      dep_var_col = {{ dep_var_col }},
      lin_related_regressors = regressors_subset,
      which_matrices = c(
        "Y1", "Y2", "Z", "res_maker_matrix", "cur_Y2", "cur_Z"))

  # set last n params to NA, for n - difference model params
  # for full matrix & model params for current setup
  curr_n_regressors <- length(regressors_subset)
  curr_n_phi <- curr_n_regressors * (n_timestamp - 1)
  curr_n_psi <- curr_n_regressors * (n_timestamp - 1) * n_timestamp / 2

  full_n_phi <- n_all_regressors * (n_timestamp - 1)
  full_n_psi <- n_all_regressors * (n_timestamp - 1) * n_timestamp / 2

  num_new_na <- (full_n_phi + full_n_psi) - (curr_n_phi + curr_n_psi)

  if (num_new_na != 0) {
    params[(length(params) - num_new_na + 1):length(params)] <- NA_real_
  }


  params_no_na <- stats::na.omit(params)

  # Adjust the optimization control parameters.
  control$parscale <- control$scale * params_no_na
  control$scale <- NULL

  optimized <- stats::optim(params_no_na, sem_likelihood, data = data,
                            exact_value = exact_value,
                            method = "BFGS",
                            control = control)

  params[!is.na(params)] <- optimized[[1]]
  params
}



#' Finds MLE parameters for each model in the given model space
#'
#' Given a dataset and an initial value for parameters, initializes a model
#' space with parameters equal to the initial value for each model. Then for each
#' model performs a numerical optimization and finds parameters which maximize
#' the likelihood.
#'
#' @param df Data frame with data for the analysis.
#' @param timestamp_col The name of the column with time stamps.
#' @param entity_col Column with entities (e.g. countries).
#' @param dep_var_col Column with the dependent variable.
#' @param init_value The value with which the model space will be initialized.
#' This will be the starting point for the numerical optimization.
#' @param exact_value Whether the exact value of the likelihood should be
#' computed (\code{TRUE}) or just the proportional part (\code{FALSE}). Check
#' \link[bdsm]{sem_likelihood} for details.
#' @param cl An optional cluster object. If supplied, the function will use this
#' cluster for parallel processing. If \code{NULL} (the default),
#' \code{pbapply::pblapply} will run sequentially.
#' @param control a list of control parameters for the optimization which are
#' passed to \link[stats]{optim}. Default is
#' \code{list(trace = 2, maxit = 10000, fnscale = -1, REPORT = 100, scale = 0.05)}.
#'
#' @return
#' List (or matrix) of parameters describing analyzed models.
#'
#' @importFrom pbapply pbapply
#'
#' @export
optim_model_space_params <- function(
  df,
  timestamp_col,
  entity_col,
  dep_var_col,
  init_value,
  nested,
  exact_value = FALSE, cl = NULL,
  control = list(trace = 0, maxit = 10000, fnscale = -1, REPORT = 100, scale = 0.05)
  ) {

  all_regressors <- df %>%
    dplyr::select(
      -c(
        {{timestamp_col}},
        {{entity_col}},
        {{dep_var_col}},
      )
    )

  init_params <- df %>%
    init_model_space_params(timestamp_col = {{ timestamp_col }},
                            entity_col = {{ entity_col }},
                            dep_var_col = {{ dep_var_col }},
                            init_value = init_value)

  if (nested) {
    matrices_shared_across_models <- df %>%
      matrices_from_df(timestamp_col = {{ timestamp_col }},
                       entity_col = {{ entity_col }},
                       dep_var_col = {{ dep_var_col }},
                       which_matrices = c("Y1", "Y2", "Z", "res_maker_matrix"))

    # optimization performed for nested version
    return(
      pbapply::pbapply(init_params, MARGIN = 2,  function(x) {
        nested_optimization_wrapper(
          x,
          df = df,
          timestamp_col = {{ timestamp_col }},
          entity_col = {{ entity_col }},
          dep_var_col = {{ dep_var_col }},
          data = matrices_shared_across_models,
          exact_value = exact_value,
          control = control
          )
      }, cl = cl)
    )
  } else {
    # optimization performed for non-nested version
    n_all_regressors = ncol(all_regressors)
    n_timestamp = df %>%
      dplyr::distinct({{timestamp_col}}) %>%
      nrow() - 1


    return(
      pbapply::pbapply(init_params, MARGIN = 2,  function(x) {
        non_nested_optimization_wrapper(
          x,
          df = df,
          timestamp_col = {{ timestamp_col }},
          entity_col = {{ entity_col }},
          dep_var_col = {{ dep_var_col }},
          exact_value = exact_value,
          n_all_regressors = n_all_regressors,
          n_timestamp = n_timestamp,
          control = control
          )
      }, cl = cl)
    )
  }
}


#' Approximate standard deviations for the models
#'
#' Approximate standard deviations are computed for the models in the given
#' model space. Two versions are computed.
#'
#' @param df Data frame with data for the SEM analysis.
#' @param dep_var_col Column with the dependent variable
#' @param timestamp_col The name of the column with timestamps
#' @param entity_col Column with entities (e.g. countries)
#' @param params A matrix (with named rows) with each column corresponding
#' to a model. Each column specifies model parameters. Compare with
#' \link[bdsm]{optim_model_space_params}
#' @param model_prior Which model prior to use. For now there are two options:
#' \code{'uniform'} and \code{'binomial-beta'}. Default is \code{'uniform'}.
#' @param exact_value Whether the exact value of the likelihood should be
#' computed (\code{TRUE}) or just the proportional part (\code{FALSE}). Check
#' \link[bdsm]{sem_likelihood} for details.
#' @param cl An optional cluster object. If supplied, the function will use this
#' cluster for parallel processing. If \code{NULL} (the default),
#' \code{pbapply::pblapply} will run sequentially.
#'
#' @return
#' Matrix with columns describing likelihood and standard deviations for each
#' model. The first row is the likelihood for the model (computed using the
#' parameters in the provided model space). The second row is almost 1/2 * BIC_k
#' as in Raftery's Bayesian Model Selection in Social Research eq. 19 (see TODO
#' in the code below). The third row is model posterior probability. Then there
#' are rows with standard deviations for each parameter. After that we have rows
#' with robust standard deviation (not sure yet what exactly "robust" means).
#'
#' @importFrom pbapply pbapply
#' @export
#'
#' @examples
#' \donttest{
#'  library(magrittr)
#'  data_prepared <- bdsm::economic_growth[, 1:6] %>%
#'    bdsm::feature_standardization(
#'      excluded_cols = c(country, year, gdp)
#'    ) %>%
#'    bdsm::feature_standardization(
#'      group_by_col  = year,
#'      excluded_cols = country,
#'      scale         = FALSE
#'    )
#'
#'  compute_model_space_stats(
#'    df            = data_prepared,
#'    dep_var_col   = gdp,
#'    timestamp_col = year,
#'    entity_col    = country,
#'    params        = small_model_space$params
#'  )
#' }
#'
compute_model_space_stats <- function(df, dep_var_col, timestamp_col, entity_col,
                              params, nested = TRUE, exact_value = FALSE,
                              model_prior = 'uniform', cl = NULL) {
  regressors <- df %>%
    regressor_names(timestamp_col = {{ timestamp_col }},
                    entity_col = {{ entity_col }},
                    dep_var_col = {{ dep_var_col }})
  regressors_n <- length(regressors)
  variables_n <- regressors_n + 1

  matrices_shared_across_models <- df %>%
    matrices_from_df(timestamp_col = {{ timestamp_col }},
                     entity_col = {{ entity_col }},
                     dep_var_col = {{ dep_var_col }},
                     which_matrices = c("Y1", "Y2", "Z", "res_maker_matrix"))

  n_entities <- nrow(matrices_shared_across_models$Z)
  periods_n <- nrow(df) / n_entities - 1

  prior_exp_model_size <- regressors_n / 2
  prior_inc_prob <- prior_exp_model_size / regressors_n

  # THIS WILL BE DELETED
  #print(paste("Prior Mean Model Size:", prior_exp_model_size))
  #print(paste("Prior Inclusion Probability:", prior_inc_prob))

  # parameter for beta (random) distribution of the prior inclusion probability
  b <- (regressors_n - prior_exp_model_size) / prior_exp_model_size

  if (nested) {
    std_dev_from_params <- function(params, data) {
      regressors_subset <-
        regressor_names_from_params_vector(params)

      lin_features_n <- length(regressors_subset) + 1
      features_n <- ncol(data$Z)

      model_specific_matrices <- df %>%
        matrices_from_df(timestamp_col = {{ timestamp_col }},
                         entity_col = {{ entity_col }},
                         dep_var_col = {{ dep_var_col }},
                         lin_related_regressors = regressors_subset,
                         which_matrices = c("cur_Y2", "cur_Z"))

      data$cur_Z <- model_specific_matrices$cur_Z
      data$cur_Y2 <- model_specific_matrices$cur_Y2

      params_no_na <- params %>% stats::na.omit()

      likelihood <-
        sem_likelihood(params = params_no_na, data = data,
                       exact_value = TRUE)

      hess <- hessian(sem_likelihood, theta = params_no_na, data = data)

      likelihood_per_entity <-
        sem_likelihood(params_no_na, data = data, per_entity = TRUE)

      # TODO: how to interpret the Gmat and Imat
      Gmat <- rootSolve::gradient(sem_likelihood, params_no_na, data = data,
                                  per_entity = TRUE)
      Imat <- crossprod(Gmat)

      # Section 2.3.3 in Moral-Benito
      # GROWTH EMPIRICS IN PANEL DATA UNDER MODEL UNCERTAINTY AND WEAK EXOGENEITY:
      # "Finally, each model-specific posterior is given by a normal distribution
      # with mean at the MLE and dispersion matrix equal to the inverse of the
      # Fisher information."
      # This is most likely why hessian is used to compute standard errors.
      # TODO: Learn the Bernstein–von Mises theorem which explain in detail how
      # all this works
      stdr <- rep(0, features_n)
      stdh <- rep(0, features_n)

      . <- NULL
      linear_params <- t(params) %>% as.data.frame() %>%
        dplyr::select(tidyselect::matches('alpha'),
                      tidyselect::matches('beta')) %>%
        as.matrix() %>% t()

      betas_first_ind <- 4 + periods_n
      betas_last_ind <- betas_first_ind + lin_features_n - 2
      inds <- if (betas_first_ind > betas_last_ind) {
        c(1)
      } else {
        c(1, betas_first_ind:betas_last_ind)
      }

      stdr[!is.na(linear_params)] <- sqrt(diag(solve(hess) %*% Imat %*% solve(hess)))[inds]
      stdh[!is.na(linear_params)] <- sqrt(diag(solve(hess)))[inds]

      # Below we have almost 1/2 * BIC_k as in Raftery's Bayesian Model Selection
      # in Social Research eq. 19. The part with reference model M_1 is skipped,
      # because we use this formula to compute exp(logl) which is in turn used to
      # compute posterior probabilities using eqs. 34/35. Since the part connected
      # with M_1 model would be present in all posteriors it cancels out. Hence
      # the important part is the one computed below.
      #
      # TODO: Why everything is divided by n_entities?

      # Eq. 19
      loglikelihood <-
        (likelihood - (lin_features_n/2)*(log(n_entities*periods_n)))/n_entities

      # Eq. 35
      bic <- exp(loglikelihood)

      c(likelihood, bic, stdh, stdr)
    }

    return(
      pbapply::pbapply(params, MARGIN = 2,  function(x) {
        std_dev_from_params(x, matrices_shared_across_models)
      }, cl = cl)
    )
  } else {
    # in non-nested version, shared matrices are not meant to be used,
    # instead generate model-specific matrices
    std_dev_from_params <- function(params) {
      regressors_subset <-
        regressor_names_from_params_vector(params)

      df_loc <- df %>% dplyr::select(
        {{ timestamp_col }},
        {{ entity_col }},
        {{ dep_var_col }},
        dplyr::all_of(regressors_subset)
      )

      data <- df_loc %>%
        matrices_from_df(
          timestamp_col = {{ timestamp_col }},
          entity_col = {{ entity_col }},
          dep_var_col = {{ dep_var_col }},
          lin_related_regressors = regressors_subset,
          which_matrices = c("Y1", "Y2", "Z", "res_maker_matrix", "cur_Y2", "cur_Z"))

      lin_features_n <- length(regressors_subset) + 1
      features_n <- ncol(data$Z)

      params_no_na <- params %>% stats::na.omit()
      likelihood <-
        sem_likelihood(params = params_no_na, data = data,
                       exact_value = TRUE)

      hess <- hessian(sem_likelihood, theta = params_no_na, data = data)

      likelihood_per_entity <-
        sem_likelihood(params_no_na, data = data, per_entity = TRUE)

      # TODO: how to interpret the Gmat and Imat
      Gmat <- rootSolve::gradient(sem_likelihood, params_no_na, data = data,
                                  per_entity = TRUE)
      Imat <- crossprod(Gmat)

      # Section 2.3.3 in Moral-Benito
      # GROWTH EMPIRICS IN PANEL DATA UNDER MODEL UNCERTAINTY AND WEAK EXOGENEITY:
      # "Finally, each model-specific posterior is given by a normal distribution
      # with mean at the MLE and dispersion matrix equal to the inverse of the
      # Fisher information."
      # This is most likely why hessian is used to compute standard errors.
      # TODO: Learn the Bernstein–von Mises theorem which explain in detail how
      # all this works
      stdr <- rep(0, features_n)
      stdh <- rep(0, features_n)

      . <- NULL
      linear_params <- t(params) %>% as.data.frame() %>%
        dplyr::select(tidyselect::matches('alpha'),
                      tidyselect::matches('beta')) %>%
        as.matrix() %>% t()

      betas_first_ind <- 4 + periods_n
      betas_last_ind <- betas_first_ind + lin_features_n - 2
      inds <- if (betas_first_ind > betas_last_ind) {
        c(1)
      } else {
        c(1, betas_first_ind:betas_last_ind)
      }

      stdr[!is.na(linear_params)] <- sqrt(diag(solve(hess) %*% Imat %*% solve(hess)))[inds]
      stdh[!is.na(linear_params)] <- sqrt(diag(solve(hess)))[inds]

      # Below we have almost 1/2 * BIC_k as in Raftery's Bayesian Model Selection
      # in Social Research eq. 19. The part with reference model M_1 is skipped,
      # because we use this formula to compute exp(logl) which is in turn used to
      # compute posterior probabilities using eqs. 34/35. Since the part connected
      # with M_1 model would be present in all posteriors it cancels out. Hence
      # the important part is the one computed below.
      #
      # TODO: Why everything is divided by n_entities?

      # Eq. 19
      loglikelihood <-
        (likelihood - (lin_features_n/2)*(log(n_entities*periods_n)))/n_entities

      # Eq. 35
      bic <- exp(loglikelihood)

      c(likelihood, bic, stdh, stdr)
    }

    return(
      pbapply::pbapply(params, MARGIN = 2,  function(x) {
        std_dev_from_params(x)
      }, cl = cl)
    )
  }
}


#' Calculation of the model_space object
#'
#' This function calculates model space, values of the maximized likelihood function, BICs, and
#' standard deviations of the parameters that will be used in Bayesian model averaging. Moreover,
#' it provides a vector with the names of the variables for bma function and the number of observations.
#'
#' @param df Data frame with data for the analysis.
#' @param timestamp_col The name of the column with time stamps
#' @param entity_col Column with entities (e.g. countries)
#' @param dep_var_col Column with the dependent variable
#' @param init_value The value with which the model space will be initialized.
#' This will be the starting point for the numerical optimization.
#' @param exact_value Whether the exact value of the likelihood should be
#' computed (\code{TRUE}) or just the proportional part (\code{FALSE}). Check
#' \link[bdsm]{sem_likelihood} for details.
#' @param cl An optional cluster object. If supplied, the function will use this
#' cluster for parallel processing. If \code{NULL} (the default),
#' \code{pbapply::pblapply} will run sequentially.
#' @param control a list of control parameters for the optimization which are
#' passed to \link[stats]{optim}. Default is
#' \code{list(trace = 2, maxit = 10000, fnscale = -1, REPORT = 100, scale = 0.05)}, but note
#' that \code{scale} is used only for adjusting the \code{parscale} element added later in the function code.
#'
#' @importFrom parallel parApply
#'
#' @return
#' List with two objects: \cr
#' 1) params - table with parameters of all estimated models \cr
#' 2) stats - table with the value of maximized likelihood function, BIC, and
#' standard errors for all estimated models \cr
#' 3) reg_names - vector with the names of the variables \cr
#' 4) observations_num - number of observations \cr
#' 5) df - data frame used in estimation
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' data_prepared <- bdsm::economic_growth[, 1:5] %>%
#'   bdsm::feature_standardization(
#'     excluded_cols = c(country, year, gdp)
#'   ) %>%
#'   bdsm::feature_standardization(
#'     group_by_col  = year,
#'     excluded_cols = country,
#'     scale         = FALSE
#'   )
#'
#' optim_model_space(
#'   df            = data_prepared,
#'   dep_var_col   = gdp,
#'   timestamp_col = year,
#'   entity_col    = country,
#'   init_value    = 0.5
#' )
#'}
#' @export
#
optim_model_space <-
  function(
    df,
    timestamp_col,
    entity_col,
    dep_var_col,
    init_value,
    nested = TRUE,
    exact_value = FALSE,
    cl = NULL,
    control = list(trace = 0, maxit = 10000, fnscale = -1, REPORT = 100, scale = 0.05)
  ) {
    params <- optim_model_space_params(
      df            = df,
      timestamp_col = {{timestamp_col}},
      entity_col    = {{entity_col}},
      dep_var_col   = {{dep_var_col}},
      init_value    = init_value,
      nested        = nested,
      exact_value   = exact_value,
      cl            = cl,
      control       = control
    )

    stats <- compute_model_space_stats(
      df            = df,
      dep_var_col   = {{dep_var_col}},
      timestamp_col = {{timestamp_col}},
      entity_col    = {{entity_col}},
      params        = params,
      nested        = nested,
      cl            = cl
    )

    reg_names <- extract_names(df)
    observations_num <- nrow((na.omit(df[,4])))

    list(params = params, stats = stats, reg_names = reg_names, observations_num = observations_num, df = df, is_nested = nested)
  }
