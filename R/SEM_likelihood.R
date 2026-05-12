#' @useDynLib badp, .registration = TRUE

# Import C++ dependencies to satisfy CRAN checks.
# Fixes the following NOTE:
# Namespace in Imports field not imported from: ‘X’
#   All declared Imports should be used.
#' @importFrom Rcpp sourceCpp
#' @importFrom RcppArmadillo armadillo_version

generate_params_vector <- function(value, n_timestamps, n_regressors,
                                   n_lin_related_regressors) {
  alpha <- value
  phi_0 <- value
  err_var <- value
  dep_vars <- rep(value, n_timestamps)
  beta <- rep(value, n_lin_related_regressors)
  phi_1 <- rep(value, n_lin_related_regressors)
  n_phis <- n_regressors*(n_timestamps - 1)
  phis <- rep(value, n_phis)
  n_psis <- n_regressors*n_timestamps*(n_timestamps - 1)/2
  psis <- rep(value, n_psis)

  matrix(c(alpha, phi_0, err_var, dep_vars, phi_1, beta, phis, psis))
}

sem_params_to_list <- function(params, n_periods, n_tot_regressors,
                               n_lin_related_regressors) {
  n_phis <- n_tot_regressors*(n_periods - 1)
  n_psis <- n_tot_regressors*n_periods*(n_periods - 1)/2

  alpha <- params[1]
  phi_0 <- params[2]
  err_var <- params[3]
  dep_vars <- params[4:(4 + n_periods - 1)]
  betas_first_ind <- 4 + n_periods
  if (n_tot_regressors == 0) {
    beta <- c()
    phi_1 <- c()
    phis <- c()
    psis <- c()
  } else {
    if (n_lin_related_regressors == 0) {
      beta <- c()
      phi_1 <- c()
    } else {
      beta <-
        params[betas_first_ind:(betas_first_ind + n_lin_related_regressors - 1)]
      phi_1_first_ind <- betas_first_ind + n_lin_related_regressors
      phi_1 <-
        params[phi_1_first_ind:(phi_1_first_ind + n_lin_related_regressors - 1)]
    }
    phis <-
      params[(4 + 2*n_lin_related_regressors + n_periods):(3 + 2*n_lin_related_regressors + n_periods + n_phis)]
    psis <-
      params[(4 + 2*n_lin_related_regressors + n_periods + n_phis):(3 + 2*n_lin_related_regressors + n_periods + n_phis + n_psis)]
  }

  list(alpha = alpha, phi_0 = phi_0, err_var = err_var, dep_vars = dep_vars,
       beta = beta, phi_1 = phi_1, phis = phis, psis = psis)
}

#' List of matrices for SEM model
#'
#' @param df Dataframe with data for the likelihood computations.
#' @param timestamp_col Column which determines time stamps. For now only
#' natural numbers can be used.
#' @param entity_col Column which determines entities (e.g. countries, people)
#' @param dep_var_col Column with dependent variable
#' @param lin_related_regressors Vector of strings of column names. Which subset of regressors is in non trivial
#' linear relation with the dependent variable (\code{dep_var_col}). In other
#' words regressors with non-zero \code{beta} parameters.
#' @param which_matrices character vector with names of matrices which should be
#' computed. Possible matrices are
#' \code{"Y1"}, \code{"Y2"}, \code{"Z"}, \code{"cur_Y2"}, \code{"cur_Z"},
#' \code{"res_maker_matrix"}. Default is
#' \code{c("Y1", "Y2", "Z", "cur_Y2","cur_Z", "res_maker_matrix")} in which case
#' all possible matrices are generated
#' @importFrom dplyr select all_of
#' @importFrom magrittr "%>%"
#' @return
#' Named list with matrices as its elements
#' @export
#'
#' @examples
#' matrices_from_df(economic_growth, year, country, gdp, c("pop", "sed"),
#'                  c("Y1", "Y2"))
#'
#' @keywords internal
matrices_from_df <- function(df, timestamp_col, entity_col, dep_var_col,
                             lin_related_regressors = NULL,
                             which_matrices = c("Y1", "Y2", "Z", "cur_Y2",
                                                "cur_Z", "res_maker_matrix")) {
  Y1 <- if ("Y1" %in% which_matrices) {
    df %>% sem_dep_var_matrix(
      timestamp_col = {{ timestamp_col }}, entity_col = {{ entity_col }},
      dep_var_col = {{ dep_var_col }})
  } else NULL
  Y2 <- if ("Y2" %in% which_matrices) {
    Y2_tmp <- df %>%
      sem_regressors_matrix(
        timestamp_col = {{ timestamp_col }},
        entity_col = {{ entity_col }},
        dep_var_col = {{ dep_var_col }}
        )
    # Y2 should never be null -  but can be 0x0 matrix if no regressors in the model
    if (is.null(Y2_tmp)) matrix(numeric(0), nrow = 0, ncol = 0) else Y2_tmp
  } else NULL
  cur_Y2 <- if ("cur_Y2" %in% which_matrices) {
    df %>%
      dplyr::select({{ timestamp_col }}, {{ entity_col }}, {{ dep_var_col }},
                    all_of(lin_related_regressors)) %>%
      sem_regressors_matrix(timestamp_col = {{ timestamp_col }},
                            entity_col = {{ entity_col }},
                            dep_var_col = {{ dep_var_col }})
  } else NULL
  cur_Z <- if ("cur_Z" %in% which_matrices) {
    df %>%
      dplyr::select({{ timestamp_col }}, {{ entity_col }}, {{ dep_var_col }},
                    all_of(lin_related_regressors)) %>%
      exogenous_matrix(timestamp_col = {{ timestamp_col }},
                       entity_col = {{ entity_col }},
                       dep_var_col = {{ dep_var_col }})
  } else NULL
  Z <- if ("Z" %in% which_matrices) {
    df %>%
      exogenous_matrix(timestamp_col = {{ timestamp_col }},
                       entity_col = {{ entity_col }},
                       dep_var_col = {{ dep_var_col }})
  } else NULL
  res_maker_matrix <- if ("res_maker_matrix" %in% which_matrices) {
    if (!"Z" %in% which_matrices) {
      warning('For res_maker_matrix the Z matrix has to be computed anyway')
      Z <- df %>%
        exogenous_matrix(timestamp_col = {{ timestamp_col }},
                         entity_col = {{ entity_col }},
                         dep_var_col = {{ dep_var_col }})
    }
    residual_maker_matrix(Z)
  } else NULL

  list(Y1 = Y1, Y2 = Y2, cur_Y2 = cur_Y2, Z = Z, cur_Z = cur_Z,
       res_maker_matrix = res_maker_matrix)
}

#' Likelihood for the SEM model
#'
#' @param params Parameters describing the model. Can be either a vector or a
#' list with named parameters. See 'Details'
#' @param data Data for the likelihood computations. Can be either a list of
#' matrices or a dataframe. If the dataframe, additional parameters are
#' required to build the matrices within the function.
#' @param timestamp_col Column which determines time stamps. For now only
#' natural numbers can be used.
#' @param entity_col Column which determines entities (e.g. countries, people)
#' @param dep_var_col Column with dependent variable
#' @param lin_related_regressors Which subset of columns should be used as
#' regressors for the current model. In other words \code{regressors} are the
#' total set of regressors and \code{lin_related_regressors} are the ones for
#' which linear relation is not set to zero for a given model.
#' @param per_entity Whether to compute overall likelihood or a vector of
#' likelihoods with per entity value
#' @param exact_value Whether the exact value of the likelihood should be
#' computed (\code{TRUE}) or just the proportional part (\code{FALSE}).
#' Currently \code{TRUE} adds: 1. a normalization constant coming from Gaussian
#' distribution, 2. a term disappearing during likelihood simplification in
#' Likelihood-based Estimation of Dynamic Panels with Predetermined Regressors
#' by Moral-Benito (see Appendix A.1). The latter happens when transitioning
#' from equation (47) to equation (48), in step 2: the term \code{trace(HG_22)} is
#' dropped, because it can be assumed to be constant from Moral-Benito
#' perspective. To get the exact value of the likelihood we have to take this
#' term into account.
#'
#' @details
#' The \code{params} argument is a list that should contain the following
#' components:
#'
#' \code{alpha} scalar value which determines linear dependence on lagged
#' dependent variable
#'
#' \code{phi_0} scalar value which determines linear dependence on the value
#' of dependent variable at the lowest time stamp
#'
#' \code{err_var} scalar value which determines classical error component
#' (Sigma11 matrix, sigma_epsilon^2)
#'
#' \code{dep_vars} double vector of length equal to the number of time stamps
#' (i.e. time stamps greater than or equal to the second lowest time stamp)
#'
#' \code{beta} double vector which determines the linear dependence on
#' regressors different than the lagged dependent variable; The vector should
#' have length equal to the number of regressors.
#'
#' \code{phi_1} double vector which determines the linear dependence on
#' initial values of regressors different than the lagged dependent variable;
#' The vector should have length equal to the number of regressors.
#'
#' \code{phis} double vector which together with \code{psis} determines upper
#' right and bottom left part of the covariance matrix; The vector should have
#' length equal to the number of regressors times number of time stamps minus 1,
#' i.e. \code{n_regressors * (n_periods - 1)}
#'
#' \code{psis} double vector which together with \code{psis} determines upper
#' right and bottom left part of the covariance matrix; The vector should have
#' length equal to the number of regressors times number of time stamps minus 1
#' times number of time stamps divided by 2, i.e.
#' \code{n_regressors * (n_periods - 1) * n_periods / 2}
#'
#'
#' @return
#' The value of the likelihood for SEM model (or a part of interest of the
#' likelihood)
#'
#' @export
#'
#' @examples
#' data(economic_growth)
#' eg <- feature_standardization(economic_growth, excluded_cols = c(year, country))
#' sem_likelihood(0.5, eg, year, country, gdp)
sem_likelihood <- function(params, data, timestamp_col, entity_col, dep_var_col,
                           lin_related_regressors = NULL,
                           per_entity = FALSE,
                           exact_value = TRUE) {
  if (is.list(params) && !is.data.frame(data)) {
    alpha <- params$alpha
    phi_0 <- params$phi_0
    err_var <- params$err_var
    dep_vars <- params$dep_vars
    beta <- if(is.null(params$beta)) c() else params$beta
    phi_1 <- if(is.null(params$phi_1)) c() else params$phi_1
    phis <- if(is.null(params$phis)) c() else params$phis
    psis <- if(is.null(params$psis)) c() else params$psis

    Y1 <- data$Y1
    Y2 <- data$Y2
    cur_Y2 <- data$cur_Y2
    cur_Z <- data$cur_Z

    likelihood <- sem_likelihood_calculate(alpha, phi_0, err_var, dep_vars,
                                           Y1, Y2, cur_Z, cur_Y2, beta, phi_1,
                                           phis, psis, per_entity, exact_value)
  } else {
    if (is.data.frame(data)) {
      data <- data %>%
        matrices_from_df(timestamp_col = {{ timestamp_col }},
                         entity_col = {{ entity_col }},
                         dep_var_col = {{ dep_var_col }},
                         lin_related_regressors = lin_related_regressors)
    }
    if (!is.list(params)) {
      n_periods <- ncol(data$Y1)
      n_tot_regressors <- ncol(data$Y2) / (n_periods - 1)
      n_lin_related_regressors <- if (is.null(data$cur_Y2)) {
        0
      } else {
        ncol(data$cur_Y2) / (n_periods - 1)
      }

      if (is.double(params)) {
        params <-
          generate_params_vector(
            value = params, n_timestamps = n_periods,
            n_regressors = n_tot_regressors,
            n_lin_related_regressors = n_lin_related_regressors
            )
      }

      params <-
        sem_params_to_list(params, n_periods = n_periods,
                           n_tot_regressors = n_tot_regressors,
                           n_lin_related_regressors = n_lin_related_regressors)
    }
    likelihood <-
      sem_likelihood(params = params, data = data, per_entity = per_entity,
                     exact_value = exact_value)
  }
  likelihood
}
