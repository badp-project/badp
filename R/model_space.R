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
#' @param init_value A function of one argument \code{n} returning \code{n}
#' starting values (e.g. \code{function(n) runif(n, 0.1, 1)}). It generates the
#' starting point for every parameter of every model and thus enables, e.g.,
#' grid or randomized multi-start experiments. A constant starting point is
#' obtained with \code{function(n) rep(0.5, n)}. Generated values must be
#' non-zero, because zeros encode excluded parameters.
#'
#' @return matrix of model parameters
#'
#' @examples
#' library(magrittr)
#'
#' data_prepared <- badp::economic_growth[, 1:5] %>%
#'   badp::feature_standardization(
#'     excluded_cols = c(country, year, gdp)
#'   ) %>%
#'   badp::feature_standardization(
#'     group_by_col  = year,
#'     excluded_cols = country,
#'     scale         = FALSE
#'   )
#'
#' init_model_space_params(data_prepared, year, country, gdp,
#'                         init_value = function(n) runif(n, 0.1, 1))
#' @export
#' @keywords internal
init_model_space_params <- function(df, timestamp_col, entity_col,
                                    dep_var_col, init_value) {
  regressors <- df %>%
    regressor_names(timestamp_col = {{ timestamp_col }},
                    entity_col = {{ entity_col }},
                    dep_var_col = {{ dep_var_col }})
  n_regressors <- length(regressors)

  counts <- df %>% dplyr::select({{ timestamp_col }}, {{ entity_col }}) %>%
    sapply(function(x) dplyr::n_distinct(x))

  n_timestamps <- counts[[1]] - 1
  n_entities <- counts[[2]]

  fill_mask <- function(mask) {
    mask[mask == 1] <- init_value(sum(mask == 1))
    mask
  }
  inclusion_mask <- rje::powerSetMat(n_regressors)

  linear_params_matrix <-
    t(cbind(fill_mask(inclusion_mask), fill_mask(inclusion_mask)))

  rownames(linear_params_matrix) <-
    c(paste('beta', regressors, sep="_"), paste('phi_1', regressors, sep="_"))

  dep_var_matrix <-
    t(matrix(init_value(2^n_regressors * (3 + n_timestamps)),
             nrow = 2^n_regressors, ncol = 3 + n_timestamps))
  rownames(dep_var_matrix) <- c(c('alpha', 'phi_0', 'err_var'),
                                paste('dep_var', 1:n_timestamps, sep = '_'))

  n_phis <- n_regressors * (n_timestamps - 1)
  n_psis <- n_regressors * (n_timestamps - 1) * n_timestamps / 2

  psis_phis_matrix <-
    matrix(init_value((n_phis + n_psis) * 2^n_regressors),
           nrow = n_phis + n_psis, ncol = 2^n_regressors)

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
#' model space (see \link[badp]{init_model_space_params} for details).
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
#'
#' @keywords internal
regressor_names_from_params_vector <- function(params) {
  regressors_subset <-
    t(params %>% stats::na.omit()) %>% as.data.frame() %>%
    dplyr::select(tidyselect::matches('beta'))

  names(regressors_subset) <- gsub("beta_", "", names(regressors_subset))
  names(regressors_subset)
}

# Run BFGS repeatedly, each time starting from the previous solution, until
# the log-likelihood value stops improving. A restart resets BFGS's internal
# curvature approximation, which often makes further progress on
# ill-conditioned ridges where a single run stalls at its relative tolerance.
#
# The value is considered converged when a restart improves it by no more
# than restart_tol. Note that the gradient norm is deliberately not used as
# the convergence criterion: for models with nearly collinear regressors the
# likelihood forms a razor-thin curved ridge on which the value converges
# while the gradient stays large (the parameters along the ridge are simply
# not identified by the data). The final gradient norm is reported in the
# diagnostics so that such models can be identified downstream.
optim_with_restarts <- function(par, lik_tape, control, max_restarts,
                                restart_tol) {
  # BFGS is unconstrained: its line search evaluates the likelihood at trial
  # points of full step length, which - especially from a randomly drawn
  # starting point, where the first gradient is large - can lie outside the
  # region where the likelihood is defined. stats::optim() rejects a trial
  # point whose value is not finite and shrinks the step, but an R error
  # propagates and aborts the whole model space, and the Cholesky
  # factorizations in the likelihood do throw rather than return NaN when the
  # covariance matrix is not positive definite (both while taping and while
  # replaying the tape). Reporting such points as NA turns the error back
  # into the step rejection optim() knows how to handle.
  #
  # The gradient needs no such guard: optim() only evaluates it at points the
  # line search has already accepted, i.e. where the likelihood is defined.
  fn <- function(p) tryCatch(as.numeric(lik_tape(p)),
                             error = function(e) NA_real_)
  gr <- function(p) as.numeric(lik_tape$jacobian(p))
  maximize <- !is.null(control$fnscale) && control$fnscale < 0
  gain <- function(new, old) {
    if (maximize) new$value - old$value else old$value - new$value
  }

  fit <- stats::optim(par, fn, gr = gr, method = "BFGS",
                      control = control)

  n_restarts <- 0
  value_stalled <- FALSE
  while (n_restarts < max_restarts) {
    refit <- stats::optim(fit$par, fn, gr = gr, method = "BFGS",
                          control = control)
    n_restarts <- n_restarts + 1
    improvement <- gain(refit, fit)
    if (improvement > 0) fit <- refit
    if (improvement <= restart_tol && refit$convergence == 0) {
      value_stalled <- TRUE
      break
    }
  }

  list(
    par = fit$par,
    diagnostics = c(
      converged = as.numeric(value_stalled && fit$convergence == 0),
      optim_code = fit$convergence,
      n_restarts = n_restarts,
      max_abs_gradient = max(abs(gr(fit$par)))
    )
  )
}

# Keep drawing starting points from the init_value generator until one is
# feasible, i.e. until the likelihood is defined there (see the
# max_init_attempts argument of optim_model_space() for why infeasible points
# occur and why they cannot simply be optimized away).
#
# Feasibility is model-specific, because every model implies different
# matrices, so it can only be checked here, once the model-specific data is
# known.
feasible_init_params <- function(params_no_na, data, exact_value, init_value,
                                 max_init_attempts, regressors_subset) {
  par <- as.numeric(params_no_na)

  is_feasible <- function(p) {
    value <- suppressWarnings(
      sem_likelihood(p, data = data, exact_value = exact_value))
    isTRUE(is.finite(value))
  }

  n_init_draws <- 1
  while (!is_feasible(par)) {
    if (n_init_draws >= max_init_attempts) {
      model_label <- if (length(regressors_subset) == 0) {
        "the model with no linearly related regressors"
      } else {
        paste0("the model with regressors: ",
               paste(regressors_subset, collapse = ", "))
      }
      stop(sprintf(paste(
        "Could not draw a feasible starting point for %s in %d attempts:",
        "the likelihood is undefined at every drawn point (the implied",
        "covariance matrix is not positive definite). Narrow the range of",
        "the init_value generator or increase max_init_attempts."),
        model_label, max_init_attempts), call. = FALSE)
    }
    par <- as.numeric(init_value(length(par)))
    n_init_draws <- n_init_draws + 1
  }

  list(par = par, n_init_draws = n_init_draws)
}

# The standard errors of a model are read off the inverse of the observed
# information at its solution (see nested_std_dev_from_params()), so a
# solution is only of any use if that matrix can be inverted. solve() refuses
# once the reciprocal condition number drops below .Machine$double.eps, and
# requiring positive definiteness on top of that rejects stationary points
# which are not maxima - their inverse has negative entries on the diagonal
# and the square root of those is NaN.
usable_solution <- function(observed_information) {
  positive_definite <- tryCatch({
    chol(observed_information)
    TRUE
  }, error = function(e) FALSE)

  positive_definite &&
    isTRUE(rcond(observed_information) > .Machine$double.eps)
}

# Optimize a single model, from a starting point drawn from init_value, until
# the solution is one the standard errors can be computed from.
#
# BFGS stops wherever the gradient vanishes, which need not be a maximum. From
# a randomly drawn starting point a model occasionally ends up in a degenerate
# region instead - hundreds of log-likelihood units below its maximum, with an
# observed information matrix so rank-deficient that it cannot be inverted.
# The remedy attempted here is to draw a fresh starting point and run the
# whole optimization again, up to max_reoptimizations times.
#
# If every attempt ends in a degenerate region the best of them is returned
# with converged = 0 rather than an error, so that a single model cannot bring
# down the estimation of the whole model space.
optim_from_usable_start <- function(params_no_na, data, exact_value,
                                    init_value, max_init_attempts, control,
                                    max_restarts, restart_tol,
                                    max_reoptimizations, regressors_subset) {
  scale <- control$scale
  control$scale <- NULL

  optimize_from <- function(start) {
    # parscale entries must be positive; starting values can be negative when
    # init_value generates negative numbers.
    control$parscale <- scale * abs(start)

    # Tape the likelihood once for this attempt; BFGS then uses the exact
    # (automatically differentiated) gradient instead of finite differences.
    lik_tape <- RTMB::MakeTape(
      function(p) sem_likelihood(p, data = data, exact_value = exact_value),
      start
    )

    # The Hessian tape has to be derived here, before any optimization:
    # jacfun() re-traces the likelihood at the point the tape was last
    # evaluated at, and once the line search has visited a point where the
    # likelihood is undefined that re-trace throws from inside RTMB, which
    # leaves the tape unusable for everything afterwards. Right now the tape
    # has only ever seen the starting point, which is known to be feasible.
    # Evaluating the derived tape later on is unaffected.
    hessian_tape <- lik_tape$jacfun()

    optimized <- optim_with_restarts(start, lik_tape, control = control,
                                     max_restarts = max_restarts,
                                     restart_tol = restart_tol)
    # observed information: minus the Hessian of the log-likelihood
    optimized$observed_information <- -hessian_tape$jacobian(optimized$par)
    optimized$value <- suppressWarnings(
      sem_likelihood(optimized$par, data = data, exact_value = exact_value))
    optimized
  }

  draw <- feasible_init_params(params_no_na, data = data,
                               exact_value = exact_value,
                               init_value = init_value,
                               max_init_attempts = max_init_attempts,
                               regressors_subset = regressors_subset)
  n_init_draws <- draw$n_init_draws
  best <- optimize_from(draw$par)

  n_reoptimizations <- 0
  while (!usable_solution(best$observed_information) &&
         n_reoptimizations < max_reoptimizations) {
    n_reoptimizations <- n_reoptimizations + 1
    draw <- feasible_init_params(init_value(length(params_no_na)),
                                 data = data, exact_value = exact_value,
                                 init_value = init_value,
                                 max_init_attempts = max_init_attempts,
                                 regressors_subset = regressors_subset)
    n_init_draws <- n_init_draws + draw$n_init_draws

    candidate <- optimize_from(draw$par)
    # A usable solution always wins; between two unusable ones keep the
    # likelier, so that an exhausted budget still returns the best attempt.
    if (usable_solution(candidate$observed_information) ||
        isTRUE(candidate$value > best$value)) {
      best <- candidate
    }
  }

  best$diagnostics["converged"] <-
    best$diagnostics["converged"] *
    usable_solution(best$observed_information)
  best$diagnostics <- c(best$diagnostics, n_init_draws = n_init_draws)
  best
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
#' \link[badp]{sem_likelihood} for details.
#' @param init_value The generator function the starting point in \code{params}
#' was drawn from. It is used to redraw the starting point if the likelihood
#' turns out to be undefined at the drawn point. See
#' \link[badp]{optim_model_space}.
#' @param max_init_attempts Maximum number of starting points drawn from
#' \code{init_value} for this model. See \link[badp]{optim_model_space}.
#' @param control a list of control parameters for the optimization which are
#' passed to \link[stats]{optim}. Default is
#' \code{list(trace = 0, maxit = 10000, fnscale = -1, REPORT = 100, scale = 0.05)}.
#' @param max_restarts Maximum number of times the BFGS optimization is
#' restarted from its previous solution for a single model. A restart resets
#' the internal curvature approximation of BFGS, which often makes further
#' progress on ill-conditioned likelihood ridges where a single run stalls.
#' Default is \code{5}.
#' @param restart_tol Log-likelihood improvement between restarts below which
#' the optimization is considered converged. Improvements of this size are
#' immaterial for posterior model probabilities. Default is \code{1e-3}.
#' @param max_reoptimizations Maximum number of times this model is
#' re-optimized from a fresh starting point when its solution turns out to be
#' one no standard errors can be computed from. See
#' \link[badp]{optim_model_space}.
#'
#' @returns List (or matrix) of parameters describing analyzed models.
#' @export
#' @keywords internal
nested_optimization_wrapper <- function(
    params,
    df,
    timestamp_col,
    entity_col,
    dep_var_col,
    data,
    exact_value,
    init_value,
    max_init_attempts,
    control,
    max_restarts,
    restart_tol,
    max_reoptimizations
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

  optimized <- optim_from_usable_start(
    params_no_na, data = data, exact_value = exact_value,
    init_value = init_value, max_init_attempts = max_init_attempts,
    control = control, max_restarts = max_restarts, restart_tol = restart_tol,
    max_reoptimizations = max_reoptimizations,
    regressors_subset = regressors_subset)

  params[!is.na(params)] <- optimized$par
  c(params, optimized$diagnostics)
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
#' \link[badp]{sem_likelihood} for details.
#' @param init_value The generator function the starting point in \code{params}
#' was drawn from. It is used to redraw the starting point if the likelihood
#' turns out to be undefined at the drawn point. See
#' \link[badp]{optim_model_space}.
#' @param max_init_attempts Maximum number of starting points drawn from
#' \code{init_value} for this model. See \link[badp]{optim_model_space}.
#' @param control a list of control parameters for the optimization which are
#' passed to \link[stats]{optim}. Default is
#' \code{list(trace = 0, maxit = 10000, fnscale = -1, REPORT = 100, scale = 0.05)}.
#' @param n_all_regressors Integer. Total number of potential regressors in the
#' full (maximal) model space. Used to compute the full parameter dimension
#' (for \eqn{\phi} and \eqn{\psi}) so that parameters corresponding to excluded
#' regressors can be padded with \code{NA} in the non-nested setup.
#' @param n_timestamps Integer. Number of time periods in the panel (i.e. the
#' number of distinct values in \code{timestamp_col}). Used to determine the
#' required number of \eqn{\phi} and \eqn{\psi} parameters for the current model
#' and for the full model.
#' @param max_restarts Maximum number of times the BFGS optimization is
#' restarted from its previous solution for a single model. A restart resets
#' the internal curvature approximation of BFGS, which often makes further
#' progress on ill-conditioned likelihood ridges where a single run stalls.
#' Default is \code{5}.
#' @param restart_tol Log-likelihood improvement between restarts below which
#' the optimization is considered converged. Improvements of this size are
#' immaterial for posterior model probabilities. Default is \code{1e-3}.
#' @param max_reoptimizations Maximum number of times this model is
#' re-optimized from a fresh starting point when its solution turns out to be
#' one no standard errors can be computed from. See
#' \link[badp]{optim_model_space}.
#'
#' @returns List (or matrix) of parameters describing analyzed models.
#' @export
#'
#' @keywords internal
non_nested_optimization_wrapper <- function(
    params,
    df,
    timestamp_col,
    entity_col,
    dep_var_col,
    exact_value,
    init_value,
    max_init_attempts,
    n_all_regressors,
    n_timestamps,
    control,
    max_restarts,
    restart_tol,
    max_reoptimizations
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
  curr_n_phi <- curr_n_regressors * (n_timestamps - 1)
  curr_n_psi <- curr_n_regressors * (n_timestamps - 1) * n_timestamps / 2

  full_n_phi <- n_all_regressors * (n_timestamps - 1)
  full_n_psi <- n_all_regressors * (n_timestamps - 1) * n_timestamps / 2

  num_new_na <- (full_n_phi + full_n_psi) - (curr_n_phi + curr_n_psi)

  if (num_new_na != 0) {
    params[(length(params) - num_new_na + 1):length(params)] <- NA_real_
  }


  params_no_na <- stats::na.omit(params)

  optimized <- optim_from_usable_start(
    params_no_na, data = data, exact_value = exact_value,
    init_value = init_value, max_init_attempts = max_init_attempts,
    control = control, max_restarts = max_restarts, restart_tol = restart_tol,
    max_reoptimizations = max_reoptimizations,
    regressors_subset = regressors_subset)

  params[!is.na(params)] <- optimized$par
  c(params, optimized$diagnostics)
}



#' Finds MLE parameters for each model in the given model space
#'
#' Given a dataset and a generator of starting values, initializes a model
#' space by drawing a starting point for each model. Then for each
#' model performs a numerical optimization and finds parameters which maximize
#' the likelihood.
#'
#' @param df Data frame with data for the analysis.
#' @param timestamp_col The name of the column with time stamps.
#' @param entity_col Column with entities (e.g. countries).
#' @param dep_var_col Column with the dependent variable.
#' @param init_value Function of one argument \code{n} returning \code{n}
#' starting values for the numerical optimization (e.g.
#' \code{function(n) runif(n, 0.1, 1)}). It is called separately for every
#' model, so every model gets its own starting point; with a random generator
#' this turns the estimation into a randomized multi-start experiment. A
#' constant starting point is obtained with \code{function(n) rep(0.5, n)}.
#' Generated values must be non-zero, because zeros encode excluded
#' parameters.
#' @param exact_value Whether the exact value of the likelihood should be
#' computed (\code{TRUE}) or just the proportional part (\code{FALSE}). Check
#' \link[badp]{sem_likelihood} for details.
#' @param cl An optional cluster object. If supplied, the function will use this
#' cluster for parallel processing. If \code{NULL} (the default),
#' \code{pbapply::pblapply} will run sequentially.
#' @param control a list of control parameters for the optimization which are
#' passed to \link[stats]{optim}. Default is
#' \code{list(trace = 0, maxit = 10000, fnscale = -1, REPORT = 100, scale = 0.05)}.
#' @param nested Logical. If \code{TRUE} (default), compute approximate standard
#' deviations using the nested-model approach via
#' \code{nested_std_dev_from_params()}. If \code{FALSE}, use the non-nested
#' approach via \code{non_nested_std_dev_from_params()}. The choice affects which
#' approximation routine is used for each model in \code{params}.
#' @param max_restarts Maximum number of times the BFGS optimization is
#' restarted from its previous solution for a single model. A restart resets
#' the internal curvature approximation of BFGS, which often makes further
#' progress on ill-conditioned likelihood ridges where a single run stalls.
#' Default is \code{5}.
#' @param restart_tol Log-likelihood improvement between restarts below which
#' the optimization is considered converged. Improvements of this size are
#' immaterial for posterior model probabilities. Default is \code{1e-3}.
#' @param max_reoptimizations Maximum number of times a model is re-optimized
#' from a fresh starting point drawn from \code{init_value}, when the solution
#' reached turns out to be one no standard errors can be computed from. BFGS
#' stops wherever the gradient vanishes, which need not be a maximum, and from
#' a randomly drawn starting point a model occasionally ends up in a
#' degenerate region instead - hundreds of log-likelihood units below its
#' maximum, with an observed information matrix so rank-deficient that
#' inverting it into standard errors fails with \sQuote{system is
#' computationally singular}. Such a solution is therefore discarded and the
#' whole optimization repeated from a new starting point. A model whose every
#' attempt ends this way keeps the likeliest of them and is reported with
#' \code{converged = 0}; no error is raised, so a single model cannot bring
#' down the estimation of the whole model space. Default is \code{5}.
#' @param max_init_attempts Maximum number of starting points drawn from
#' \code{init_value} for a single model. Not every parameter vector is a
#' usable starting point: at some of them the covariance matrix implied by
#' the parameters is not positive definite, so the likelihood is undefined
#' there and the optimization cannot even start (taping the likelihood for
#' automatic differentiation fails on the Cholesky factorization with an
#' error such as \sQuote{the leading minor of order 13 is not positive}).
#' Whether a starting point is usable depends on the model, so it can only be
#' checked after the point has been drawn: unusable draws are discarded and
#' replaced by fresh draws from \code{init_value}, and only after
#' \code{max_init_attempts} unsuccessful draws is an error raised. The number
#' of draws each model actually needed is reported in the
#' \code{n_init_draws} row of the convergence diagnostics. Default is
#' \code{100}.
#'
#' @return
#' List (or matrix) of parameters describing analyzed models. The returned
#' matrix carries a \code{"convergence"} attribute: a matrix with one column
#' per model and rows \code{converged} (1 if the likelihood value stalled at a
#' solution standard errors can be computed from, 0 if the restart budget was
#' exhausted while still improving or every re-optimization ended in a
#' degenerate region),
#' \code{optim_code} (the \link[stats]{optim} convergence code of the final
#' run), \code{n_restarts}, \code{max_abs_gradient} and \code{n_init_draws}
#' (the number of starting points drawn from \code{init_value} before one at
#' which the likelihood is defined was found). A large final
#' gradient with \code{converged = 1} indicates parameters on a degenerate
#' (nearly collinear) likelihood ridge rather than a failed optimization.
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
  control = list(trace = 0, maxit = 10000, fnscale = -1, REPORT = 100, scale = 0.05),
  max_restarts = 5,
  restart_tol = 1e-3,
  max_reoptimizations = 5,
  max_init_attempts = 100
  ) {

  all_regressors <- df %>%
    dplyr::select(
      -c(
        {{timestamp_col}},
        {{entity_col}},
        {{dep_var_col}}
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
    raw <- pbapply::pbapply(init_params, MARGIN = 2,  function(x) {
      nested_optimization_wrapper(
        x,
        df = df,
        timestamp_col = {{ timestamp_col }},
        entity_col = {{ entity_col }},
        dep_var_col = {{ dep_var_col }},
        data = matrices_shared_across_models,
        exact_value = exact_value,
        init_value = init_value,
        max_init_attempts = max_init_attempts,
        control = control,
        max_restarts = max_restarts,
        restart_tol = restart_tol,
        max_reoptimizations = max_reoptimizations
        )
    }, cl = cl)
    return(split_convergence_diagnostics(raw, nrow(init_params)))
  } else {
    # optimization performed for non-nested version
    n_all_regressors = ncol(all_regressors)
    n_timestamps = df %>%
      dplyr::distinct({{timestamp_col}}) %>%
      nrow() - 1


    raw <- pbapply::pbapply(init_params, MARGIN = 2,  function(x) {
      non_nested_optimization_wrapper(
        x,
        df = df,
        timestamp_col = {{ timestamp_col }},
        entity_col = {{ entity_col }},
        dep_var_col = {{ dep_var_col }},
        exact_value = exact_value,
        init_value = init_value,
        max_init_attempts = max_init_attempts,
        n_all_regressors = n_all_regressors,
        n_timestamps = n_timestamps,
        control = control,
        max_restarts = max_restarts,
        restart_tol = restart_tol,
        max_reoptimizations = max_reoptimizations
        )
    }, cl = cl)
    return(split_convergence_diagnostics(raw, nrow(init_params)))
  }
}

# The optimization wrappers return the optimized parameter vector with the
# convergence-diagnostic values appended, so that pbapply can carry both
# through a single matrix. Split them apart again: the diagnostics travel as
# an attribute so that the params matrix keeps its documented shape.
split_convergence_diagnostics <- function(raw, n_param_rows) {
  params <- raw[seq_len(n_param_rows), , drop = FALSE]
  diagnostics <- raw[-seq_len(n_param_rows), , drop = FALSE]
  attr(params, "convergence") <- diagnostics
  params
}

#' Helper function - wraps single execution of the log-likelihood & deviation
#' parameters calculations. Used for nested version of SEM likelihood.
#'
#' @param params A matrix (with named rows) with each column corresponding
#' to a model. Each row specifies model parameters. Compare with
#' \link[badp]{optim_model_space_params}
#' @param data List of the SEM setup matrices, shared along different models
#' @param df Data frame with data for the SEM analysis.
#' @param timestamp_col The name of the column with timestamps
#' @param entity_col Column with entities (e.g. countries)
#' @param dep_var_col Column with the dependent variable
#' @param n_entities Number of entities - passed to save calc. time
#' @param n_periods Number of periods - passed to save calc. time
#'
#' @returns
#' Matrix with columns describing likelihood and standard deviations for each
#' model. The first row is the likelihood for the model (computed using the
#' parameters in the provided model space). The second row is almost 1/2 * BIC_k
#' as in Raftery's Bayesian Model Selection in Social Research eq. 19. Then there
#' are rows with standard deviations for each parameter. After that we have rows
#' with robust standard deviation.
#' @export
#' @keywords internal
nested_std_dev_from_params <- function(
    params,
    data,
    df,
    timestamp_col,
    entity_col,
    dep_var_col,
    n_entities,
    n_periods
    ) {
  regressors_subset <-
    regressor_names_from_params_vector(params)

  n_lin_features <- length(regressors_subset) + 1
  n_features <- ncol(data$Z)

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

  # Exact derivatives from the AD tape. hess is the negative Hessian of the
  # log-likelihood (observed information), matching the sign convention of
  # the finite-difference hessian() used previously. Gmat holds the
  # per-entity score vectors.
  lik_tape <- RTMB::MakeTape(
    function(p) sem_likelihood(p, data = data, exact_value = TRUE),
    as.numeric(params_no_na)
  )
  hess <- -lik_tape$jacfun()$jacobian(as.numeric(params_no_na))

  per_entity_tape <- RTMB::MakeTape(
    function(p) sem_likelihood(p, data = data, per_entity = TRUE),
    as.numeric(params_no_na)
  )
  Gmat <- per_entity_tape$jacobian(as.numeric(params_no_na))
  Imat <- crossprod(Gmat)

  stdr <- rep(0, n_features)
  stdh <- rep(0, n_features)

  . <- NULL
  linear_params <- t(params) %>% as.data.frame() %>%
    dplyr::select(tidyselect::matches('alpha'),
                  tidyselect::matches('beta')) %>%
    as.matrix() %>% t()

  betas_first_ind <- 4 + n_periods
  betas_last_ind <- betas_first_ind + n_lin_features - 2
  inds <- if (betas_first_ind > betas_last_ind) {
    c(1)
  } else {
    c(1, betas_first_ind:betas_last_ind)
  }

  stdr[!is.na(linear_params)] <- sqrt(diag(solve(hess) %*% Imat %*% solve(hess)))[inds]
  stdh[!is.na(linear_params)] <- sqrt(diag(solve(hess)))[inds]

  loglikelihood <-
    (likelihood - (n_lin_features/2)*(log(n_entities*n_periods)))/n_entities

  bic <- exp(loglikelihood)

  c(likelihood, bic, stdh, stdr)
}


#' Helper function - wraps single execution of the log-likelihood & deviation
#' parameters calculations. Used for non-nested version of SEM likelihood.
#'
#' @param params A matrix (with named rows) with each column corresponding
#' to a model. Each row specifies model parameters. Compare with
#' \link[badp]{optim_model_space_params}
#' @param df Data frame with data for the SEM analysis.
#' @param timestamp_col The name of the column with timestamps
#' @param entity_col Column with entities (e.g. countries)
#' @param dep_var_col Column with the dependent variable
#' @param n_entities Number of entities - passed to save calc. time
#' @param n_periods Number of periods - passed to save calc. time
#'
#' @returns
#' Matrix with columns describing likelihood and standard deviations for each
#' model. The first row is the likelihood for the model (computed using the
#' parameters in the provided model space). The second row is almost 1/2 * BIC_k
#' as in Raftery's Bayesian Model Selection in Social Research eq. 19. Then there
#' are rows with standard deviations for each parameter. After that we have rows
#' with robust standard deviation.
#' @export
#'
#' @keywords internal
non_nested_std_dev_from_params <- function(
    params,
    df,
    timestamp_col,
    entity_col,
    dep_var_col,
    n_entities,
    n_periods) {

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

  n_lin_features <- length(regressors_subset) + 1
  n_features <- ncol(data$Z)

  params_no_na <- params %>% stats::na.omit()
  likelihood <-
    sem_likelihood(params = params_no_na, data = data,
                   exact_value = TRUE)

  # Exact derivatives from the AD tape. hess is the negative Hessian of the
  # log-likelihood (observed information), matching the sign convention of
  # the finite-difference hessian() used previously. Gmat holds the
  # per-entity score vectors.
  lik_tape <- RTMB::MakeTape(
    function(p) sem_likelihood(p, data = data, exact_value = TRUE),
    as.numeric(params_no_na)
  )
  hess <- -lik_tape$jacfun()$jacobian(as.numeric(params_no_na))

  per_entity_tape <- RTMB::MakeTape(
    function(p) sem_likelihood(p, data = data, per_entity = TRUE),
    as.numeric(params_no_na)
  )
  Gmat <- per_entity_tape$jacobian(as.numeric(params_no_na))
  Imat <- crossprod(Gmat)

  stdr <- rep(0, n_features)
  stdh <- rep(0, n_features)

  . <- NULL
  linear_params <- t(params) %>% as.data.frame() %>%
    dplyr::select(tidyselect::matches('alpha'),
                  tidyselect::matches('beta')) %>%
    as.matrix() %>% t()

  betas_first_ind <- 4 + n_periods
  betas_last_ind <- betas_first_ind + n_lin_features - 2
  inds <- if (betas_first_ind > betas_last_ind) {
    c(1)
  } else {
    c(1, betas_first_ind:betas_last_ind)
  }

  stdr[!is.na(linear_params)] <- sqrt(diag(solve(hess) %*% Imat %*% solve(hess)))[inds]
  stdh[!is.na(linear_params)] <- sqrt(diag(solve(hess)))[inds]

  loglikelihood <-
    (likelihood - (n_lin_features/2)*(log(n_entities*n_periods)))/n_entities

  bic <- exp(loglikelihood)

  c(likelihood, bic, stdh, stdr)

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
#' \link[badp]{optim_model_space_params}
#' @param model_prior Which model prior to use. For now there are two options:
#' \code{'uniform'} and \code{'binomial-beta'}. Default is \code{'uniform'}.
#' @param exact_value Whether the exact value of the likelihood should be
#' computed (\code{TRUE}) or just the proportional part (\code{FALSE}). Check
#' \link[badp]{sem_likelihood} for details.
#' @param cl An optional cluster object. If supplied, the function will use this
#' cluster for parallel processing. If \code{NULL} (the default),
#' \code{pbapply::pblapply} will run sequentially.
#' @param nested Logical. If \code{TRUE} (default), compute approximate standard
#' deviations using the nested-model approach via
#' \code{nested_std_dev_from_params()}. If \code{FALSE}, use the non-nested
#' approach via \code{non_nested_std_dev_from_params()}. The choice affects which
#' approximation routine is used for each model in \code{params}.
#'
#' @return
#' Matrix with columns describing likelihood and standard deviations for each
#' model. The first row is the likelihood for the model (computed using the
#' parameters in the provided model space). The second row is almost 1/2 * BIC_k
#' as in Raftery's Bayesian Model Selection in Social Research eq. 19. Then there
#' are rows with standard deviations for each parameter. After that we have rows
#' with robust standard deviation.
#'
#' @importFrom pbapply pbapply
#' @export
#'
#' @examples
#' \donttest{
#'  library(magrittr)
#'  data_prepared <- badp::economic_growth[, 1:6] %>%
#'    badp::feature_standardization(
#'      excluded_cols = c(country, year, gdp)
#'    ) %>%
#'    badp::feature_standardization(
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
  n_regressors <- length(regressors)
  n_variables <- n_regressors + 1

  matrices_shared_across_models <- df %>%
    matrices_from_df(timestamp_col = {{ timestamp_col }},
                     entity_col = {{ entity_col }},
                     dep_var_col = {{ dep_var_col }},
                     which_matrices = c("Y1", "Y2", "Z", "res_maker_matrix"))

  n_entities <- nrow(matrices_shared_across_models$Z)
  n_periods <- nrow(df) / n_entities - 1

  if (nested) {
    return(
      pbapply::pbapply(params, MARGIN = 2,  function(x) {
        nested_std_dev_from_params(
          x,
          data = matrices_shared_across_models,
          df = df,
          timestamp_col = {{ timestamp_col }},
          entity_col = {{ entity_col }},
          dep_var_col = {{  dep_var_col}},
          n_entities = n_entities,
          n_periods = n_periods
          )
      }, cl = cl)
    )
  } else {
    return(
      pbapply::pbapply(params, MARGIN = 2,  function(x) {
        non_nested_std_dev_from_params(
          x,
          df = df,
          timestamp_col = {{ timestamp_col }},
          entity_col = {{ entity_col }},
          dep_var_col = {{  dep_var_col}},
          n_entities = n_entities,
          n_periods = n_periods
          )
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
#' @param init_value Function of one argument \code{n} returning \code{n}
#' starting values for the numerical optimization (e.g.
#' \code{function(n) runif(n, 0.1, 1)}). It is called separately for every
#' model, so every model gets its own starting point; with a random generator
#' this turns the estimation into a randomized multi-start experiment. A
#' constant starting point is obtained with \code{function(n) rep(0.5, n)}.
#' Generated values must be non-zero, because zeros encode excluded
#' parameters.
#' @param exact_value Whether the exact value of the likelihood should be
#' computed (\code{TRUE}) or just the proportional part (\code{FALSE}). Check
#' \link[badp]{sem_likelihood} for details.
#' @param cl An optional cluster object. If supplied, the function will use this
#' cluster for parallel processing. If \code{NULL} (the default),
#' \code{pbapply::pblapply} will run sequentially.
#' @param control a list of control parameters for the optimization which are
#' passed to \link[stats]{optim}. Default is
#' \code{list(trace = 0, maxit = 10000, fnscale = -1, REPORT = 100, scale = 0.05)}, but note
#' that \code{scale} is used only for adjusting the \code{parscale} element added later in the function code.
#' @param nested Logical. If \code{TRUE} (default), compute approximate standard
#' deviations using the nested-model approach via
#' \code{nested_std_dev_from_params()}. If \code{FALSE}, use the non-nested
#' approach via \code{non_nested_std_dev_from_params()}. The choice affects which
#' approximation routine is used for each model in \code{params}.
#' @param max_restarts Maximum number of times the BFGS optimization is
#' restarted from its previous solution for a single model. A restart resets
#' the internal curvature approximation of BFGS, which often makes further
#' progress on ill-conditioned likelihood ridges where a single run stalls.
#' Default is \code{5}.
#' @param restart_tol Log-likelihood improvement between restarts below which
#' the optimization is considered converged. Improvements of this size are
#' immaterial for posterior model probabilities. Default is \code{1e-3}.
#' @param max_reoptimizations Maximum number of times a model is re-optimized
#' from a fresh starting point drawn from \code{init_value}, when the solution
#' reached turns out to be one no standard errors can be computed from. BFGS
#' stops wherever the gradient vanishes, which need not be a maximum, and from
#' a randomly drawn starting point a model occasionally ends up in a
#' degenerate region instead - hundreds of log-likelihood units below its
#' maximum, with an observed information matrix so rank-deficient that
#' inverting it into standard errors fails with \sQuote{system is
#' computationally singular}. Such a solution is therefore discarded and the
#' whole optimization repeated from a new starting point. A model whose every
#' attempt ends this way keeps the likeliest of them and is reported with
#' \code{converged = 0}; no error is raised, so a single model cannot bring
#' down the estimation of the whole model space. Default is \code{5}.
#' @param max_init_attempts Maximum number of starting points drawn from
#' \code{init_value} for a single model. Not every parameter vector is a
#' usable starting point: at some of them the covariance matrix implied by
#' the parameters is not positive definite, so the likelihood is undefined
#' there and the optimization cannot even start (taping the likelihood for
#' automatic differentiation fails on the Cholesky factorization with an
#' error such as \sQuote{the leading minor of order 13 is not positive}).
#' Whether a starting point is usable depends on the model, so it can only be
#' checked after the point has been drawn: unusable draws are discarded and
#' replaced by fresh draws from \code{init_value}, and only after
#' \code{max_init_attempts} unsuccessful draws is an error raised. The number
#' of draws each model actually needed is reported in the
#' \code{n_init_draws} row of the \code{convergence} element of the result.
#' Default is \code{100}.
#'
#' @importFrom parallel parApply
#'
#' @return
#' An object of class \code{badp_model_space}, which is a list with the following elements: \cr
#' 1) params - table with parameters of all estimated models \cr
#' 2) stats - table with the value of maximized likelihood function, BIC, and
#' standard errors for all estimated models \cr
#' 3) reg_names - vector with the names of the variables \cr
#' 4) observations_num - number of observations \cr
#' 5) df - data frame used in estimation \cr
#' 6) is_nested - logical indicating whether nested approach was used \cr
#' 7) convergence - matrix of per-model convergence diagnostics with rows
#' \code{converged} (1 if the likelihood value stalled across restarts at a
#' solution standard errors can be computed from, 0 if the restart budget was
#' exhausted while the value was still improving or every re-optimization
#' ended in a degenerate region),
#' \code{optim_code} (the \link[stats]{optim} convergence code of the final
#' run), \code{n_restarts}, \code{max_abs_gradient} and \code{n_init_draws}
#' (the number of starting points drawn from \code{init_value} before one at
#' which the likelihood is defined was found). A large final
#' gradient with \code{converged = 1} indicates parameters on a degenerate
#' (nearly collinear) likelihood ridge; the likelihood value is trustworthy
#' but the standard errors of the affected coordinates are not.
#'
#' @section Methods:
#' Objects of class \code{badp_model_space} have the following methods available:
#' \itemize{
#'   \item \code{\link{print.badp_model_space}} - Display model space information
#' }
#'
#' @examples
#' \donttest{
#' library(magrittr)
#'
#' data_prepared <- badp::economic_growth[, 1:5] %>%
#'   badp::feature_standardization(
#'     excluded_cols = c(country, year, gdp)
#'   ) %>%
#'   badp::feature_standardization(
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
#'   init_value    = function(n) runif(n, 0.1, 1)
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
    control = list(trace = 0, maxit = 10000, fnscale = -1, REPORT = 100, scale = 0.05),
    max_restarts = 5,
    restart_tol = 1e-3,
    max_reoptimizations = 5,
    max_init_attempts = 100
  ) {
    params <- optim_model_space_params(
      df                = df,
      timestamp_col     = {{timestamp_col}},
      entity_col        = {{entity_col}},
      dep_var_col       = {{dep_var_col}},
      init_value        = init_value,
      nested            = nested,
      exact_value       = exact_value,
      cl                = cl,
      control           = control,
      max_restarts      = max_restarts,
      restart_tol         = restart_tol,
      max_reoptimizations = max_reoptimizations,
      max_init_attempts   = max_init_attempts
    )

    convergence <- attr(params, "convergence")
    attr(params, "convergence") <- NULL

    if (!is.null(convergence)) {
      n_nonconverged <- sum(convergence["converged", ] == 0)
      if (n_nonconverged > 0) {
        warning(sprintf(
          paste("%d of %d models did not converge (restart budget exhausted",
                "while the likelihood was still improving). Their",
                "likelihoods may be understated and their standard errors",
                "unreliable; see the 'convergence' element of the returned",
                "object. Consider increasing max_restarts."),
          n_nonconverged, ncol(convergence)
        ))
      }
    }

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

    structure(
      list(params = params, stats = stats, reg_names = reg_names,
           observations_num = observations_num, df = df, is_nested = nested,
           convergence = convergence),
      class = "badp_model_space"
    )
  }
