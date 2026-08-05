# Builders for the SEM representation matrices which depend on model
# parameters. All functions are AD-generic: they accept both plain numeric
# parameters and RTMB AD types, so the same code serves direct evaluation and
# automatic-differentiation taping. This is why matrices are filled through
# subassignment (with RTMB::ADoverload) instead of matrix() constructors,
# which would drop the AD class.

#' Residual maker matrix
#'
#' Computes the residual maker matrix, i.e. a matrix M such that given a
#' matrix X and a vector Y the vector of residuals from the regression of Y on
#' X is given by M * Y
#'
#' @param m matrix based on which the residual maker matrix is built
#'
#' @return
#' Residual maker matrix
#'
#' @export
#'
#' @examples
#' residual_maker_matrix(matrix(c(1,2,3,4), nrow = 2))
#'
#' @keywords internal
residual_maker_matrix <- function(m) {
  diag(nrow(m)) - m %*% solve(crossprod(m), t(m))
}

#' Matrices with alpha and beta parameters for SEM representation
#'
#' Matrices which describe linear dependencies between dependent variable and
#' regressors (including lagged dependent variable) within the SEM
#' representation.
#'
#' @param alpha numeric
#' @param n_periods integer
#' @param beta numeric vector. Default is \code{NULL} for no regressors case.
#'
#' @return List with two matrices B11 and B12
#' @export
#'
#' @examples
#' sem_B_matrix(3, 4, 4:6)
#'
#' @keywords internal
sem_B_matrix <- function(alpha, n_periods, beta = NULL) {
  "[<-" <- RTMB::ADoverload("[<-")

  B11 <- diag(n_periods)
  for (i in 2:n_periods) {
    B11[i, i - 1] <- -alpha
  }

  if (is.null(beta) || length(beta) == 0) {
    return(list(B11, NULL))
  }

  n_regressors <- length(beta)
  B12 <- matrix(0, n_periods, n_regressors * (n_periods - 1))
  for (row_ind in 2:n_periods) {
    B12[row_ind, (row_ind - 2) * n_regressors + seq_len(n_regressors)] <- -beta
  }

  list(B11, B12)
}

#' Matrix with alpha, beta, phi_0 and phi_1 parameters for SEM representation
#'
#' Matrix which describes linear dependencies between the dependent variable
#' and the initial values of the dependent variable and the regressors within
#' the SEM representation.
#'
#' @param alpha numeric
#' @param phi_0 numeric
#' @param n_periods integer
#' @param beta numeric vector. Default is \code{NULL} for no regressors case.
#' @param phi_1 numeric vector. Default is \code{NULL} for no regressors case.
#'
#' @return Matrix with linear dependencies for initial conditions
#' @export
#'
#' @examples
#' alpha <- 9
#' phi_0 <- 19
#' beta <- 11:15
#' phi_1 <- 21:25
#' n_periods <- 4
#' sem_C_matrix(alpha, phi_0, n_periods, beta, phi_1)
#'
#' @keywords internal
sem_C_matrix <- function(alpha, phi_0, n_periods, beta = NULL, phi_1 = NULL) {
  "[<-" <- RTMB::ADoverload("[<-")

  C1 <- matrix(0, n_periods, 1)
  C1[, 1] <- phi_0
  C1[1, 1] <- C1[1, 1] + alpha

  if (is.null(beta) || length(beta) == 0) {
    return(C1)
  }

  if (length(phi_1) != length(beta)) {
    stop("phi_1 must be supplied with the same length as beta.")
  }

  n_regressors <- length(phi_1)
  C2 <- matrix(0, n_periods, n_regressors)
  for (row_ind in 1:n_periods) {
    C2[row_ind, ] <- phi_1
  }
  C2[1, ] <- C2[1, ] + beta

  cbind(C1, C2)
}

#' Matrix with psi parameters for SEM representation
#'
#' @param psis double vector with psi parameter values
#' @param n_timestamps number of time stamps (e.g. years)
#' @param n_features number of features (e.g. population size, investment rate)
#'
#' @return
#' A matrix with \code{n_timestamps} rows and
#' \code{(n_timestamps - 1) * n_features} columns. Psis are filled in row by row
#' in a block manner, i.e. blocks of size \code{n_features} are placed next to
#' each other
#'
#' @export
#'
#' @examples
#' sem_psi_matrix(1:30, 4, 5)
#'
#' @keywords internal
sem_psi_matrix <- function(psis, n_timestamps, n_features) {
  "[<-" <- RTMB::ADoverload("[<-")

  psi_m <- matrix(0, n_timestamps, (n_timestamps - 1) * n_features)
  for (row_ind in seq_len(n_timestamps - 1)) {
    start <- row_ind * (row_ind - 1) * n_features / 2 +
      (row_ind - 1) * (n_timestamps - row_ind) * n_features
    n_zeros <- (row_ind - 1) * n_features
    n_vals <- min((n_timestamps - row_ind) * n_features, length(psis) - start)
    if (n_vals > 0) {
      psi_m[row_ind, n_zeros + seq_len(n_vals)] <- psis[start + seq_len(n_vals)]
    }
  }
  psi_m
}

#' Matrices with the sigma parameters for SEM representation
#'
#' Matrices which describe the covariance structure within the SEM
#' representation.
#'
#' @param err_var numeric error variance parameter
#' @param dep_vars double vector with variance parameters of the dependent
#' variable
#' @param phis double vector with phi parameter values. Default is \code{NULL}
#' for no regressors case.
#' @param psis double vector with psi parameter values. Default is \code{NULL}
#' for no regressors case.
#'
#' @return List with two matrices describing the covariance structure
#' @export
#'
#' @examples
#' err_var <- 1
#' dep_vars <- c(2, 2, 2, 2)
#' phis <- c(10, 10, 20, 20, 30, 30)
#' psis <- c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112)
#' sem_sigma_matrix(err_var, dep_vars, phis, psis)
#'
#' @keywords internal
sem_sigma_matrix <- function(err_var, dep_vars, phis = NULL, psis = NULL) {
  "[<-" <- RTMB::ADoverload("[<-")

  n_periods <- length(dep_vars)

  # x * x rather than x^2: CppAD's pow() evaluates x^y as exp(y * log(x)) for
  # non-integer-literal y, so its second derivative is NaN wherever the tape
  # meets log(0) - even though the true derivative of x^2 is finite there.
  # The Hessian of the likelihood is needed at the fitted parameters, and a
  # fitted err_var or dep_var can legitimately land at (or very near) zero.
  O11 <- (err_var * err_var) * matrix(1, n_periods, n_periods)
  for (i in 1:n_periods) {
    O11[i, i] <- O11[i, i] + dep_vars[i] * dep_vars[i]
  }

  if (is.null(phis) || length(phis) == 0) {
    return(list(O11, NULL))
  }

  n_regressors <- length(phis) / (n_periods - 1)

  phi_m <- matrix(0, n_periods, length(phis))
  for (row_ind in 1:n_periods) {
    phi_m[row_ind, ] <- phis
  }

  psi_m <- if (is.null(psis)) {
    matrix(0, n_periods, length(phis))
  } else {
    sem_psi_matrix(psis, n_periods, n_regressors)
  }

  list(O11, phi_m + psi_m)
}
