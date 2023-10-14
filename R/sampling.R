#' Simulation from a conditional multivariate Gaussian
#'
#' @param n sample size
#' @param ind d vector containing the index of variables to simulate
#' @param mean vector of means
#' @param covariance covariance matrix
#' @param precision precision matrix
#' @return an \code{n} by \ode{d} matrix of draws
#' @export
rcondmvnorm <- function(n, value, ind, mean, covariance, precision = NULL){
  if(is.null(precision)){
    precision <- solve(covariance)
  }
  d <- length(mean)
  mean <- as.numeric(mean)
  value <- as.numeric(value)
  # schurcomp <- function(sigma, ind) {
  #   stopifnot(c(length(ind) > 0, ncol(sigma) - length(ind) > 0))
  #   sigma[ind, ind, drop = FALSE] - sigma[ind, -ind, drop = FALSE] %*% solve(sigma[-ind, -ind, drop = FALSE]) %*% sigma[-ind, ind, drop = FALSE]
  # }
  stopifnot(d == nrow(precision),
            d == ncol(precision),
            isTRUE(all(ind %in% seq_len(d))),
            length(ind) >= 1)
  k <- length(ind)
  mu1 <- mean[ind]
  mu2 <- mean[-ind]
  if(length(value) == d){
    value <- value[-ind]
  } else{
    stopifnot(length(value) == length(ind))
  }
  Q11 <- precision[ind, ind, drop = FALSE]
  Q12 <- precision[ind, -ind, drop = FALSE]
  Q11cholinv <- backsolve(chol(Q11), diag(k), transpose = TRUE)
  condmean <- as.numeric(mu1 - crossprod(Q11cholinv) %*% Q12 %*% (value - mu2))
  if(length(Q11) == 1L){
    return(condmean + rnorm(n = n)/sqrt(Q11[1,1]))
  } else{
    return(t(condmean + Q11cholinv %*% matrix(rnorm(n*k), nrow = k, ncol = n)))
  }
}

#' Equicorrelation matrix
#'
#' Given a dimension and a correlation, returns the covariance or precision matrix
#' of the compound symmetry model.
#' @param d dimension of the matrix
#' @param rho correlation between pairs
#' @param precision logical; if \code{TRUE}, returns the precision matrix, otherwise the correlation matrix (default to \code{FALSE})
#' @return a \code{d} by \code{d} square matrix
#' @export
equicorrelation <- function(d, rho = 0, precision = FALSE){
  d <- as.integer(d)
  rho <- as.numeric(rho[1])
  stopifnot(d > 2, abs(rho) <= 1, is.logical(precision))
  if(precision){
    (diag(d) - matrix(rho/(1+(d-1)*rho), d, d))/(1-rho)
  } else{
  diag(rep(1-rho, d)) + matrix(rho, d, d)
  }
}
