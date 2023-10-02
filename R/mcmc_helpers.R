
#' Update for Markov chain Monte Carlo
#'
#' This function computes a single update for a Metropolis-Hastings
#' algorithm for univariate proposal based on random walk, adjusted Langevin
#' dynamics with gradient or a quadratic Laplace approximation.
#'
#' @inheritParams univariate_laplace_update
#' @param type [string] update type, either random walk (\code{rw}), adjusted Langevin (\code{mala}) or quadratic approximation (\code{laplace})
#' @param transform [logical] should the parameter be mapped onto the real line (\code{TRUE}) or should proposals be made based on truncated distributions (\code{FALSE})
#' @param sd_prop [double] standard deviation of the proposal
#' @inheritParams univariate_rw_update
#' @return value of the parameter
#' @export
#' @importFrom "methods" "formalArgs"
#' @importFrom "stats" "dnorm" "plogis" "qlogis" "rnorm" "runif"
mh <- function(
    type = c("rw","mala","laplace"),
    par_curr,
    par_name,
    loglik,
    loglik_grad = NULL,
    loglik_hessian = NULL,
    logprior,
    logprior_grad = NULL,
    logprior_hessian = NULL,
    lb = -Inf,
    ub = Inf,
    transform = FALSE,
    damping = 1,
    sd_prop,
    ...){
  type <- match.arg(type)
  if(type == "rw"){
    univariate_rw_update(par_curr = par_curr, par_name = par_name, loglik = loglik, logprior = logprior,
                         sd_prop = sd_prop, lb = lb, ub = ub, transform = transform,...)
  } else if(type == "mala"){
    univariate_mala_update(par_curr = par_curr, par_name = par_name, loglik = loglik, logprior = logprior,
                         loglik_grad = loglik_grad, logprior_grad = logprior_grad, damping = damping, transform = transform,
                         sd_prop = sd_prop, lb = lb, ub = ub, ...)
  } else if(type == "laplace"){
    univariate_laplace_update(par_curr = par_curr, par_name = par_name, loglik = loglik, logprior = logprior,
                         loglik_grad = loglik_grad, logprior_grad = logprior_grad, damping = damping,
                         logprior_hessian = logprior_hessian, loglik_hessian = loglik_hessian, lb = lb, ub = ub, ...)
  }
}

#' Univariate updates based on Laplace approximation
#'
#' Following Rue and Held (2004), we perform
#' a random walk based on a normal approximation
#' at the \code{cur} parameter value. This requires both
#' the gradient and the hessian of the log-likelihood and log-posterior;
#' if the latter are left undefined, they are obtained using numerical
#' differentiation via the \code{numDeriv} package.
#' In such cases, users cannot have a formal argument of the functions matching those of
#' \code{grad} and \code{hessian} (typically \code{x} or \code{method})
#'
#' The algorithm uses a random walk proposal
#' and a Metropolis acceptance step.
#'
#' @param par_curr [double] current value of the parameter
#' @param par_name [string] the name of the argument for the function
#' @param loglik [function] log likelihood function
#' @param loglik_grad [function] derivative of the log likelihood with respect to parameter of interest
#' @param loglik_hessian [function] second derivative of the log likelihood with respect to the parameter of interest
#' @param logprior [function] log prior of parameter
#' @param logprior_grad [function] derivative of the log prior with respect to parameter of interest
#' @param logprior_hessian [function] second derivative of the log prior with respect to the parameter of interest
#' @param lb [scalar] lower bound for the parameter
#' @param ub [scalar] upper bound for the parameter
#' @param damping [scalar] contraction factor for the Newton update
#' @param ... additional arguments passed to the log likelihood function and its derivatives
#' @return a new value for the parameter of interest
#' @export
#' @keywords internal
#' @references Rue, H. and L. Held (2005), Gaussian Markov random fields, CRC press, section 4.4.1
univariate_laplace_update <-
  function(par_curr,
           par_name,
           loglik,
           loglik_grad = NULL,
           loglik_hessian = NULL,
           logprior,
           logprior_grad = NULL,
           logprior_hessian = NULL,
           lb = -Inf,
           ub = Inf,
           damping = 1,
           ...){
    ellipsis <- list(...)
    args <- formalArgs(loglik)
    stopifnot(length(par_curr) == 1L,
              is.finite(par_curr),
              length(lb) == 1,
              length(ub) == 1,
              lb < ub,
              length(damping) == 1L,
              damping > 0,
              damping <= 1,
              par_name %in% args
    )
    if(is.null(logprior_grad)){
       if(isTRUE(any(c("x","method","method.args","side") %in% args))){
          stop("Some arguments in \"logprior\" match those of \"numDeriv\" functions.")
       }
       logprior_grad <- function(...){
          logprior_alt <- function(x, ...){
             args <- list(...)
             args[[par_name]] <- x
             do.call(logprior, args = args)
          }
          numDeriv::grad(func = logprior_alt, x = args[[par_name]], ...)
       }
    } else{
       if(!isTRUE(all(args == formalArgs(logprior_grad)))){
          stop("Invalid signature for \"logprior_grad\".")
       }
    }
    if(is.null(loglik_grad)){
       if(isTRUE(any(c("x","method","method.args","side") %in% args))){
          stop("Some arguments in \"loglik\" match those of \"numDeriv\" functions.")
       }
       loglik_grad <- function(...){
          loglik_alt <- function(x, ...){
             args <- list(...)
             args[[par_name]] <- x
             do.call(loglik, args = args)
          }
          numDeriv::grad(func = loglik_alt, x = args[[par_name]], ...)
       }
    } else{
       if(!isTRUE(all(args == formalArgs(loglik_grad)))){
          stop("Invalid signature for \"loglik_grad\".")
       }
    }
    # Check for Hessians
    if(is.null(logprior_hessian)){
       if(isTRUE(any(c("x","method","method.args","side") %in% args))){
          stop("Some arguments in \"logprior\" match those of \"numDeriv\" functions.")
       }
       logprior_hessian <- function(...){
          logprior_alt <- function(x, ...){
             args <- list(...)
             args[[par_name]] <- x
             do.call(logprior, args = args)
          }
          numDeriv::grad(func = logprior_alt, x = args[[par_name]], ...)
       }
    } else{
       if(!isTRUE(all(args == formalArgs(logprior_hessian)))){
          stop("Invalid signature for \"logprior_hessian\".")
       }
    }
    if(is.null(loglik_hessian)){
       if(isTRUE(any(c("x","method","method.args","side") %in% args))){
          stop("Some arguments in \"loglik\" match those of \"numDeriv\" functions.")
       }
       loglik_hessian <- function(...){
          loglik_alt <- function(x, ...){
             args <- list(...)
             args[[par_name]] <- x
             do.call(loglik, args = args)
          }
          numDeriv::hessian(func = loglik_alt, x = args[[par_name]], ...)
       }
    } else{
       if(!isTRUE(all(args == formalArgs(loglik_hessian)))){
          stop("Invalid signature for \"loglik_hessian\".")
       }
    }
    # Parameter value to be returned
    par_new <- par_curr # if all things fail
    # Copy arguments into a list to call function
    loglik_args <- ellipsis[names(ellipsis) %in% args]
    # Override value of parameter
    loglik_args[[par_name]] <- par_curr
    logpost_grad_curr <-
      do.call(what = loglik_grad,
              args = loglik_args) +
      do.call(what = logprior_grad,
              args = loglik_args)
    logpost_hessian_curr <-
      do.call(what = loglik_hessian,
              args = loglik_args) +
      do.call(what = logprior_hessian,
              args = loglik_args)
    mean_curr <- par_curr -
      damping * logpost_grad_curr/logpost_hessian_curr
    precision_curr <- -logpost_hessian_curr
    if(!isTRUE(precision_curr > 0)){
      return(par_new)
    }
    par_prop <- rtnorm(n = 1,
                       a = lb,
                       b = ub,
                       mean = mean_curr,
                       sd = sqrt(1/precision_curr))
    logpost_curr <-
      do.call(what = loglik,
              args = loglik_args) +
      do.call(what = logprior,
              args = loglik_args)
    # Reverse move
    loglik_args[[par_name]] <- par_prop
    # TODO determine whether we allow users to pass
    # other arguments to logprior
    logpost_grad_prop <-
      do.call(what = loglik_grad,
              args = loglik_args) +
      do.call(what = logprior_grad,
              args = loglik_args)
    logpost_hessian_prop <-
      do.call(what = loglik_hessian,
              args = loglik_args) +
      do.call(what = logprior_hessian,
              args = loglik_args)
    mean_prop <- par_prop -
      damping * logpost_grad_prop/logpost_hessian_prop
    precision_prop <- -logpost_hessian_prop
    if(!isTRUE(precision_prop > 0)){
      return(par_new)
    }
    logpost_prop <-
      do.call(what = loglik,
              args = loglik_args) +
      do.call(what = logprior,
              args = loglik_args)
    log_MH_ratio <-
      logpost_prop - logpost_curr +
      dtnorm(par_curr,
             a = lb,
             b = ub,
             mean = mean_prop,
             sd = sqrt(1/precision_prop),
             log = TRUE) -
      dtnorm(par_prop,
             a = lb,
             b = ub,
             mean = mean_curr,
             sd = sqrt(1/precision_curr),
             log = TRUE)
    if(log_MH_ratio > log(runif(1))){
      par_new <- par_prop
    }
    return(par_new)
  }


#' Multivariate updates based on Laplace approximation
#'
#' Following Rue and Held (2004), we perform
#' a random walk based on a multivariate normal approximation
#' at the \code{cur} parameter value.
#'
#' @param cur [double] current value of the vector parameter
#' @param par_name [string] the name of the argument for the function
#' @param loglik [function] log likelihood function
#' @param loglik_gradient [function] derivative of the log likelihood with respect to parameter of interest
#' @param loglik_hessian [function] hessian matrix, the second derivative of the log likelihood with respect to the parameter of interest
#' @param logprior [function] log prior of parameter
#' @param logprior_gradient [function] derivative of the log prior with respect to parameter of interest
#' @param logprior_hessian [function] second derivative of the log prior with respect to the parameter of interest
#' @param lb [double] vector of lower bounds for the parameters
#' @param ub [double] vector of upper bounds for the parameters
#' @param damping [double] double or vector of contraction factor for the Newton update
#' @param ... additional arguments passed to the log likelihood function and its derivatives
#' @return a new vector for the parameter of interest
#' @export
#' @keywords internal
#' @references Rue, H. and L. Held (2005), Gaussian Markov random fields, CRC, section 4.4.2
multivariate_laplace_update <-
  function(par_curr,
           par_name,
           loglik,
           loglik_grad = NULL,
           loglik_hessian = NULL,
           logprior,
           logprior_grad = NULL,
           logprior_hessian = NULL,
           lb = -Inf,
           ub = Inf,
           damping = 1,
           ...){
    ellipsis <- list(...)
    args <- formalArgs(loglik)
    stopifnot(length(par_curr) == 1L,
              is.finite(par_curr),
              length(lb) == 1,
              length(ub) == 1,
              lb < ub,
              length(damping) == 1L,
              damping > 0,
              damping <= 1,
              par_name %in% args
    )
    # Parameter value to be returned
    par_new <- par_curr # if all things fail
    # Copy arguments into a list to call function
    loglik_args <- ellipsis[names(ellipsis) %in% args]
    # Check if loglik_grad and logprior_grad are provided
    if(is.null(logprior_grad)){
       if(isTRUE(any(c("x","method","method.args","side") %in% args))){
          stop("Some arguments in \"logprior\" match those of \"numDeriv\" functions.")
       }
       logprior_grad <- function(...){
          logprior_alt <- function(x, ...){
             args <- list(...)
             args[[par_name]] <- x
             do.call(logprior, args = args)
          }
          numDeriv::grad(func = logprior_alt, x = args[[par_name]], ...)
       }
    } else{
       if(!isTRUE(all(args == formalArgs(logprior_grad)))){
          stop("Invalid signature for \"logprior_grad\".")
       }
    }
    if(is.null(loglik_grad)){
       if(isTRUE(any(c("x","method","method.args","side") %in% args))){
          stop("Some arguments in \"loglik\" match those of \"numDeriv\" functions.")
       }
       loglik_grad <- function(...){
          loglik_alt <- function(x, ...){
             args <- list(...)
             args[[par_name]] <- x
             do.call(loglik, args = args)
          }
          numDeriv::grad(func = loglik_alt, x = args[[par_name]], ...)
       }
    } else{
       if(!isTRUE(all(args == formalArgs(loglik_grad)))){
          stop("Invalid signature for \"loglik_grad\".")
       }
    }
    # Check for Hessians
    if(is.null(logprior_hessian)){
       if(isTRUE(any(c("x","method","method.args","side") %in% args))){
          stop("Some arguments in \"logprior\" match those of \"numDeriv\" functions.")
       }
       logprior_hessian <- function(...){
          logprior_alt <- function(x, ...){
             args <- list(...)
             args[[par_name]] <- x
             do.call(logprior, args = args)
          }
          numDeriv::grad(func = logprior_alt, x = args[[par_name]], ...)
       }
    } else{
       if(!isTRUE(all(args == formalArgs(logprior_hessian)))){
          stop("Invalid signature for \"logprior_hessian\".")
       }
    }
    if(is.null(loglik_hessian)){
       if(isTRUE(any(c("x","method","method.args","side") %in% args))){
          stop("Some arguments in \"loglik\" match those of \"numDeriv\" functions.")
       }
       loglik_hessian <- function(...){
          loglik_alt <- function(x, ...){
             args <- list(...)
             args[[par_name]] <- x
             do.call(loglik, args = args)
          }
          numDeriv::hessian(func = loglik_alt, x = args[[par_name]], ...)
       }
    } else{
       if(!isTRUE(all(args == formalArgs(loglik_hessian)))){
          stop("Invalid signature for \"loglik_hessian\".")
       }
    }
    # Override value of parameter
    loglik_args[[par_name]] <- par_curr
    logpost_grad_curr <-
      do.call(what = loglik_grad,
              args = loglik_args) +
      do.call(what = logprior_grad,
              args = loglik_args)
    logpost_hessian_curr <-
      do.call(what = loglik_hessian,
              args = loglik_args) +
      do.call(what = logprior_hessian,
             args = loglik_args)
    eigen_precision <- eigen(-logpost_hessian_curr)
    if(!isTRUE(all(eigen_precision$values > 0))){
      return(par_new)
    }
    covar_curr <- tcrossprod(eigen_precision$vectors %*% diag(1/sqrt(eigen_precision$values)))
    mean_curr <- par_curr -
      damping * logpost_grad_curr %*% covar_curr
    precision_curr <- -logpost_hessian_curr
    par_prop <- TruncatedNormal::rtmvnorm(
      n = 1,
      lb = lb,
      ub = ub,
      mu = mean_curr,
      sigma = covar_curr)
    logpost_curr <-
      do.call(what = loglik,
              args = loglik_args) +
      do.call(what = logprior,
              args = loglik_args)
    # Reverse move
    loglik_args[[par_name]] <- par_prop
    logpost_grad_prop <-
      do.call(what = loglik_grad,
              args = loglik_args) +
      do.call(what = logprior_grad,
              args = loglik_args)
    logpost_hessian_prop <-
      do.call(what = loglik_hessian,
              args = loglik_args) +
      do.call(what = logprior_hessian,
              args = loglik_args)
    eigen_precision <- eigen(-logpost_hessian_prop)
    if(!isTRUE(all(eigen_precision$values > 0))){
      return(par_new)
    }
    covar_prop <- tcrossprod(eigen_precision$vectors %*% diag(1/sqrt(eigen_precision$values)))
    mean_prop <- as.numeric(par_prop -
      damping * logpost_grad_prop %*% covar_prop)
    logpost_prop <-
      do.call(what = loglik,
              args = loglik_args) +
      do.call(what = logprior,
              args = loglik_args)
    log_MH_ratio <-
      logpost_prop - logpost_curr +
      TruncatedNormal::dtmvnorm(par_curr,
             lb = lb,
             ub = ub,
             mu = mean_prop,
             sigma = covar_prop,
             log = TRUE) -
      TruncatedNormal::dtmvnorm(par_prop,
             lb = lb,
             ub = ub,
             mu = mean_curr,
             sigma = covar_curr,
             log = TRUE)
    if(log_MH_ratio > log(runif(1))){
      par_new <- par_prop
    }
    return(par_new)
  }


#' Univariate updates based on MALA
#'
#' @keywords internal
#' @export
univariate_mala_update <-
  function(par_curr,
           par_name,
           loglik,
           loglik_grad = NULL,
           logprior,
           logprior_grad = NULL,
           sd_prop,
           lb = -Inf,
           ub = Inf,
           damping = 1,
           transform = FALSE,
           ...){
    ellipsis <- list(...)
    args <- formalArgs(loglik)
    stopifnot(length(par_curr) == 1L,
              is.finite(par_curr),
              length(lb) == 1,
              length(ub) == 1,
              lb < ub,
              length(damping) == 1L,
              damping > 0,
              damping <= 1,
              par_name %in% args,
              is.logical(transform),
              length(transform) == 1L
              )
    if(lb == -Inf & ub == Inf){
      transform <- FALSE
    }
# Parameter value to be returned
par_new <- par_curr # if all things fail
# Copy arguments into a list to call function
loglik_args <- ellipsis[names(ellipsis) %in% args]

# Check if loglik_grad and logprior_grad are provided
if(is.null(logprior_grad)){
   if(isTRUE(any(c("x","method","method.args","side") %in% args))){
      stop("Some arguments in \"logprior\" match those of \"numDeriv\" functions.")
   }
   logprior_grad <- function(...){
      logprior_alt <- function(x, ...){
         args <- list(...)
         args[[par_name]] <- x
         do.call(logprior, args = args)
      }
      numDeriv::grad(func = logprior_alt, x = args[[par_name]], ...)
   }
} else{
   if(!isTRUE(all(args == formalArgs(logprior_grad)))){
      stop("Invalid signature for \"logprior_grad\".")
   }
}
if(is.null(loglik_grad)){
   if(isTRUE(any(c("x","method","method.args","side") %in% args))){
      stop("Some arguments in \"loglik\" match those of \"numDeriv\" functions.")
   }
   loglik_grad <- function(...){
      loglik_alt <- function(x, ...){
         args <- list(...)
         args[[par_name]] <- x
         do.call(loglik, args = args)
      }
      numDeriv::grad(func = loglik_alt, x = args[[par_name]], ...)
   }
} else{
   if(!isTRUE(all(args == formalArgs(loglik_grad)))){
      stop("Invalid signature for \"loglik_grad\".")
   }
}

# Override value of parameter
loglik_args[[par_name]] <- par_curr
logpost_curr <-
  do.call(what = loglik,
          args = loglik_args) +
  do.call(what = logprior,
          args = loglik_args)

if(!transform){
  logpost_grad_curr <-
    do.call(what = loglik_grad,
            args = loglik_args) +
    do.call(what = logprior_grad,
            args = loglik_args)
  mean_curr <- par_curr +
    damping * 0.5 * logpost_grad_curr
  par_prop <- rtnorm(n = 1,
                     a = lb,
                     b = ub,
                     mean = mean_curr,
                     sd = sd_prop)
  # Reverse move
  loglik_args[[par_name]] <- par_prop
  logpost_grad_prop <-
    do.call(what = loglik_grad,
            args = loglik_args) +
    do.call(what = logprior_grad,
            args = loglik_args)
  mean_prop <- par_prop +
    damping * 0.5 * logpost_grad_prop
  logpost_prop <-
    do.call(what = loglik,
            args = loglik_args) +
    do.call(what = logprior,
            args = loglik_args)
  log_MH_ratio <-
    logpost_prop - logpost_curr +
    dtnorm(par_curr,
           a = lb,
           b = ub,
           mean = mean_prop,
           sd = sd_prop,
           log = TRUE) -
    dtnorm(par_prop,
           a = lb,
           b = ub,
           mean = mean_curr,
           sd = sd_prop,
           log = TRUE)
} else {# TRANSFORM
  tpar_curr <- transfo(par_curr, lb = lb, ub = ub)
  jac_curr <- jac_inv_transfo(tpar_curr, lb = lb, ub = ub, log = FALSE)
  logpost_grad_tcurr <-
    (do.call(what = loglik_grad,
            args = loglik_args) +
    do.call(what = logprior_grad,
            args = loglik_args)) * jac_curr +
    dlogjac_inv_transfo(tpar = tpar_curr, lb = lb, ub = ub)
  tmean_curr <- tpar_curr + damping * 0.5 * logpost_grad_tcurr
  tpar_prop <- rnorm(n = 1,
                    mean = tmean_curr,
                    sd = sd_prop)
  # Reverse move
  par_prop <- inv_transfo(tpar = tpar_prop, lb = lb, ub = ub)
  loglik_args[[par_name]] <- par_prop
  logpost_prop <-
    do.call(what = loglik,
            args = loglik_args) +
    do.call(what = logprior,
            args = loglik_args)
  jac_prop <- jac_inv_transfo(tpar = tpar_prop, lb = lb, ub = ub)
  logpost_grad_tprop <-
    (do.call(what = loglik_grad,
            args = loglik_args) +
    do.call(what = logprior_grad,
            args = loglik_args)) * jac_prop +
      dlogjac_inv_transfo(tpar = tpar_prop, lb = lb, ub = ub)
  tmean_prop <- tpar_prop + damping * 0.5 * logpost_grad_tprop
  log_MH_ratio <-
    logpost_prop - logpost_curr +
    log(jac_prop) - log(jac_curr) +
    dnorm(tpar_curr,
           mean = tmean_prop,
           sd = sd_prop,
           log = TRUE) -
    dnorm(tpar_prop,
           mean = tmean_curr,
           sd = sd_prop,
           log = TRUE)
}
if(log_MH_ratio > log(runif(1))){
  par_new <- par_prop
  attr(par_new, "accept") <- TRUE
} else{
  attr(par_new, "accept") <- FALSE
}

return(par_new)
}

#' Transform parameter to unconstrained scale
#'
#' @param par scalar parameter
#' @param lb lower bound
#' @param ub upper bound
#' @references Section 56 of the Stan Reference manual version 2.9 at \url{https://github.com/stan-dev/stan/releases/download/v2.9.0/stan-reference-2.9.0.pdf}
transfo <- function(par, lb, ub){
  stopifnot(length(par) == 1L,
            length(lb) == 1L,
            length(ub) == 1L,
            isTRUE(lb < ub))
  if(lb == -Inf & ub == Inf){
    return(par)
  } else if(lb > -Inf & ub == Inf){
    return(log(par - lb))
  } else if(lb == -Inf & ub < Inf){
    return(log(ub - par))
  } else if(lb > -Inf & ub < Inf){
    return(qlogis((par - lb) / (ub - lb)))
  }
}

jac_inv_transfo <- function(tpar, lb, ub, log = FALSE){
  if(lb == -Inf & ub == Inf){
    ljac <- log(tpar)
  }
  if(lb > -Inf & ub == Inf){
    ljac <- tpar
  } else if(lb == -Inf & ub < Inf){
    ljac <- tpar
  } else if(lb > -Inf & ub < Inf){
    ljac <- log(ub - lb) + plogis(tpar, log.p = TRUE) + plogis(tpar, log.p = TRUE, lower.tail = FALSE)
  }
  if(log){
    return(ljac)
  } else{
    return(exp(ljac))
  }
}

dlogjac_inv_transfo <- function(tpar, lb, ub){
  if(lb == -Inf & ub == Inf){
    return(0)
  }
  if(lb > -Inf & ub == Inf){
    return(1)
  } else if(lb == -Inf & ub < Inf){
    return(1)
  } else{
   -1 + 2*plogis(-tpar)
  }
}

inv_transfo <- function(tpar, lb, ub){
  stopifnot(length(tpar) == 1L,
            length(lb) == 1L,
            length(ub) == 1L,
            isTRUE(lb < ub))
  if(lb == -Inf & ub == Inf){
    return(tpar)
  } else if(lb > -Inf & ub == Inf){
    return(exp(tpar) + lb)
  } else if(lb == -Inf & ub < Inf){
    return(ub - exp(tpar))
  } else{
    return(lb + (ub - lb)*plogis(tpar))
  }
}

#' Univariate updates based on random walk
#'
#' The algorithm uses a random walk proposal.
#'
#' @inheritParams univariate_laplace_update
#' @return a new value for the parameter of interest
#' @export
#' @keywords internal
univariate_rw_update <-
  function(par_curr,
           par_name,
           loglik,
           logprior,
           sd_prop,
           lb = -Inf,
           ub = Inf,
           transform = FALSE,
           ...){
    ellipsis <- list(...)
    args <- formalArgs(loglik)
    stopifnot(length(par_curr) == 1L,
              is.finite(par_curr),
              length(lb) == 1,
              length(ub) == 1,
              lb < ub)
    if(lb == -Inf & ub == Inf){
      transform <- FALSE
    }
    # Parameter value to be returned
    par_new <- par_curr # if all things fail
    # Copy arguments into a list to call function
    loglik_args <- ellipsis[names(ellipsis) %in% args]
    # Override value of parameter
    loglik_args[[par_name]] <- par_curr
    logpost_curr <-
      do.call(what = loglik,
              args = loglik_args) +
      do.call(what = logprior,
              args = loglik_args)
    if(!transform){
    par_prop <- rtnorm(n = 1,
                       a = lb,
                       b = ub,
                       mean = par_curr,
                       sd = sd_prop)
    adj <- dtnorm(par_curr,
                  a = lb,
                  b = ub,
                  mean = par_prop,
                  sd = sd_prop,
                  log = TRUE) -
      dtnorm(par_prop,
             a = lb,
             b = ub,
             mean = par_curr,
             sd = sd_prop,
             log = TRUE)
    } else{ # Transformation is TRUE
      # Y = f(X)
      # Transform parameter to the real line
      trpar_curr <- transfo(par = par_curr, lb = lb, ub = ub)
      # Sample a proposal from the normal
      trpar_prop <- rnorm(n = 1, mean = trpar_curr, sd = sd_prop)
      # Compute the difference of log-jacobians
      adj <- jac_inv_transfo(tpar = trpar_prop, lb = lb, ub = ub, log = TRUE) -
        jac_inv_transfo(tpar = trpar_curr, lb = lb, ub = ub, log = TRUE)
      par_prop <- inv_transfo(tpar = trpar_prop, lb = lb, ub = ub)
    }
    # Reverse move
    loglik_args[[par_name]] <- par_prop
    logpost_prop <-
      do.call(what = loglik,
              args = loglik_args) +
      do.call(what = logprior,
              args = loglik_args)
    log_MH_ratio <-
      logpost_prop - logpost_curr + adj
    if(log_MH_ratio > log(runif(1))){
      par_new <- par_prop
    }
    return(par_new)
  }



#' Variance adaptation
#'
#' Adapt standard deviation of a proposal for a Markov chain Monte Carlo algorithm, based on the acceptance rate.
#' The function is targeting an acceptance probability of \code{target} for univariate Metropolis--Hastings proposals, which is
#' optimal for certain Gaussian regimes. If the number of attempts is large enough and the acceptance rate is not close
#' to the target, the standard deviation of the proposal will be increased or reduced and the number acceptance and attempts
#' reinitialized. If no change is made, the components \code{acc} will be the same as acceptance and similarly for attempts.
#'
#' @param attempts integer indicating number of attempts
#' @param acceptance integer giving the number of moves accepted by the algorithm
#' @param sd.p standard deviation of proposal
#' @param target target acceptance rate
#' @return a list with components \code{sd}, \code{acc}, \code{att}
#' @export
adaptive <- function(attempts, acceptance, sd.p, target = 0.234) {
  stopifnot(sd.p > 0)
  if (attempts < acceptance) {
    stop("Invalid input: the number of attempts must be larger than the number of acceptance")
  }
  att <- attempts
  acc <- acceptance
  newsd <- sd.p
  dist_target <- qlogis(acc / att) - qlogis(target)
  dist_target <- pmax(pmin(3, dist_target), -3)
  multfact <- plogis(dist_target)/0.5
  if (att > 20 & abs(dist_target) > 2) {
      newsd <- sd.p * multfact
      att <- 0L
      acc <- 0L
  }
  if (att > 30 & abs(dist_target) > 1.25) {
      newsd <- sd.p * multfact
      att <- 0L
      acc <- 0L
  }
  if (att > 50) {
    newsd <- sd.p * multfact
    att <- 0L
    acc <- 0L
  }
  return(list(sd = newsd, acc = acc, att = att))
}

#' Univariate truncated normal sampler
#'
#' This implementations works for a < 37
#' or b > 37
#' @param n [integer] sample size
#' @param mean [numeric] scalar location parameter
#' @param sd [numeric] scalar scale parameter
#' @param a [numeric] lower bound
#' @param b [numeric] upper bound
#' @rdname tnorm
#' @export
rtnorm <- function(n = 1,
                   mean = 0,
                   sd = 1,
                   a = -Inf,
                   b = Inf
){
  stopifnot(length(a) == 1L,
            length(b) == 1L,
            length(mean) == 1L,
            length(sd) == 1L,
            isTRUE(a < b),
            isTRUE(sd > 0))
  a_std <- (a - mean) / sd
  b_std <- (b - mean) / sd
  if(b_std < -37 | a_std > 37){
    warning("Interval requested is beyond numerical tolerance.\nUse \"TruncatedNormal\" package \"rtnorm\" instead for rare events simulation.")
    return(TruncatedNormal::rtnorm(n = n, mu = mean, sd = sd, lb = a, ub = b))
  }
  if(a_std < 0){
    Fa <- pnorm(a_std)
    Fb <- pnorm(b_std)
    mean + sd * qnorm(Fa + runif(n) * (Fb - Fa))
  } else {
    Fa <- pnorm(a_std, lower.tail = FALSE)
    Fb <- pnorm(b_std, lower.tail = FALSE)
    mean + sd * (-qnorm(Fa - (Fa - Fb) * runif(n)))
  }
}

#' Density of a univariate truncated Normal
#' @importFrom "stats" "pnorm" "qnorm"
#' @param x [numeric] observation vector
#' @param log [logical] if \code{TRUE}, return the log density
#' @export
#' @rdname tnorm
dtnorm <- function(x,
                   mean = 0,
                   sd = 1,
                   a = -Inf,
                   b = Inf,
                   log = FALSE){
  stopifnot(length(a) == 1L,
            length(b) == 1L,
            length(mean) == 1L,
            length(sd) == 1L,
            isTRUE(a < b),
            isTRUE(sd > 0))
  dens <- dnorm(x, mean = mean, sd = sd, log = TRUE)
  # if(is.finite(a) & is.finite(b)){
  dens <- dens  - TruncatedNormal::lnNpr(a = (a-mean)/sd, b = (b-mean)/sd)
  # log(
  #   pnorm(q = b, mean = mean, sd = sd) -
  #     pnorm(q = a, mean = mean, sd = sd))
  # } else if(is.infinite(a) & is.finite(b)){
  #   dens <- dens - pnorm(q = b, mean = mean, sd = sd, log.p = TRUE)
  # } else if(is.finite(a) & is.infinite(b)){
  #   dens <- dens - pnorm(q = a, mean = mean, sd = sd,
  #                        log.p = TRUE, lower.tail = FALSE)
  # }
  return(dens)
}

