
glasso_cd <- function(X, lambda,
                      tol = 1e-6,
                      inner_tol = 1e-8,
                      maxit = 100,
                      inner_maxit = 1000) {



  n <- nrow(X)
  p <- ncol(X)



  X_centered <- scale(X, center = TRUE, scale = FALSE)

  S <- crossprod(X_centered) / n

  if (any(diag(S) <= 0)) {
    stop("X cannot contain constant columns.")
  }



  S_off <- S
  diag(S_off) <- 0

  max_off <- max(abs(S_off))

  alpha <- if (max_off == 0) {
    1
  } else {
    min(1, lambda / max_off)
  }

  W <- (1 - alpha) * S + alpha * diag(diag(S))
  converged <- FALSE


  return(list(
    Omega_hat = Omega_hat,
    Sigma_hat = W,
    A_hat = A_hat,
    niter = iter,
    converged = converged
  ))
}

