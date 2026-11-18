#' Simulate Data from a Gaussian Graphical Model
#'
#' Generate Gaussian data with a user-defined number of edges.
#'
#' @param n Sample size.
#' @param p Number of variables.
#' @param n_edges Number of edges in the true network.
#'
#' @return A list containing the simulated data, true adjacency
#'   matrix, true precision matrix, and true covariance matrix.
#' @export
simdata <- function(n, p, n_edges) {

  # Check inputs
  if (length(n) != 1 || !is.numeric(n) ||
      !is.finite(n) || n < 2 || n != floor(n)) {
    stop("n must be an integer greater than 1.")
  }

  if (length(p) != 1 || !is.numeric(p) ||
      !is.finite(p) || p < 2 || p != floor(p)) {
    stop("p must be an integer greater than 1.")
  }

  max_edges <- p * (p - 1) / 2

  if (length(n_edges) != 1 || !is.numeric(n_edges) ||
      !is.finite(n_edges) || n_edges < 0 ||
      n_edges > max_edges || n_edges != floor(n_edges)) {
    stop("n_edges must be an integer between 0 and p*(p-1)/2.")
  }

  if (!is.null(seed)) set.seed(seed)

  # Generate a network with exactly n_edges edges
  A_true <- matrix(0, p, p)

  idx <- which(upper.tri(A_true))
  selected <- sample(idx, n_edges)

  A_true[selected] <- 1
  A_true <- A_true + t(A_true)

  # Assign random edge weights
  Omega <- matrix(0, p, p)

  Omega[selected] <- runif(n_edges, 0.5, 1) *
    sample(c(-1, 1), n_edges, replace = TRUE)

  Omega <- Omega + t(Omega)

  # Construct a positive definite precision matrix
  s <- rowSums(abs(Omega))
  d <- ifelse(s == 0, 1, 1.01 * s)

  diag(Omega) <- d

  Omega_true <- Omega / outer(sqrt(d), sqrt(d))

  # True covariance matrix
  Sigma_true <- solve(Omega_true)

  # Generate Gaussian data
  X <- MASS::mvrnorm(
    n = n,
    mu = rep(0, p),
    Sigma = Sigma_true
  )

  return(list(
    X = X,
    A_true = A_true,
    Omega_true = Omega_true,
    Sigma_true = Sigma_true
  ))
}
