#' Simulate Data from a Scale-Free Gaussian Graphical Model
#'
#' Generate Gaussian data from a scale-free network
#'
#' @param n Sample size.
#' @param p Number of variables(genes).
#'
#' @return A list containing the simulated data, true adjacency
#'   matrix, true precision matrix, and true covariance matrix.
#' @export
#'
simdata <- function(n, p) {

  # create the graph
  graph <- igraph::sample_pa(p, directed = FALSE)

  A_true <- as.matrix(
    igraph::as_adjacency_matrix(graph)
  )

  diag(A_true) <- 0


  # Assign random edge weights
  Omega <- matrix(0, p, p)

  selected <- which(
    upper.tri(A_true) & A_true != 0
  )

  n_edges <- length(selected)

  Omega[selected] <- runif(n_edges, 0.5, 1) *
    sample(c(-1, 1), n_edges, replace = TRUE)

  Omega <- Omega + t(Omega)

  # Construct a positive definite precision matrix
  s <- rowSums(abs(Omega))

  d <- ifelse(s == 0, 1, 1.01 * s)

  diag(Omega) <- d

  Omega_true <- Omega / outer(sqrt(d), sqrt(d))

  # Calculate true covariance matrix
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
