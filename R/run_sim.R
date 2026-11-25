#' Run Graphical LASSO Simulation Study
#'
#' Evaluate graphical lasso recovery over repeated Gaussian
#' simulations using a fixed true scale-free network.
#'
#' @param n Sample size.
#' @param p Number of variables.
#' @param lambda_seq Vector of regularization parameters.
#' @param n_sim Number of simulation repetitions.
#'
#' @return A list containing raw simulation results,
#'   summary statistics, and the true network.
#' @export

run_sim <- function(n, p, lambda_seq, n_sim = 100) {


  # Generate one fixed true network
  sim <- simdata(n, p)

  Omega_true <- sim$Omega_true
  Sigma_true <- sim$Sigma_true
  A_true <- sim$A_true

  # Run repeated simulations
  results <- vector("list", n_sim * length(lambda_seq))
  index <- 1

  for (b in seq_len(n_sim)) {

    # Generate new data from the SAME true covariance
    X <- MASS::mvrnorm(
      n = n,
      mu = rep(0, p),
      Sigma = Sigma_true
    )

    for (lambda in lambda_seq) {

      # Fit graphical lasso
      fit <- glasso_cd(X, lambda = lambda)

      # Evaluate network recovery
      evaluation <- eval_graph(
        Omega_hat = fit$Omega_hat,
        Omega_true = Omega_true
      )

      # Store simulation results
      results[[index]] <- data.frame(
        sim = b,
        lambda = lambda,
        evaluation
      )

      index <- index + 1
    }
  }

  # Combine all simulation results
  raw_results <- do.call(rbind, results)
  rownames(raw_results) <- NULL

  # Calculate mean performance for each lambda

  metrics <- c(
    "Selected", "TP", "FP", "FN", "TN",
    "TPR", "FPR", "FDP", "Frobenius"
  )

  summary_results <- aggregate(
    raw_results[, metrics],
    by = list(lambda = raw_results$lambda),
    FUN = mean
  )

  names(summary_results)[-1] <- paste0(
    "Mean_", metrics
  )

  # Return results

  return(list(
    raw = raw_results,
    summary = summary_results,
    A_true = A_true,
    Omega_true = Omega_true,
    Sigma_true = Sigma_true
  ))
}
