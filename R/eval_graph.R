#' Evaluate The Network Recovery
#'
#' Compare the estimated precision matrix with the true
#' precision matrix to evaluate network recovery.
#'
#' @param Omega_hat The estimated precision matrix.
#' @param Omega_true True precision matrix.
#' @param threshold Numerical threshold for edge selection.
#'
#' @return A data frame containing the number of selected
#' edges, TP, FP, FN, TN, TPR, FPR, FDP, and Frobenius error.
#'
#' @export


eval_graph <- function(Omega_hat, Omega_true,
                       threshold = 1e-6) {

  # use upper since symmetric
  upper <- upper.tri(Omega_true)

  # true and predicted value
  true <- abs(Omega_true[upper]) > 1e-10
  pred <- abs(Omega_hat[upper]) > threshold


  # TP FP FN and TN
  TP <- sum(true & pred)
  FP <- sum(!true & pred)
  FN <- sum(true & !pred)
  TN <- sum(!true & !pred)

  # ratios
  TPR <- if (TP + FN == 0) {
    0
  } else {
    TP / (TP + FN)
  }

  FPR <- if (FP + TN == 0) {
    0
  } else {
    FP / (FP + TN)
  }

  FDP <- if (TP + FP == 0) {
    0
  } else {
    FP / (TP + FP)
  }

  # thinking if this one is needed
  #Frobenius <- sqrt(sum((Omega_hat - Omega_true)^2))


  return(data.frame(
    Selected = TP + FP,
    TP = TP,
    FP = FP,
    FN = FN,
    TN = TN,
    TPR = TPR,
    FPR = FPR,
    FDP = FDP,
    #Frobenius = Frobenius
  ))
}
