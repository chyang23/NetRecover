


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
