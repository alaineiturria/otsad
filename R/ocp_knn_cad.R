#' Optimized Clasical processing KNN based Conformal Anomaly Detector
#' @description calculates the anomalies of a data set using the optimized version of the classical processing based on the KNN-CAD algorithm
#' @param data all data set
#' @param threshold detection threshold
#' @param l window length
#' @param n number of training rows
#' @param m number of calibration rows
#' @param k number of neighbours to take into account
#'
#' @return anomaly and grade results
OcpKnnCad <- function(data, threshold, l, n, m, k) {

  # Reshape dataset
  data <- t(sapply(l:length(data), function(i) data[(i-l+1):i]))

  # Auxiliar function
  CalculateKNN <- function(train, test, k) {
    complete.set <- rbind(test, train)
    cov <- cov(complete.set)
    distances <- apply(train, 1, mahalanobis, center = test, cov = cov)
    nearest <- sort(distances)[1:k]
    alpha <- mean(nearest)
    return(alpha)
  }

  # Auxiliar function
  Test.phase <- function(index.row, env) {
    # Select subsamples
    trainingSet <- data[(index.row - n - m):(index.row - m - 1), ]
    calibrationSet <- data[(index.row - m):(index.row - 1), ]
    test <- data[index.row, ]

    # Apply KNN to Calibration and Test
    calibration.alpha <- get("calibration.alpha", envir = env)
    if (is.null(calibration.alpha)) {
      calibration.alpha <- apply(calibrationSet, 1, CalculateKNN, train = trainingSet, k)
    }
    test.alpha <- CalculateKNN(trainingSet, test, k)
    calibration.alpha <- calibration.alpha[-1]
    calibration.alpha[m] <- test.alpha
    assign("calibration.alpha", calibration.alpha, env)

    # Experimental p-value
    p.value <- sum(calibration.alpha >= test.alpha) / (m + 1)
    return(p.value)
  }

  new.enviroment <- new.env()
  assign("calibration.alpha", NULL, envir = new.enviroment)
  anomaly.score <- sapply((n + m + 1):nrow(data), Test.phase, new.enviroment)

  return(list(anomaly.score = anomaly.score, is.anomaly = anomaly.score < threshold))

}
