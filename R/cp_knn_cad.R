#' @function CpKnnCad
#' @description Clasical processing KNN based Conformal Anomaly Detector
#' @param data all data set
#' @param threshold detection threshold
#' @param l window length
#' @param n number of training rows
#' @param m number of calibration rows
#' @param k number of neighbours to take into account
#'
#' @return anomaly and grade results

CpKnnCad <- function(data, threshold, l, n, m, k) {
  
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
  
  # Test Phase
  init <- n + m + 1
  end <- nrow(data)
  anomaly.score <- NULL
  calibration.alpha <- NULL
  for (index.row in init:end) {
    # Select subsamples
    training.set <- data[(index.row - n - m):(index.row - m - 1), ]
    calibration.set <- data[(index.row - m):(index.row-1), ]
    test <- data[index.row, ]
    
    # Apply KNN to Calibration and Test
    if (is.null(calibration.alpha)) {
      calibration.alpha <- apply(calibration.set, 1, CalculateKNN, train = training.set, k)
    }
    test.alpha <- CalculateKNN(training.set, test, k)
    calibration.alpha <- calibration.alpha[-1]
    calibration.alpha[m] <- test.alpha
    
    # Experimental p-value
    p.value <- sum(calibration.alpha >= test.alpha) / (m + 1)
    anomaly.score <- rbind(anomaly.score, p.value)
  }
  
  rownames(anomaly.score) <- 1:nrow(anomaly.score)
  
  return(list(anomaly.score = anomaly.score, is.anomaly = anomaly.score < threshold))
  
}
