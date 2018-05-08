#' Classic processing KNN based Conformal Anomaly Detector (KNN-CAD)
#'
#' \code{CpKnnCad} calculates the anomalies of a data set using classical
#' processing based on the KNN-CAD algorithm. KNN-CAD is a model-free anomaly
#' detection method for univariate time-series which adapts to non-stationarity
#' in the data stream and provides probabilistic abnormality scores based on the
#' conformal prediction paradigm.
#'
#' @param data Numerical vector that conforms the training and test data set.
#' @param threshold Anomaly threshold.
#' @param l Window length.
#' @param n Number of training set rows.
#' @param m Number of calibration set rows.
#' @param k Number of neighbours to take into account.
#'
#' @details \code{data} must be numerical vector without NA values.
#' \code{threshold} must be a numeric value between 0 and 1. If the anomaly
#' score obtained for an observation is less than the \code{threshold}, the
#' observation will be considered abnormal. It should be noted that to determine
#' whether an observation in time t is anomalous the dataset must have at least
#' \code{l}+\code{n}+\code{m} values.
#'
#' @return Data set conformed by the following columns:
#'
#'   \item{is.anomaly}{1 if the value is anomalous 0 otherwise.}
#'   \item{anomaly.score}{Probability of anomaly.}
#'
#' @references V. Ishimtsev, I. Nazarov, A. Bernstein and E. Burnaev. Conformal
#' k-NN Anomaly Detector for Univariate Data Streams. ArXiv e-prints, jun. 2017.
#'
#' @example examples/cp_knn_cad_example.R
#'
#' @export

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

    # Experimental p-value
    score <- sum(calibration.alpha < test.alpha) / (m + 1)
    anomaly.score <- rbind(anomaly.score, score)

    # Prepare parametres to next iteration
    calibration.alpha <- calibration.alpha[-1]
    calibration.alpha[m] <- test.alpha
  }

  rownames(anomaly.score) <- 1:nrow(anomaly.score)

  return(list(anomaly.score = anomaly.score, is.anomaly = anomaly.score < threshold))

}
