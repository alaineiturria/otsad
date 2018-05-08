#' Optimized Incremental processing KNN based Conformal Anomaly Detector
#' (KNN-CAD).
#'
#' \code{OipKnnCad} is the optimized implementation of the \code{IpKnnCad}
#' function using environment variables. This function allows you to calculate
#' anomalies using SD-EWMA in an incremental processing mode. KNN-CAD is a
#' model-free anomaly detection method for univariate time-series which adapts
#' to non-stationarity in the data stream and provides probabilistic
#' abnormality scores based on the conformal prediction paradigm.
#'
#' @param data Numerical vector that conforms the training and test data set.
#' @param threshold Anomaly threshold.
#' @param l Window length.
#' @param n Number of training set rows.
#' @param m Number of calibration set rows.
#' @param k Number of neighbours to take into account.
#' @param calibration.alpha Last calibration alpha values calculated in
#' the previous iteration and required for the next run.
#' @param last.data Last values of the data set converted into a
#' multi-dimensional vectors.
#'
#' @details \code{data} must be numerical vector without NA values.
#' \code{threshold} must be a numeric value between 0 and 1. If the anomaly
#' score obtained for an observation is less than the \code{threshold}, the
#' observation will be considered abnormal. It should be noted that to determine
#' whether an observation in time t is anomalous the dataset must have at least
#' \code{l}+\code{n}+\code{m} values. \code{calibration.alpha} and
#' \code{last.data} are the last calculations made in the last iteration of this
#' algorithm. The first time the algorithm is executed \code{calibration.alpha}
#' and \code{last.data} values are NULL. However, if you want to run a new batch
#' of data without having to include it in the old data set and restart the
#' process you only need to add this two parameters returned by the last run.
#'
#' #' This algorithm can be used for both classical and incremental processing.
#' It should be noted that in case of having a finite data set the
#' \code{\link{CpKnnCad}} or \code{\link{OcpKnnCad}} algorithms are faster.
#' Incremental processing can be used in two ways. 1) Processing all available
#' data and saving \code{calibration.alpha} and \code{last.data} for future runs
#' in which you have new data. 2) Using the
#' \href{https://CRAN.R-project.org/package=stream}{stream} library for when you
#' have too much data and it does not fit into memory. An example has been made
#' for this use case.
#'
#' @return Data set conformed by the following columns:
#'
#'   \item{is.anomaly}{1 if the value is anomalous 0 otherwise.}
#'   \item{anomaly.score}{Probability of anomaly.}
#'   \item{calibration.alpha}{Last calibration alpha values calculated in the
#'   previous iteration and required for the next run.}
#'   \item{last.data}{Last values of the data set converted into a
#'   multi-dimensional vectors.}
#'
#' @references V. Ishimtsev, I. Nazarov, A. Bernstein and E. Burnaev. Conformal
#' k-NN Anomaly Detector for Univariate Data Streams. ArXiv e-prints, jun. 2017.
#'
#' @example examples/oip_knn_cad_example.R
#'
#' @export


OipKnnCad <- function(data, threshold, l, n, m, k, calibration.alpha = NULL, last.data = NULL) {

  # Reshape dataset
  if (is.null(last.data)) {
    data <- t(sapply(l:length(data), function(i) data[(i-l+1):i]))
  } else {
    data <- c(last.data[nrow(last.data),], data)
    data <- rbind(last.data[-nrow(last.data),], t(sapply(l:length(data), function(i) data[(i-l+1):i])))
    rownames(data) <- 1:nrow(data)
  }

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
    training.set <- data[(index.row - n - m):(index.row - m - 1), ]
    calibration.set <- data[(index.row - m):(index.row - 1), ]
    test <- data[index.row, ]

    # Apply KNN to Calibration and Test
    calibration.alpha <- get("calibration.alpha", envir = env)
    if (is.null(calibration.alpha)) {
      calibration.alpha <- apply(calibration.set, 1, CalculateKNN,
                                 train = training.set, k)
    }
    test.alpha <- CalculateKNN(training.set, test, k)


    # Experimental p-value
    score <- sum(calibration.alpha < test.alpha) / (m + 1)

    # Prepare parametres to next iteration
    calibration.alpha <- calibration.alpha[-1]
    calibration.alpha[m] <- test.alpha
    assign("calibration.alpha", calibration.alpha, env)

    return(score)
  }


  new.enviroment <- new.env()
  assign("calibration.alpha", calibration.alpha, envir = new.enviroment)
  anomaly.score <- sapply(((n + m + 1):nrow(data)), Test.phase, new.enviroment)
  n.data <- nrow(data)
  last.data <- data[(n.data - n - m + 1):n.data, ]
  calibration.alpha <- get("calibration.alpha", envir = new.enviroment)

  return(list(anomaly.score = anomaly.score,
              is.anomaly = anomaly.score < threshold,
              calibration.alpha = calibration.alpha,
              last.data = last.data))

}
