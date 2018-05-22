#' Incremental processing KNN based Conformal Anomaly Detector (KNN-CAD).
#'
#' \code{IpKnnCad} allows the calculation of anomalies using SD-EWMA in an
#' incremental processing mode. See also \code{\link{OipKnnCad}} the optimized
#' and faster function of this function. KNN-CAD is a model-free anomaly
#' detection method for univariate time-series which adapts itself to
#' non-stationarity in the data stream and provides probabilistic abnormality
#' scores based on the conformal prediction paradigm.
#'
#' @param data Numerical vector with training and test datasets.
#' @param threshold Anomaly threshold.
#' @param l Window length.
#' @param n Number of training set rows.
#' @param m Number of calibration set rows.
#' @param k Number of neighbours to take into account.
#' @param calibration.alpha Last calibration alpha values calculated in
#' the previous iteration and required for the next run.
#' @param last.data Last values of the dataset converted into a
#' multi-dimensional vectors.
#'
#' @details \code{data} must be a numerical vector without NA values.
#' \code{threshold} must be a numeric value between 0 and 1. If the anomaly
#' score obtained from an observation is less than the \code{threshold}, the
#' observation will be considered abnormal. It should be noted that, to
#' determine whether an observation in time t is anomalous, the dataset must
#' have at least \code{l}+\code{n}+\code{m} values. \code{calibration.alpha} and
#' \code{last.data} are the last calculations made in the last iteration of this
#' algorithm. The first time the algorithm is executed, \code{calibration.alpha}
#' and \code{last.data} values are NULL. However, to run a new batch
#' of data without having to include it in the old dataset and restart the
#' process, the two parameters returned by the last run are only needed.
#'
#' This algorithm can be used for both classical and incremental processing.
#' It should be noted that in case of having a finite dataset, the
#' \code{\link{CpKnnCad}} or \code{\link{OcpKnnCad}} algorithms are faster.
#' Incremental processing can be used in two ways. 1) Processing all available
#' data and saving \code{calibration.alpha} and \code{last.data} for future runs
#' with new data. 2) Using the
#' \href{https://CRAN.R-project.org/package=stream}{stream} library for when
#' there is much data and it does not fit into the memory. An example has been
#' made for this use case.
#'
#' @return dataset conformed by the following columns:
#'
#'   \item{is.anomaly}{1 if the value is anomalous 0, otherwise.}
#'   \item{anomaly.score}{Probability of anomaly.}
#'   \item{calibration.alpha}{Last calibration alpha values calculated in the
#'   previous iteration and required for the next run.}
#'   \item{last.data}{Last values of the dataset converted into
#'   multi-dimensional vectors.}
#'
#' @references V. Ishimtsev, I. Nazarov, A. Bernstein and E. Burnaev. Conformal
#' k-NN Anomaly Detector for Univariate Data Streams. ArXiv e-prints, jun. 2017.
#'
#' @example examples/ip_knn_cad_example.R
#'
#' @export

IpKnnCad <- function(data, threshold, l, n, m, k, calibration.alpha = NULL,
                     last.data = NULL) {

  # validate parameters
  if (!is.numeric(data) | (sum(is.na(data)) > 0)) {
    stop("data argument must be a numeric vector and without NA values.")
  }
  if (!is.numeric(threshold) | threshold <= 0 |  threshold > 1) {
    stop("threshold argument must be a numeric value in (0,1] range.")
  }
  if (!is.numeric(l)) {
    stop("l argument must be a numeric value.")
  }
  if (!is.numeric(n)) {
    stop("n argument must be a numeric value.")
  }
  if (!is.numeric(m)) {
    stop("m argument must be a numeric value.")
  }
  if (!is.numeric(k)) {
    stop("k argument must be a numeric value.")
  }
  if (!is.null(calibration.alpha) & !is.vector(calibration.alpha)) {
    stop("calibration.alpha argument must be NULL or a numeric vector with las
         execution calibration.alpha result.")
  }
  if (!is.null(last.data) & !is.matrix(last.data)) {
    stop("last.data argument must be NULL or a data.frame with las execution
          last.data result.")
  }

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
    distances <- apply(train, 1, stats::mahalanobis, center = test, cov = cov)
    nearest <- sort(distances)[1:k]
    alpha <- mean(nearest)
    return(alpha)
  }

  # Test Phase
  init <- n + m + 1
  end <- nrow(data)
  anomaly.score <- NULL
  for (index.row in init:end) {
    # Select subsamples
    training.set <- data[(index.row - n - m):(index.row - m - 1), ]
    calibration.set <- data[(index.row - m):(index.row - 1), ]
    test <- data[index.row, ]

    # Apply KNN to Calibration and Test
    if (is.null(calibration.alpha)) {
      calibration.alpha <- apply(calibration.set, 1, CalculateKNN,
                                 train = training.set, k)
    }
    test.alpha <- CalculateKNN(training.set, test, k)

    # Experimental p-value
    score <- sum(calibration.alpha < test.alpha) / (m + 1)
    anomaly.score <- rbind(anomaly.score, score)

    # Prepare parametres to next iteration
    calibration.alpha <- calibration.alpha[-1]
    calibration.alpha[m] <- test.alpha
  }

  n.data <- nrow(data)
  last.data <- data[(n.data - n - m + 1):n.data, ]
  rownames(anomaly.score) <- 1:nrow(anomaly.score)

  return(list(anomaly.score = anomaly.score,
              is.anomaly = anomaly.score < threshold,
              calibration.alpha = calibration.alpha,
              last.data = as.matrix(last.data)))
}
