#' Classic processing KNN based Conformal Anomaly Detector (KNN-CAD)
#'
#' \code{CpKnnCad} calculates the anomalies of a dataset using classical
#' processing based on the KNN-CAD algorithm. KNN-CAD is a model-free anomaly
#' detection method for univariate time-series which adapts itself to
#' non-stationarity in the data stream and provides probabilistic abnormality
#' scores based on the conformal prediction paradigm.
#'
#' @param data Numerical vector with training and test dataset.
#' @param n.train Number of points of the dataset that correspond to the
#' training set.
#' @param threshold Anomaly threshold.
#' @param l Window length.
#' @param n Number of training set rows.
#' @param m Number of calibration set rows.
#' @param k Number of neighbours to take into account.
#' @param ncm.type Non Conformity Measure to use \"ICAD\" or \"LDCD\"
#' @param reducefp If TRUE reduces false positives
#'
#' @details \code{data} must be a numerical vector without NA values.
#' \code{threshold} must be a numeric value between 0 and 1. If the anomaly
#' score obtained for an observation is less than the \code{threshold}, the
#' observation will be considered abnormal. It should be noted that, to
#' determine whether an observation in time t is anomalous, the dataset must
#' have at least \code{l}+\code{n}+\code{m} values.
#'
#' @return dataset conformed by the following columns:
#'
#'   \item{is.anomaly}{1 if the value is anomalous, 0 otherwise.}
#'   \item{anomaly.score}{Probability of anomaly.}
#'
#' @references V. Ishimtsev, I. Nazarov, A. Bernstein and E. Burnaev. Conformal
#' k-NN Anomaly Detector for Univariate Data Streams. ArXiv e-prints, jun. 2017.
#'
#' @example examples/cp_knn_cad_example.R
#'
#' @export

CpKnnCad <- function(data, n.train, threshold, l, n = l, m = l, k,
                     ncm.type = "ICAD", reducefp = TRUE) {

  # validate parameters
  if (!is.numeric(data) | (sum(is.na(data)) > 0)) {
    stop("data argument must be a numeric vector and without NA values.")
  }
  if (!is.numeric(threshold) | threshold <= 0 |  threshold > 1) {
    stop("threshold argument must be a numeric value in (0,1] range.")
  }
  if (!is.numeric(l) & (l > n.train / 2)) {
    stop("l argument must be a numeric value and less than n.train / 2.")
  }
  if(l + n + m > n.train) {
    stop("n.train must be less than l + m + n")
  }
  if (!is.numeric(n)) {
    stop("n argument must be a numeric value.")
  }
  if (!is.numeric(m)) {
    stop("m argument must be a numeric value.")
  }
  if (!is.numeric(k) & (k >= n)) {
    stop("k argument must be a numeric value and less than n.")
  }
  if (ncm.type != "ICAD" & ncm.type != "LDCD") {
    stop("ncm.type argument must be ICAD or LDCD")
  }

  # Reshape dataset
  data <- t(sapply(l:length(data), function(i) data[(i-l+1):i]))

  # Auxiliar function
  CalculateKNN <- function(train, test, k, cov) {
    distances <- apply(train, 1, stats::mahalanobis, center = test,
                       cov = cov, inverted = TRUE)
    nearest <- sort(distances)[1:k]
    if (ncm.type == "ICAD") {
      alpha <- sum(nearest)
    }
    else {
      alpha <- mean(nearest)
    }
    return(alpha)
  }

  # Train Phase
  cov <- solve(diag(l))
  init <- n + m + 1
  end <- n.train - (l - 1)
  calibration.alpha <- NULL
  for (index.row in init:end) {
    training.set <- data[(index.row - n):(index.row - 1), ]
    calibration.set <- data[(index.row - m):(index.row - 1), ]
    test <- data[index.row, ]
    tryCatch({
      cov <- cov(training.set)
      cov <- solve(cov)
    }, error = function(e) {
      print("no tiene inversa")
      cov <- cov
    })

    if (is.null(calibration.alpha)) {
      calibration.alpha <- apply(calibration.set, 1, CalculateKNN,
        train = training.set, k, cov)
    }
    test.alpha <- CalculateKNN(training.set, test, k, cov)

    # Prepare parametres to next iteration
    calibration.alpha <- calibration.alpha[-1]
    calibration.alpha[m] <- test.alpha
  }

  # Test Phase
  init <- end + 1
  end <- nrow(data)
  anomaly.score <- NULL
  pred <- -1
  for (index.row in init:end) {
    # Select subsamples
    training.set <- data[(index.row - n - m):(index.row - m - 1), ]
    calibration.set <- data[(index.row - m):(index.row - 1), ]
    test <- data[index.row, ]

    # Apply KNN to Calibration and Test
    tryCatch({
      cov <- cov(training.set)
      cov <- solve(cov)
    }, error = function(e) {
      print("no tiene inversa")
      cov <- cov
    })

    if (is.null(calibration.alpha)) {
      calibration.alpha <- apply(calibration.set, 1, CalculateKNN,
                                 train = training.set, k, cov)
    }
    test.alpha <- CalculateKNN(training.set, test, k, cov)

    # Experimental p-value
    if (ncm.type == "ICAD") {
      score <- sum(calibration.alpha >= test.alpha) / m
    } else {
      score <- sum(calibration.alpha >= test.alpha) / (m + 1)
    }

    # Reduce false positives
    if (reducefp) {
      if(pred > 0) {
        pred <- pred - 1
        score <- 0.5
      } else if (score >= 0.9965) {
        pred <- floor(n.train / 5)
      }
    }

    anomaly.score <- rbind(anomaly.score, score)

    # Prepare parametres to next iteration
    calibration.alpha <- calibration.alpha[-1]
    calibration.alpha[m] <- test.alpha

  }

  rownames(anomaly.score) <- 1:nrow(anomaly.score)

  return(list(anomaly.score = anomaly.score,
              is.anomaly = anomaly.score < threshold))

}
