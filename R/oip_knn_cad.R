#' Optimized Incremental processing KNN based Conformal Anomaly Detector
#' (KNN-CAD).
#'
#' \code{OipKnnCad} is the optimized implementation of the \code{IpKnnCad}
#' function using environmental variables. This function allows the calculation
#' of anomalies using SD-EWMA in an incremental processing mode. KNN-CAD is a
#' model-free anomaly detection method for univariate time-series which
#' adapts itself to non-stationarity in the data stream and provides
#' probabilistic abnormality scores based on the conformal prediction paradigm.
#'
#' @param data Numerical vector with training and test datasets.
#' @param n.train Number of points of the dataset that correspond to the
#' training set.
#' @param threshold Anomaly threshold.
#' @param l Window length.
#' @param n Number of training set rows.
#' @param m Number of calibration set rows.
#' @param k Number of neighbours to take into account.
#' @param ncm.type Non Conformity Measure to use \"ICAD\" or \"LDCD\"
#' @param reducefp If TRUE reduces false positives.
#' @param to.next.iteration list with the necessary parameters to execute in
#' the next iteration.
#'
#' @details \code{data} must be a numerical vector without NA values.
#' \code{threshold} must be a numeric value between 0 and 1. If the anomaly
#' score obtained from an observation is less than the \code{threshold}, the
#' observation will be considered abnormal. It should be noted that, to
#' determine whether an observation in time t is anomalous, the dataset must
#' have at least \code{l}+\code{n}+\code{m} values. \code{to.next.iteration}
#' is the last result returned by some previous execution of this algorithm.
#' The first time the algorithm is executed its value is NULL. However, to run a
#' new batch of data without having to include it in the old dataset and restart
#'  the process, this parameter returned by the last run is only needed.
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
#'   \item{to.next.iteration}{Last result returned by the algorithm. It is a list
#'   containing the following items.}
#'   \itemize{
#'      \item \code{calibration.alpha} Last calibration alpha values calculated
#'      in the previous iteration and required for the next run.
#'      \item \code{last.data} Last values of the dataset converted into
#'      multi-dimensional vectors.
#'      \item \code{last.cov} Last covariance matrix calculated in the previous
#'      iteration and required for the next run.
#'      \item \code{pred} Last covariance matrix calculated in the previous
#'      iteration and required for the next run.
#'      \item \code{num.instance} Number of observations that have been
#'      processed up to the last iteration.
#'      \item \code{pred} Parameter that is used to reduce false positives. Only
#'      necessary in case of reducefp is TRUE.
#'  }
#'
#' @references V. Ishimtsev, I. Nazarov, A. Bernstein and E. Burnaev. Conformal
#' k-NN Anomaly Detector for Univariate Data Streams. ArXiv e-prints, jun. 2017.
#'
#' @example examples/oip_knn_cad_example.R
#'
#' @export


OipKnnCad <- function(data, n.train, threshold, l, n, m, k, ncm.type = "ICAD",
                      reducefp = TRUE,
                      to.next.iteration = list(calibration.alpha = NULL,
                        last.data = NULL, last.cov = NULL,
                        pred = NULL, num.instance = NULL)) {

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

  # Reshape dataset
  if (is.null(to.next.iteration$last.data)) {
    num.data <- length(data)
    data <- t(sapply(l:num.data, function(i) data[(i-l+1):i]))
  } else {
    num.data <- length(data)
    data <- c(to.next.iteration$last.data[nrow(to.next.iteration$last.data),], data)
    data <- rbind(to.next.iteration$last.data[-nrow(to.next.iteration$last.data),],
      t(sapply(l:length(data), function(i) data[(i-l+1):i])))
    rownames(data) <- 1:nrow(data)
  }

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

  # Auxiliar function
  Train.phase <- function(index.row, env) {
    training.set <- data[(index.row - n):(index.row - 1), ]
    calibration.set <- data[(index.row - m):(index.row - 1), ]
    test <- data[index.row, ]
    tryCatch({
      cov <- cov(training.set)
      cov <- solve(cov)
      assign("cov", cov, env)
    }, error = function(e) {
      print("no tiene inversa")
      cov <- get("cov", envir = env)
    })

    calibration.alpha <- get("calibration.alpha", envir = env)
    if (is.null(calibration.alpha)) {
      calibration.alpha <- apply(calibration.set, 1, CalculateKNN,
        train = training.set, k, cov)
    }
    test.alpha <- CalculateKNN(training.set, test, k, cov)

    # Prepare parametres to next iteration
    calibration.alpha <- calibration.alpha[-1]
    calibration.alpha[m] <- test.alpha
    assign("calibration.alpha", calibration.alpha, env)

    return("ok")
  }

  # Auxiliar function
  Test.phase <- function(index.row, env) {
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
      cov <- get("cov", envir = env)
    })

    calibration.alpha <- get("calibration.alpha", envir = env)
    if (is.null(calibration.alpha)) {
      calibration.alpha <- apply(calibration.set, 1, CalculateKNN,
        train = training.set, k, cov)
    }
    test.alpha <- CalculateKNN(training.set, test, k, cov)

    # Experimental p-value
    if (ncm.type == "ICAD") {
      score <- sum(calibration.alpha < test.alpha) / m
    } else {
      score <- 1 - sum(calibration.alpha >= test.alpha) / (m + 1)
    }

    # Reduce false positives
    pred <- get("pred", envir = env)
    if (reducefp) {
      if(pred > 0) {
        pred <- pred - 1
        assign("pred", pred, env)
        score <- 0.5
      } else if (score >= 0.9965) {
        pred <- floor(n.train / 5)
        assign("pred", pred, env)
      }
    }

    # Prepare parametres to next iteration
    calibration.alpha <- calibration.alpha[-1]
    calibration.alpha[m] <- test.alpha
    assign("calibration.alpha", calibration.alpha, env)
    assign("cov", cov, env)

    return(score)
  }

  if (is.null(to.next.iteration$last.cov)) {
    cov <- solve(diag(l))
  } else {
    cov <- to.next.iteration$last.cov
  }

  if (is.null(to.next.iteration$pred)) {
    pred <- -1
  } else {
    pred <- to.next.iteration$pred
  }

  if (is.null(to.next.iteration$num.instance)) {
    num.instance <- 0
  } else {
    num.instance <- to.next.iteration$num.instance
  }

  new.enviroment <- new.env()
  assign("calibration.alpha", to.next.iteration$calibration.alpha,
         envir = new.enviroment)
  assign("cov", cov, envir = new.enviroment)
  assign("pred", pred, envir = new.enviroment)

  if (num.instance <= n.train) {
    init <- n + m + 1
    end <- n.train - (l - 1)
    none <- sapply(init:end, Train.phase, new.enviroment)
    init <- end + 1
  } else {
    init <- n + m + 1
  }
  anomaly.score <- sapply(init:nrow(data), Test.phase, new.enviroment)

  n.data <- nrow(data)
  last.data <- data[(n.data - n - m + 1):n.data, ]
  calibration.alpha <- get("calibration.alpha", envir = new.enviroment)
  pred <- get("pred", envir = new.enviroment)
  cov <- get("cov", envir = new.enviroment)

  return(list(anomaly.score = anomaly.score,
    is.anomaly = anomaly.score >= threshold,
    to.next.iteration = list(
      calibration.alpha = calibration.alpha,
      last.data = as.matrix(last.data),
      last.cov = cov, pred = pred,
      num.instance =  num.instance + num.data
    )))

}
