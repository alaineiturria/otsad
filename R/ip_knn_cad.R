#' Incremental processing KNN based Conformal Anomaly Detector (KNN-CAD).
#'
#' \code{IpKnnCad} allows the calculation of anomalies using SD-EWMA in an
#' incremental processing mode. KNN-CAD is a model-free anomaly
#' detection method for univariate time-series which adapts itself to
#' non-stationarity in the data stream and provides probabilistic abnormality
#' scores based on the conformal prediction paradigm.
#'
#' @param data Numerical vector with training and test dataset.
#' @param n.train Number of points of the dataset that correspond to the
#' training set.
#' @param threshold Anomaly threshold.
#' @param l Window length.
#' @param k Number of neighbours to take into account.
#' @param ncm.type Non Conformity Measure to use "ICAD" or "LDCD"
#' @param reducefp If TRUE reduces false positives.
#' @param to.next.iteration list with the necessary parameters to execute in
#' the next iteration.
#'
#' @details \code{data} must be a numerical vector without NA values.
#' \code{threshold} must be a numeric value between 0 and 1. If the anomaly
#' score obtained for an observation is greater than the \code{threshold}, the
#' observation will be considered abnormal. \code{l} must be a numerical value
#' between 1 and 1/\code{n}; \code{n} being the length of the training data.
#' Take into account that the value of l has a direct impact on the
#' computational cost, so very high values will make the execution time longer.
#' \code{k} parameter must be a numerical value less than the \code{n.train}
#' value. \code{ncm.type} determines the non-conformity measurement to be used.
#' ICAD calculates dissimilarity as the sum of the distances of the nearest k
#' neighbours and LDCD as the average. \code{to.next.iteration}
#' is the last result returned by some previous execution of this algorithm.
#' The first time the algorithm is executed its value is NULL. However, to run a
#' new batch of data without having to include it in the old dataset and restart
#' the process, this parameter returned by the last run is only needed.
#'
#' This algorithm can be used for both classical and incremental processing.
#' It should be noted that in case of having a finite dataset, the
#' \code{\link{CpKnnCad}} algorithm is faster.
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
#'      \item \code{training.set} Last training set values used
#'      in the previous iteration and required for the next run.
#'      \item \code{calibration.set} Last calibration set values used
#'      in the previous iteration and required for the next run.
#'      \item \code{sigma} Last covariance matrix calculated in the previous
#'      iteration and required for the next run.
#'      \item \code{alphas} Last calibration alpha values calculated
#'      in the previous iteration and required for the next run.
#'      \item \code{last.data} Last values of the dataset converted into
#'      multi-dimensional vectors..
#'      \item \code{pred} Parameter that is used to reduce false positives. Only
#'      necessary in case of reducefp is TRUE.
#'      \item \code{record.count} Number of observations that have been
#'      processed up to the last iteration.
#'  }
#'
#' @references V. Ishimtsev, I. Nazarov, A. Bernstein and E. Burnaev. Conformal
#' k-NN Anomaly Detector for Univariate Data Streams. ArXiv e-prints, jun. 2017.
#'
#' @example tests/examples/ip_knn_cad_example.R
#'
#' @export

IpKnnCad <- function(data, n.train, threshold = 1, l = 19, k = 27, ncm.type = "ICAD",
                     reducefp = TRUE, to.next.iteration = NULL) {

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
  if (!is.numeric(k)) {
    stop("k argument must be a numeric value.")
  }

  # auxiliar function
  Calcular.knn <- function(test, training.set, sigma) {
    metric <- function(a, b) {
      diff <- a - b
      return((diff %*% sigma) %*% t(t(diff)))
    }

    arr <- apply(training.set, 1, metric, test)
    if (ncm.type == "ICAD") {
      return(sum(sort(as.vector(arr))[1:k]))
    } else {
      return(mean(sort(as.vector(arr))[1:k]))
    }
  }

  if (is.null(to.next.iteration)) {
    training.set <- NULL
    calibration.set <- NULL
    sigma <- diag(l)
    results <- NULL
    alphas <- NULL
    pred <- -1
    record.count <- 0
    init <- 1
  } else {
    training.set <- to.next.iteration$training.set
    calibration.set <- to.next.iteration$calibration.set
    sigma <- to.next.iteration$sigma
    alphas <- to.next.iteration$alphas
    pred <- to.next.iteration$pred
    record.count <- to.next.iteration$record.count
    data <- c(calibration.set[nrow(calibration.set), 2:l], data)
    init <- l
  }

  results <- NULL

  for (index.row in init:length(data)) {
    record.count <- record.count + 1
    if (record.count < l) {
      result <- 0
    } else {
      new.item <- data[(index.row - l + 1):index.row]
      if (record.count < n.train) {
        training.set <- rbind(training.set, new.item)
        result <- 0
      } else {
        ost <- record.count %% n.train
        if (ost == 0 | ost == floor(n.train / 2)) {
          tryCatch({
            sigma <- solve(t(training.set) %*% training.set)
          }, error = function(e) {
            print(paste0("Singular Matrix at record", record.count))
          })
        }
        if (length(alphas) == 0) {
          alphas <- sapply(1:nrow(training.set), function(j) {
            Calcular.knn(training.set[j,], training.set[-j,], sigma)
          })
        }

        alpha <- Calcular.knn(new.item, training.set, sigma)

        if (ncm.type == "ICAD") {
          result <- sum(alphas < alpha) / length(alphas)
        } else {
          result <- 1 - sum(alphas >= alpha) / (length(alphas) + 1)
        }


        if (record.count >= 2 * n.train) {
          training.set <- training.set[-1,]
          training.set <- rbind(training.set, calibration.set[1,])
          calibration.set <- calibration.set[-1,]
        }

        alphas <- alphas[-1]
        calibration.set <- rbind(calibration.set, new.item)
        alphas <- c(alphas, alpha)

        if (reducefp) {
          if (pred > 0) {
            pred <- pred - 1
            result <- 0.5
          } else if (result >= 0.9965) {
            pred <- floor(n.train / 5)
          }
        }

      }
    }

    results <- c(results, result)

  }

  return(list(anomaly.score = results, is.anomaly = results >= threshold,
              to.next.iteration = list(
                training.set = training.set,
                calibration.set = calibration.set,
                sigma = sigma,
                alphas = alphas,
                pred = pred,
                record.count = record.count
              )))

}
