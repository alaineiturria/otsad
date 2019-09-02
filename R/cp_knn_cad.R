#' Classic processing KNN based Conformal Anomaly Detector (KNN-CAD)
#'
#' \code{CpKnnCad} calculates the anomalies of a dataset using classical
#' processing based on the KNN-CAD algorithm. KNN-CAD is a model-free anomaly
#' detection method for univariate time-series which adapts itself to
#' non-stationarity in the data stream and provides probabilistic abnormality
#' scores based on the conformal prediction paradigm.
#'
#' @param data Numerical vector with training and test dataset.
#' @param n.train Number of points of the dataset that correspond to the training set.
#' @param threshold Anomaly threshold.
#' @param l Window length.
#' @param k Number of neighbours to take into account.
#' @param ncm.type Non Conformity Measure to use "ICAD" or "LDCD"
#' @param reducefp If TRUE reduces false positives.
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
#' neighbours and LDCD as the average.
#'
#' @return dataset conformed by the following columns:
#'
#'   \item{is.anomaly}{1 if the value is anomalous, 0 otherwise.}
#'   \item{anomaly.score}{Probability of anomaly.}
#'
#' @references V. Ishimtsev, I. Nazarov, A. Bernstein and E. Burnaev. Conformal
#' k-NN Anomaly Detector for Univariate Data Streams. ArXiv e-prints, jun. 2017.
#'
#' @example tests/examples/cp_knn_cad_example.R
#'
#' @export

CpKnnCad <- function(data, n.train, threshold = 1, l = 19, k = 27,
                     ncm.type = "ICAD", reducefp = TRUE) {

  # validate parameters
  if (!is.numeric(data) | (sum(is.na(data)) > 0)) {
    stop("data argument must be a numeric vector and without NA values.")
  }
  if (!is.numeric(n.train) | n.train > length(data)) {
    stop("n.train argument must be a numeric value greater than data length.")
  }
  if (!is.numeric(threshold) | threshold < 0 |  threshold > 1) {
    stop("threshold argument must be a numeric value in [0,1] range.")
  }
  if (!is.numeric(l) | (l > n.train / 2)) {
    stop("l argument must be a numeric value and less than n.train / 2.")
  }
  if (!is.numeric(k) | (k >= n.train)) {
    stop("k argument must be a numeric value and less than n.")
  }
  if (ncm.type != "ICAD" & ncm.type != "LDCD") {
    stop("ncm.type argument must be ICAD or LDCD")
  }

  training.set <- NULL
  calibration.set <- NULL
  sigma <- diag(l)
  end <- length(data)
  results <- NULL
  alphas <- NULL
  pred <- -1

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

  for (index.row in 1:end) {
    record.count <- index.row
    if (record.count < l) {
      result <- 0
    } else {
      new.item <- data[(index.row - l + 1): index.row]
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

  return(data.frame(anomaly.score = results, is.anomaly = results >= threshold,
                    stringsAsFactors = FALSE))

}
