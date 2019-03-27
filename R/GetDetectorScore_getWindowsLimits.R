#' Get windows limits
#'
#' @description \code{GetWindowsLimits} Calculates the start and end positions of each window that
#' are focused on the real anomalies. This windows can be used to know if the detected anomaly is a
#' true positive or not.
#'
#' @param data All dataset with training and test datasets and with at least \code{timestamp},
#' \code{value} and \code{is.real.anomaly} columns.
#' @param windowLength Window length. See \code{\link{GetWindowLength}}.
#'
#' @details \code{data} must be a data.frame with  \code{timestamp}, \code{value}, \code{is.anomaly}
#' and \code{is.real.anomaly} columns. \code{timestamp} column can be numeric, of type POSIXct, or a
#' character type date convertible to POSIXct. \code{windowLength} must be numeric value.
#'
#' @return Same data set with two additional columns \code{start.limit} and \code{end.limit} where
#' for each is.real.anomaly equal to 1 is indicated the position in the data set where each window
#' starts and ends. If two anomalies fall within the same window, the start and end positions
#' are only indicated on the first of them.
#'
#' @references A. Lavin and S. Ahmad, “Evaluating Real-time Anomaly Detection Algorithms – the
#' Numenta Anomaly Benchmark,” in 14th International Conference on Machine Learning and
#' Applications (IEEE ICMLA’15), 2015.
#'
#' @example tests/examples/getWindowsLimits_example.R
#'
#' @export

GetWindowsLimits <- function(data, windowLength = NULL) {

  col.names <- names(data)

  if (sum(c("timestamp", "value", "is.real.anomaly") %in% col.names) != 3) {
    stop("data argument must be a data.frame with timestamp, value and is.real.anomaly columns.")
  }

  if (!is.null(windowLength)) {
    if (!is.numeric(windowLength)) {
      stop("windowLength argument must be numeric.")
    }
  } else {
    windowLength <- GetWindowLength(nrow(data), sum(data$is.real.anomaly))
  }


  Calculate.limits <- function(index) {
    start <- index - floor(windowLength / 2)
    end <- index + floor(windowLength / 2)
    if (start <= 0) {
      start <- index
    }
    if (end > nrow(data)) {
      end <- nrow(data)
    }
    return(c(start, end))
  }

  anomaly.index <- which(data$is.real.anomaly == 1)
  data$start.limit <- 0
  data$end.limit <- 0
  data[anomaly.index, c("start.limit", "end.limit")] <- t(sapply(anomaly.index, Calculate.limits))

  if (length(anomaly.index) > 1) {
    for (i in 2:length(anomaly.index)) {
      if (data[anomaly.index[i], "start.limit"] < data[(anomaly.index[i - 1]), "end.limit"]) {
        start <- data[anomaly.index[i - 1], "start.limit"]
        end <- data[anomaly.index[i], "end.limit"]
        data[anomaly.index[i - 1], c("start.limit", "end.limit")] <- c(start, end)
        data[anomaly.index[i], c("start.limit", "end.limit")] <- 0
      }
    }
  }

  return(data)

}
