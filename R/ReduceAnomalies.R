#' Reduce Anomalies
#'
#' @description \code{ReduceAnomalies} It reduces the number of detected anomalies. This function is
#' designed to reduce the number of false positives keeping only the first detection of all those
#' that are close to each other. This proximity distance is defined by a window
#'
#' @param data Numerical vector with anomaly labels.
#' @param windowLength Window length.
#'
#' @return New Numerical vector with reduced anomaly labels.
#'
#' @export

ReduceAnomalies <- function (data, windowLength) {
  anomaly.index <- which(data == 1)
  pointer <- anomaly.index[1]
  while(length(anomaly.index) >= 1) {
    to.change <- anomaly.index[anomaly.index <= (pointer + windowLength) & anomaly.index > pointer]
    if (length(to.change) != 0) {
      data[to.change] <- 0
    }
    anomaly.index <- which(data == 1)
    anomaly.index <- anomaly.index[anomaly.index > pointer]
    pointer <- anomaly.index[1]
  }
  return(data)
}
