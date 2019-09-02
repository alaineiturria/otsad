#' Reduce Anomalies
#'
#' @description \code{ReduceAnomalies} It reduces the number of detected anomalies. This function is
#' designed to reduce the number of false positives keeping only the first detection of all those
#' that are close to each other. This proximity distance is defined by a window
#'
#' @param data Numerical vector with anomaly labels.
#' @param windowLength Window length.
#' @param incremental TRUE for incremental processing and FALSE for classic processing
#' @param last.res Last result returned by the algorithm.
#'
#' @return If \code{incremental} = FALSE,  new Numerical vector with reduced anomaly labels. Else,
#' a list of the following items.
#'  \item{result}{New Numerical vector with reduced anomaly labels.}
#'  \item{last.res}{Last result returned by the algorithm. It is a list with \code{pointer},
#'  the index of the last anomaly and \code{index}, the index number of the last point in the data}
#'
#' @example tests/examples/reduceAnomalies_example.R
#'
#' @export

ReduceAnomalies <- function (data, windowLength, incremental = FALSE, last.res = NULL) {

  if (!is.vector(data) | (!is.numeric(data) & !is.logical(data)) | (sum(is.na(data)) > 0)) {
    stop("data argument must be a numeric vector and without NA values.")
  }
  if (!is.numeric(windowLength) | length(windowLength) > 1 | windowLength < 0) {
    stop("windowLength argument must be a positive number.")
  }
  if (!is.logical(incremental) | length(incremental) > 1) {
    stop("incremental argument must be logical.")
  }
  if (!is.null(last.res) & !is.list(last.res)) {
    stop("last.res argument must be logical.")
  }

  if (is.null(last.res)) last.res <- list(pointer = -1, index = 0)
  n <- length(data)
  df <- data.frame(index = (last.res$index + 1):(last.res$index + n), value = data)
  anomaly.index <- df[which(df$value == 1), "index"]

  if (length(anomaly.index) >= 1) {
    if (last.res$pointer == -1 | anomaly.index[1] > (last.res$pointer + windowLength))
      last.res$pointer <- anomaly.index[1]
  }

  while (length(anomaly.index) >= 1) {
    to.change <- anomaly.index[anomaly.index <= (last.res$pointer + windowLength) &
                                 anomaly.index > last.res$pointer]
    if (length(to.change) != 0) df[df$index %in% to.change, "value"] <- 0
    anomaly.index <- anomaly.index[anomaly.index > (last.res$pointer + windowLength)]
    if (length(anomaly.index) > 0) last.res$pointer <- anomaly.index[1]
  }

  if (incremental) {
    return(list(result = df$value,
                last.res = list(pointer = last.res$pointer, index = df[n, "index"])))
  } else {
    return(df$value)
  }
}
