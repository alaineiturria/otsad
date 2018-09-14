#' Get Window Length
#'
#' @description \code{GetWindowLength} Calculates the size of the window. This window focuses on
#' the real anomaly and it can be used to know if the detected anomaly is a true positive or not.
#'
#' @param data.length Dataset length.
#' @param num.real.anomaly Number of real anomalies contained in the data set.
#'
#' @details \code{nrow.data} and \code{num.real.anomaly} must be numeric. Window length is
#' calculated as 10% of the length of the data set divided by the number of real anomalies
#' contained in it.
#'
#' @return Window length as numeric.
#'
#' @references A. Lavin and S. Ahmad, “Evaluating Real-time Anomaly Detection Algorithms – the
#' Numenta Anomaly Benchmark,” in 14th International Conference on Machine Learning and
#' Applications (IEEE ICMLA’15), 2015.
#'
#' @export

GetWindowLength <- function(data.length, num.real.anomaly) {
  windowLength <- floor((data.length * 0.1) / num.real.anomaly)
  if (windowLength %% 2 != 0) windowLength <- windowLength - 1
  return(windowLength)
}
