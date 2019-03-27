#' Get Window Length
#'
#' @description \code{GetWindowLength} Calculates the size of the window. This window focuses on
#' the real anomaly and it can be used to know if the detected anomaly is a true positive or not.
#'
#' @param data.length Dataset length.
#' @param num.real.anomaly Number of real anomalies contained in the data set.
#' @param window.length.perc Window length in percentage of the total data
#'
#' @details \code{nrow.data} and \code{num.real.anomaly} must be numeric. Window length is
#' calculated by default as 10\% of the length of the data set divided by the number of real
#' anomalies contained in it.
#'
#' @return Window length as numeric.
#'
#' @references A. Lavin and S. Ahmad, “Evaluating Real-time Anomaly Detection Algorithms – the
#' Numenta Anomaly Benchmark,” in 14th International Conference on Machine Learning and
#' Applications (IEEE ICMLA 15), 2015.
#'
#' @example tests/examples/getWindowLength_example.R
#'
#' @export

GetWindowLength <- function(data.length, num.real.anomaly, window.length.perc = 0.1) {
  windowLength <- floor((data.length * window.length.perc) / num.real.anomaly)
  if (windowLength %% 2 != 0) windowLength <- windowLength - 1
  return(windowLength)
}
