#' Get Number of Training Values
#'
#' @description \code{GetNumTrainingValues} Calculates the number of values to be used as a
#' training set.
#'
#' @param n.row Number of rows of the all dataset with training and test values.
#' @param prob.percent Percentage of training values
#'
#' @details the number of values to be used as a training set is calculated as a minimum
#' between 15\% of the number of rows in the dataset and 15\% of 5000.
#'
#' @return Number of training values.
#'
#' @references A. Lavin and S. Ahmad, “Evaluating Real-time Anomaly Detection Algorithms – the
#' Numenta Anomaly Benchmark,” in 14th International Conference on Machine Learning and
#' Applications (IEEE ICMLA’15), 2015.
#'
#' @example tests/examples/getNumTrainingValues_example.R
#'
#' @export

GetNumTrainingValues <- function(n.row, prob.percent = 0.15) {
  return(min(floor(n.row * prob.percent), floor(prob.percent * 5000)))
}
