#' Get Null And Perfect Scores
#'
#' @description \code{GetNullAndPerfectScores} Calculates the score of Perfect and Null
#' detectors scores. Perfect detector is one that outputs all true positives and no false
#' positives. And Null detector is one that outputs no anomaly detections.
#'
#' @param data All dataset with training and test datasets and with at least \code{timestamp},
#' \code{value} and \code{is.real.anomaly} columns.
#'
#' @details This function calculates the scores based on three different profiles. Each tp,
#' fp, tn, fn label is associated with a weight to give a more realistic score.
#' For the standard profile weights are tp = 1, tn = 1, fp, = 0.11, and fn = 1.
#' For the reward_low_FP_rate profile weights are tp = 1, tn = 1, fp, = 0.22, and fn = 1.
#' For the reward_low_FN_rate profile weights are tp = 1, tn = 1, fp, = 0.11, and fn = 2.
#'
#' @return data.frame with null and perfect detectors scores for each profile.
#'
#' @references A. Lavin and S. Ahmad, “Evaluating Real-time Anomaly Detection Algorithms – the
#' Numenta Anomaly Benchmark,” in 14th International Conference on Machine Learning and
#' Applications (IEEE ICMLA’15), 2015.
#'
#' @example tests/examples/getNullPerfectScores_example.R
#'
#' @export

GetNullAndPerfectScores <- function(data){

  data <- GetWindowsLimits(data)
  costs <- profiles

  perfect.score <- sum(data[data$is.real.anomaly == 1 & data$start.limit != 0, "is.real.anomaly"])
  null.data.standard <- - perfect.score * costs[costs$cost.type == "standard", "fn.weight"]
  null.data.low_FP <- - perfect.score * costs[costs$cost.type == "reward_low_FP_rate", "fn.weight"]
  null.data.low_FN <- - perfect.score * costs[costs$cost.type == "reward_low_FN_rate", "fn.weight"]

  cost.type <- c("standard", "reward_low_FP_rate", "reward_low_FN_rate")
  null.data <- c(null.data.standard, null.data.low_FP, null.data.low_FN)
  res <- data.frame(cost.type = cost.type, null.score = null.data, perfect.score = perfect.score,
                    stringsAsFactors = FALSE)

  return(res)

}
