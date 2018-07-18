#' Get Anomaly Score for Contextual Anomaly Detector
#'
#' \code{GetAnomalyScore.CAD} calculates the anomaly score of a
#' single datum using the notion of contexts conformed by facts and provides
#' probabilistic abnormality scores.
#'
#' @param datum Integer vector of activated bits.
#' @param local.environment Local environment with the variables of
#'     the Context Anomaly Detector.
#'
#' @details \code{datum} must be a numerical vector of the transformation made by \code{IntToBinarySens}. 
#'
#' @return Anomaly score of \code{datum}.
#'
#' @references Smirnov, M. (2018). CAD: Contextual Anomaly
#'     Detector. https://github.com/smirmik/CAD
#'
#' @export
GetAnomalyScore.CAD <- function(datum, local.environment){
  anomaly.values <- Step.CAD(datum, local.environment)
  current.anomaly.score <- (1 - anomaly.values[1] + anomaly.values[2]) / 2
  
  max.historic <- max(tail(local.environment$result.values.history,
                          local.environment$rest.period))
  returned.anomaly.score <- ifelse(max.historic < local.environment$base.threshold,
                                   current.anomaly.score, 0)
  local.environment$result.values.history <- c(local.environment$result.values.history,
                                              current.anomaly.score)
  return(returned.anomaly.score)
}
