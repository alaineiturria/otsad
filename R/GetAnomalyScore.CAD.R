GetAnomalyScore.CAD <- function(datum, local.environment){
  anomaly.values <- Step.CAD(datum, local.environment)
  current.anomaly.score <- (1 - anomaly.values[1] + anomaly.values[2]) / 2
  
  result.values.history <- get("result.values.history", envir = local.environment)
  rest.period <- get("rest.period", envir = local.environment)
  base.threshold <- get("base.threshold", envir = local.environment)
  max.historic <- max(tail(result.values.history, rest.period))
  returned.anomaly.score <- ifelse(max.historic < base.threshold,
                                   current.anomaly.score, 0)
  local.environment$result.values.history <- c(result.values.history,current.anomaly.score)
  return(returned.anomaly.score)
}
