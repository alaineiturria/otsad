#' Contextual Anomaly Detector - Open Source (CAD)
#'
#' \code{ContextualAnomalyDetector} calculates the anomaly score of a
#' dataset using the notion of contexts conformed by facts and provides
#' probabilistic abnormality scores.
#'
#' @param data Numerical vector with training and test dataset.
#' @param rest.period Training period after an anomaly.
#' @param max.left.semicontexts Number of semicontexts that should be maintained in memory.
#' @param max.active.neurons Number of neurons of the model.
#' @param num.norm.value.bits Granularity of the transformation into discrete values
#' @param base.threshold Threshold to be considered an anomaly.
#'
#' @details \code{data} must be a numerical vector without NA values.
#' \code{threshold} must be a numeric value between 0 and 1. If the anomaly
#' score obtained for an observation is greater than the \code{threshold}, the
#' observation will be considered abnormal.
#'
#' @return Anomaly score of \code{data}.
#'
#' @references Smirnov, M. (2018). CAD: Contextual Anomaly
#'     Detector. https://github.com/smirmik/CAD
#'
#' @export
ContextualAnomalyDetector <- function(data,
                                     rest.period = max(min(150, round(nrow(data) * 0.03), 1)),
                                     max.left.semicontexts = 7,
                                     max.active.neurons = 15,
                                     num.norm.value.bits = 3,
                                     base.threshold = 0.75,
                                     max.value = max(data, na.rm = T),
                                     min.value = min(data, na.rm = T)){

    # Load Python modules
    reticulate::source_python("inst/CAD/contextOperator.py")
    reticulate::source_python("inst/CAD/CAD_OSE.py")

    python.object <- CAD_OSE(min.value, max.value, base.threshold, rest.period,
                            max.left.semicontexts, max.active.neurons,
                            num.norm.value.bits)

    anomaly.score <- numeric(length = length(data))

    for(i in 1:length(data)){
        anomaly.score[i] <- python.object$getAnomalyScore(data[i])
    }

    return(list("Anomaly.Score" = anomaly.score,
                "Python.Object" = python.object))
}
