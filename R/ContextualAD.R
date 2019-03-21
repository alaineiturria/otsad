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
#' @param min.value Minimum expected value.
#' @param max.value Maximum expected value.
#' @param python.object Python object for incremental processing.
#' @param lib 0 to run the original python script, 1 to get the same results on all operating
#' systems.
#'
#' @details \code{data} must be a numerical vector without NA values.
#' \code{threshold} must be a numeric value between 0 and 1. If the anomaly
#' score obtained for an observation is greater than the \code{threshold}, the
#' observation will be considered abnormal. Requires hashlib and bencode python libraries.
#'
#' @return List
#'    \item{result}{Data frame with \code{anomaly.score} and \code{is.anomaly} comparing the anomaly
#'     score with \code{base.threshold}.}
#'    \item{python.object}{ContextualAnomalyDetector Python object used in online anomaly detection}
#'
#' @references Smirnov, M. (2018). CAD: Contextual Anomaly
#'     Detector. https://github.com/smirmik/CAD
#'
#'
#' @example tests/examples/contextual_ad_example.R
#'
#' @export
ContextualAnomalyDetector <- function(data,
                                      rest.period = max(min(150, round(length(data) * 0.03), 1)),
                                      max.left.semicontexts = 7,
                                      max.active.neurons = 15,
                                      num.norm.value.bits = 3,
                                      base.threshold = 0.75,
                                      min.value = min(data, na.rm = T),
                                      max.value = max(data, na.rm = T),
                                      python.object = NULL,
                                      lib = 0){
  if (is.null(python.object)){
    version <- reticulate::py_discover_config()$version_string
    version.number <- as.numeric(substring(version, 1, 1))
    if(version.number == 3) {
      CAD_OSE <- reticulate::import_from_path("cad_ose3",
                                              system.file("python", "CAD",
                                                          package = utils::packageName(),
                                                          mustWork = TRUE))
    } else {
      CAD_OSE <- reticulate::import_from_path("CAD_OSE",
                                              system.file("python", "CAD",
                                                          package = utils::packageName(),
                                                          mustWork = TRUE))
    }
    python.object <- CAD_OSE$ContextualAnomalyDetectorOSE(min.value, max.value, base.threshold, rest.period,
                                     max.left.semicontexts, max.active.neurons,
                                     num.norm.value.bits, lib)
  }

  anomaly.score <- numeric(length = length(data))

  for(i in 1:length(data)){
    anomaly.score[i] <- python.object$getAnomalyScore(data[i])
  }

  return(list("result" = cbind(anomaly.score = anomaly.score, is.anomaly = anomaly.score > base.threshold),
              "python.Object" = python.object))
}
