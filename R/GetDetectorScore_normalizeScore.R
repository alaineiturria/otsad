#' Normalize Score using Max and Min normalization
#'
#' @description \code{ReduceAnomalies} It reduces the number of detected anomalies. This function is
#' designed to reduce the number of false positives keeping only the first detection of all those
#' that are close to each other. This proximity distance is defined by a window
#'
#' @param real.score Detector score. See \code{\link{GetDetectorScore}}.
#' @param perfect.score Perfect detector score; one that outputs all true positives and no false
#' positives. See \code{\link{GetNullAndPerfectScores}}.
#' @param null.score Perfect detector score; one that outputs all true positives and no false
#' positives. See \code{\link{GetNullAndPerfectScores}}.
#'
#' @return Normalized score.
#'
#' @references A. Lavin and S. Ahmad, “Evaluating Real-time Anomaly Detection Algorithms – the
#' Numenta Anomaly Benchmark,” in 14th International Conference on Machine Learning and
#' Applications (IEEE ICMLA’15), 2015.
#'
#' @example tests/examples/normalizeScore_example.R
#'
#' @export

NormalizeScore <- function(real.score, perfect.score, null.score){

  if (!is.numeric(real.score) | length(real.score) > 1) {
    stop("n.train argument must be a numeric value.")
  }
  if (!is.numeric(perfect.score) | length(perfect.score) > 1) {
    stop("perfect.score argument must be a numeric value.")
  }
  if (!is.numeric(null.score) | length(null.score) > 1) {
    stop("null.score argument must be a numeric value.")
  }

  if (perfect.score == null.score) {
    res <- real.score
  } else {
    res <- 100 * (real.score - null.score) / (perfect.score - null.score)
  }
  return(res)
}
