#' Dynamic Sigma Scorer
#'
#' @description
#' R6 class that allows computing the anomaly score online based on the prediction errors and
#' 3-sigma control limits. The mean and standard deviations for 3-sima control limits are
#' dynamically computed whenever new data arrives.
#'
#' @examples
#'
#' scorer <- DynamicSigmaScorer$new()
#'
#' scorer$computeScore(0.5)
#' scorer$computeScore(0.6)
#' scorer$computeScore(0.5)
#' scorer$computeScore(7)
#' scorer$computeScore(0.2)
#'
#' @export

DynamicSigmaScorer <- R6::R6Class("DynamicSigmaScorer", cloneable = FALSE,

  public = list(

    #' @description Create a new DynamicSigmaScorer object.
    #' @param l Number of deviations from the mean.
    #' @param computeLogLikelihood If \code{TRUE} compute a log scale representation of the
    #' likelihood value. Since thelikelihood computations return low probabilities that often go
    #' into four 9's or five 9's, a log value is more useful for visualization, thresholding, etc.
    #' @return A new DynamicSigmaScorer object.
    initialize = function(l = 3, computeLogLikelihood = F) {

      if (!is.numeric(l) | l < 0) {
        stop("l must be an integer greater than 0.")
      }
      if (!is.logical(computeLogLikelihood)) {
        stop("computeLogLikelihood must be logical.")
      }

      private$l <- l
      private$computeLogLikelihood <- computeLogLikelihood
    },

    #' @description Calculates the anomaly score from the prediction error.
    #' @param x Current error value to be scored.
    #' @param ... Any other parameter.
    #' @return Anomaly score, a value between [0,1]. The closer the number is
    #' to 1, the higher the chance it is an anomaly.
    computeScore = function(x, ...) {

      private$iter <- private$iter + 1
      if (is.null(private$mu)) {
        private$mu <- x
        sigma <- 0.0003
      }
      else {
        mu_ant <- private$mu
        private$mu <- mu_ant + (x - mu_ant) / private$iter
        private$s <- private$s + (x - mu_ant) * (x - private$mu)
        sigma <- sqrt(private$s / (private$iter - 1))
        sigma <- ifelse(sigma < 0.0003, 0.0003, sigma)
      }

      # Compute Anomaly Score
      anomalyScore <- 1 - exp(-(log(2) / ((private$l * sigma) ^ 2)) * ((x - private$mu) ^ 2))

      if(private$computeLogLikelihood) {
        anomalyScore <- log(1.0000000001 - anomalyScore) / -23.02585084720009
      }

      return(anomalyScore)
    }

  ),
  private = list(
    mu = NULL,
    s = 0.0003,
    iter = 0,
    l = NULL,
    computeLogLikelihood = NULL
  )
)
