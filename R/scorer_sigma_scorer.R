#' Sigma scorer
#'
#' @description
#' R6 class that allows computing the anomaly score online based on historic prediction errors and
#' 3-sigma control limits.
#'
#' @examples
#'
#' scorer <- SigmaScorer$new(3, 4)
#'
#' scorer$computeScore(0.5)
#' scorer$computeScore(0.6)
#' scorer$computeScore(0.5)
#' scorer$computeScore(1)
#' scorer$computeScore(0.2)
#'
#' @export

SigmaScorer <- R6::R6Class("SigmaScorer", cloneable = FALSE,

  public = list(

   #' @description Create a new SigmaScorer object.
   #' @param wnMin Minimum window size to start computing the anomaly score. It must be an integer
   #' between [1, \code{wnMax}].
   #' @param wnMax Maximum window size to compute the anomaly score. It must be an integer
   #' equal or bigger than \code{wnMin}.
   #' @param l Number of deviations from the mean. It must be an integer greater than 0. By default
   #' 3, but another common value is 6.
   #' @param computeLogLikelihood If \code{TRUE} compute a log scale representation of the
   #' likelihood value. Since thelikelihood computations return low probabilities that often go
   #' into four 9's or five 9's, a log value is more useful for visualization, thresholding, etc.
   #' @return A new SigmaScorer object.
   initialize = function(wnMin = 100, wnMax = 2000, l = 3, computeLogLikelihood = F) {

     if (!is.numeric(wnMin) | wnMin < 1 | wnMin > wnMax) {
        stop("wnMin must be an integer greater than 1 and less than or equal to wnMax.")
     }
     if (!is.numeric(wnMax) | wnMax < 1) {
        stop("wnMax must be an integer greater than 1 and greater than or equal to wnMin.")
     }
     if (!is.numeric(l) | l <= 0) {
        stop("l must be an integer greater than 0.")
     }
     if (!is.logical(computeLogLikelihood)) {
        stop("computeLogLikelihood must be logical.")
     }

     private$wnMin <- wnMin
     private$wnMax <- wnMax
     private$l <- l
     private$computeLogLikelihood <- computeLogLikelihood
   },

   #' @description Calculates the anomaly score from the prediction error.
   #' @param x Current error value to be scored.
   #' @param ... Any other parameter.
   #' @return Anomaly score, a value between [0,1]. The closer the number is
   #' to 1, the higher the chance it is an anomaly.
   computeScore = function(x, ...) {

     # Update Sliding Window
     private$w <- c(private$w, x)
     if (length(private$w) > private$wnMax)
       private$w <- private$w[-1]

     # Compute Anomaly Score
     if (length(private$w) < private$wnMin) {
       anomalyScore <- 0
     } else {
       std <- ifelse(sd(private$w) < 0.0003, 0.0003, sd(private$w))
       mu <- ifelse(mean(private$w) < 0.03, 0.03, mean(private$w))
       anomalyScore <- 1 - exp(-(log(2) / ((private$l * std) ^ 2)) * ((x - mu) ^ 2))
     }

     if(private$computeLogLikelihood) {
        anomalyScore <- log(1.0000000001 - anomalyScore) / -23.02585084720009
     }

     return(anomalyScore)
   }

  ),
  private = list(
    w = NULL,
    wnMin = NULL,
    wnMax = NULL,
    l = NULL,
    computeLogLikelihood = NULL
  )
)
