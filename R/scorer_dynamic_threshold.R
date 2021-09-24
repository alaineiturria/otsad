#' Dynamic Threshold Scorer
#'
#' @description
#' R6 class that implements the online anomaly scoring method (based on the prediction errors)
#' proposed by Buda et al.
#'
#' @references T. S. Buda, B. Caglayan, H. Assem,  DeepAD: A generic frameworkbased on deep
#' learning for time series anomaly detection, in: LectureNotes in Computer Science
#' (including subseries Lecture Notes in Artificial Intelligence and Lecture Notes in
#' Bioinformatics), volume 10937LNAI, Springer Verlag, 2018, pp. 577-588
#'
#' @examples
#'
#' scorer <- DynamicThresholdScorer$new(3)
#'
#' scorer$computeScore(0.5)
#' scorer$computeScore(0.6)
#' scorer$computeScore(0.5)
#' scorer$computeScore(7)
#' scorer$computeScore(0.2)
#'
#' @export

DynamicThresholdScorer <- R6::R6Class("DynamicThresholdScorer", cloneable = FALSE,

  public = list(

    #' @description Create a new DynamicThresholdScorer object.
    #' @param wnMin Minimum window size to start computing the anomaly score. It must be an integer
    #' between [1, \code{wnMax}].
    #' @param wnMax Maximum window size to compute the anomaly score. It must be an integer
    #' equal or bigger than \code{wnMin}.
    #' @param l Times standard deviation. It must be an integer greater than 0. By default 10, but
    #' other common values could be 3 and 6.
    #' @return A new DynamicThresholdScorer object.
    initialize = function(wnMin = 100, wnMax = 2000, l = 10) {

      if (!is.numeric(wnMin) | wnMin < 1 | wnMin > wnMax) {
        stop("wnMin must be an integer greater than 1 and less than or equal to wnMax.")
      }
      if (!is.numeric(wnMax) | wnMax < 1) {
        stop("wnMax must be an integer greater than 1 and greater than or equal to wnMin.")
      }
      if (!is.numeric(l) | l < 0) {
        stop("l must be an integer greater than 0.")
      }

      private$w <- Buffer$new(wnMax)
      private$wnMin <- wnMin
      private$l <- l
    },

    #' @description Calculates the anomaly score from the prediction error.
    #' @param x Current error value to be scored.
    #' @param ... Any other parameter
    #' @return Anomaly score, a value between [0,1]. The closer the number is
    #' to 1, the higher the chance it is an anomaly.
    computeScore = function(x, ...) {


      if (private$w$length() < private$wnMin) {
        anomalyScore <- 0
        private$w$add(x)
      }
      else {

        mn <- min(private$w$get())
        mx <- max(private$w$get())

        if (mx == mn) {
          scaledErrors <- 0 * private$w$get()
          crtScaledError <- -1
        }
        else {
          scaledErrors <- (private$w$get() - mn) / (mx - mn)
          crtScaledError <- (x - mn) / (mx - mn)
        }

        dynamicThresh <- private$l * sd(scaledErrors)

        # print(paste0("dynamicThresh: ", dynamicThresh, " | crtScaledError: ", crtScaledError))

        if (crtScaledError >= dynamicThresh) {
          anomalyScore <- 1
        } else {
          private$w$add(x)
          anomalyScore <- 0
        }

      }

      return(anomalyScore)
    }

  ),
  private = list(
    w = NULL,
    wnMin = NULL,
    l = NULL
  )
)
