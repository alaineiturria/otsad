#' One-pass adaptive min-max normalizalizer
#'
#' @description
#' R6 class that implements our adaptation to one-pass online processing of the adaptive
#' normalization method proposed by Gupta and Hewett. A sliding window of past \code{w} data values
#' are maintained and the percentage change in mean values between the current and previous window
#' is used to determine if the new or old min-max values should be used to normalize the current
#' window. If the percentage change value exceeds the threshold, then old min-max values are
#' replaced by the new ones.
#'
#' @references V. Gupta, R. Hewett, Adaptive Normalization in Streaming Data, in:Proceedings of
#' the 2019 3rd International Conference on Big Data Re-search, ACM, New York, NY, USA, 2019,
#' pp. 12-17.
#'
#' @examples
#'
#' normalizer <- AdaptiveNormalizer2$new(wl = 3)
#'
#' normalizer$normalize(10)
#' normalizer$normalize(15)
#' normalizer$normalize(20)
#' normalizer$normalize(10)
#' normalizer$normalize(30)
#' normalizer$normalize(15)
#'
#' normalizer$denormalize(0.25)
#'
#' @export

AdaptiveNormalizer2 <- R6::R6Class("AdaptiveNormalizer2", cloneable = FALSE,

  public = list(

    #' @description Create a new AdaptiveNormalizalizer object.
    #' @param wl Length of the window with historical data values.
    #' @param threshold Dissimilarity factor between the previous and current windows mean to
    #' calculate new min and max values.
    #' @param returnPoint If \code{FALSE} then normalized current window is returned, else,
    #' the normalized current data point is returned.
    #' @return A new `AdaptiveNormalizer2` object.
    initialize = function(wl, threshold = 0.25, returnPoint = F) {

      if (!is.numeric(wl) | wl < 1) {
        stop("wl must be an integer greater than 0.")
      }
      if (!is.numeric(threshold) | threshold <= 0) {
        stop("threshold must be an integer equal or greater than 0.")
      }
      if (!is.logical(returnPoint)) {
        stop("returnPoint must be logical.")
      }

      private$window <- Buffer$new(wl)
      private$threshold <- threshold
      private$returnPoint <- returnPoint
    },

    #' @description Normalizes the current data value.
    #' @param x Current data value to be normalized.
    #' @return If \code{returnPoint = FALSE} then normalized current window is returned, else,
    #' normalized current data point is returned.
    normalize = function(x) {

      private$window$add(x)

      if (private$window$full()) {

        if (is.null(private$refmin) | is.null(private$refmax)) {
          private$refmin <- min(private$window$get())
          private$refmax <- min(private$window$get())
          private$mu <- mean(private$window$get())
        }
        else {

          if (private$mu < 0.003) private$mu <- 0.003

          curr_mu <- mean(private$window$get())
          if (curr_mu < 0.003) curr_mu <- 0.003

          diference <- abs(curr_mu - private$mu) / private$mu

          if (diference > private$threshold) {
            private$refmin <- min(private$window$get())
            private$refmax <- max(private$window$get())
          } else {
            private$refmin <- min(min(private$window$get()), private$refmin)
            private$refmax <- max(max(private$window$get()), private$refmax)
          }

          private$mu <- curr_mu
        }

        if (private$refmax - private$refmin == 0)
          normalized <- private$window$get() * 0
        else
          normalized <- (private$window$get() - private$refmin) / (private$refmax - private$refmin)

        if (private$returnPoint) {
          return(normalized[length(normalized)])
        }
        else {
          return(normalized)
        }

      }
      else {
        return(0)
      }

    },

    #' @description Denormalizes the current data value.
    #' @param y Current data value to be denormalized.
    #' @return Denormalized current data point.
    denormalize = function(y) {
      x <- y * (private$refmax - private$refmin) + private$refmin
      return(x)
    }
  ),
  private = list(
    window = NULL,
    threshold = NULL,
    refmin = NULL,
    refmax = NULL,
    mu = 0,
    returnPoint = NULL
  )
)
