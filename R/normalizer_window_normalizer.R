#' Window-based streaming normalizalizer
#'
#' @description
#' R6 class allows normalizing the data set in an online manner one-by-one or chunk-by-chunk. It
#' maintains a window with the last data values to calculate the statistical values needed for the
#' min-max normalization or the z-score standardization.
#'
#' @examples
#'
#' normalizer <- WindowNormalizer$new(wl = 3)
#'
#' normalizer$normalize(10)
#' normalizer$normalize(15)
#' normalizer$normalize(20)
#' normalizer$normalize(10)
#' normalizer$normalize(30)
#' normalizer$normalize(15)
#'
#' normalizer$denormalize(-0.3202563)
#'
#' @export

WindowNormalizer <- R6::R6Class("WindowNormalizer", cloneable = FALSE,

  public = list(

    #' @description Create a new WindowNormalizer object.
    #' @param wl Length of the window with historical data values.
    #' @param method Normalization method to be used "min-max" or "z-score".
    #' @param returnPoint If \code{FALSE} then normalized current window is returned, else,
    #' normalized current data point is returned.
    #' @return A new WindowNormalizer object.
    initialize = function(wl, method = "z-score", returnPoint = F) {

      if (!is.numeric(wl) | wl < 1) {
        stop("wl must be an integer greater than 0.")
      }
      if (!method %in% c("z-score", "max-min")) {
        stop("method must be 'min-max' or 'z-score'.")
      }
      if (!is.logical(returnPoint)) {
        stop("returnPoint must be logical.")
      }

      private$window <- Buffer$new(wl)
      private$method <- method
      private$returnPoint <- returnPoint
    },

    #' @description
    #' Normalizes the current data value.
    #' @param x Current data value to be normalized.
    #' @return If \code{returnPoint = FALSE} then normalized current window is returned, else,
    #' normalized current data point is returned.
    normalize = function(x) {

      private$window$add(x)

      if (private$window$length() > 1) {

        switch (private$method,
          "max-min" = {
            private$p1 <- max(private$window$get())
            private$p2 <- min(private$window$get())
            if (private$p1 == private$p2) {
              y <- private$window$get() * 0
            } else {
              y <- (private$window$get() - private$p2) / (private$p1 - private$p2)
            }
          },
          "z-score" = {
            private$p1 <- mean(private$window$get())
            private$p2 <- sd(private$window$get())
            if (private$p2 < 0.0003) private$p2 <- 0.0003
            y <- (private$window$get() - private$p1) / private$p2
          }
        )

      }
      else {
        y <- x
      }

      if (private$returnPoint) {
        return(y[length(y)])
      }
      else {
        return(y)
      }

    },

    #' @description
    #' Denormalizes the current data value.
    #' @param y Current data value to be denormalized.
    #' @return Denormalized current data point.
    denormalize = function(y) {

      switch (private$method,
        "max-min" = {
          x <- y * (private$p1 - private$p2) + private$p2
        },
        "z-score" = {
          x <- y * private$p2 + private$p1
        }
      )

      return(x)
    }

  ),
  private = list(
    p1 = NULL,
    p2 = NULL,
    window = NULL,
    method = NULL,
    returnPoint = NULL
  )
)
