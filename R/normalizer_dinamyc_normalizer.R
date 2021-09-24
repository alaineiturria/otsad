#' Dinamyc normalizer
#'
#' @description
#' R6 class that implements the unsupervised dynamic z-score standardization proposed by Bollegala.
#' This method allows normalizing the data sets in an online manner, by z-score standardization,
#' dynamically updating the mean and the variance whenever a new data arrives.
#'
#' @references D. Bollegala, Dynamic feature scaling for online learning of binary classifiers,
#' Knowledge-Based Syst., vol. 129, pp. 97–105, 2017.
#'
#' @examples
#'
#' normalizer <- DinamycNormalizer$new()
#'
#' normalizer$normalize(10)
#' normalizer$normalize(15)
#' normalizer$normalize(20)
#' normalizer$normalize(10)
#' normalizer$normalize(30)
#' normalizer$normalize(15)
#'
#' normalizer$denormalize(-0.2214036)
#'
#' @export

DinamycNormalizer <- R6::R6Class("DinamycNormalizer", cloneable = FALSE,

  public = list(

    #' @description Create a new DinamycNormalizer object.
    #' @return A new DinamycNormalizer object.
    initialize = function() {},

    #' @description Normalizes the current data value.
    #' @param x Current data value to be normalized.
    #' @return If \code{return_point = FALSE} then normalized current window is returned, else,
    #' normalized current data point is returned.
    normalize = function(x) {

      private$iter <- private$iter + 1
      if (is.null(private$mu)) {
        private$mu <- x
        normalized <- x
      }
      else {
        mu_ant <- private$mu
        private$mu <- mu_ant + (x - mu_ant) / private$iter
        private$s <- private$s + (x - mu_ant) * (x - private$mu)
        private$sigma <- sqrt(private$s / (private$iter - 1))
        private$sigma <- ifelse(private$sigma < 0.0003, 0.0003, private$sigma)
        normalized <- (x - private$mu) / private$sigma
      }

      return(normalized)

    },

    #' @description
    #' Denormalizes the current data value.
    #' @param y Current data value to be denormalized.
    #' @return Denormalized current data point.
    denormalize = function(y) {
      x <- y * private$sigma + private$mu
      return(x)
    }

  ),
  private = list(
    mu = NULL,
    s = 0.0003,
    sigma = 0.0003,
    iter = 0
  )
)
