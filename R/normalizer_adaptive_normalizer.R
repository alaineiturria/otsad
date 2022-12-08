#' One-pass Adaptive Normalizer
#'
#' @description
#' R6 class with our adaptation of the method proposed by Ogasawara et al. for one-pass online
#' time-series adaptive normalization. This method is designed to train neural networks and returns
#' the normalized train and test feature sets. In this proposal, the outlier elimination phase has
#' been omitted.
#'
#' @references E. Ogasawara, L. C. Martinez, D. De Oliveira, G. Zimbrão, G. L. Pappa,M. Mattoso,
#' Adaptive Normalization: A novel data normalization ap-proach for non-stationary time series,
#' in: Proceedings of the International Joint Conference on Neural Networks, 2010, pp. 1-8
#'
#' @examples
#'
#' normalizer <- AdaptiveNormalizer$new(3)
#'
#' normalizer$normalize(10)
#' normalizer$normalize(15)
#' normalizer$normalize(20)
#' normalizer$normalize(10)
#' normalizer$normalize(30)
#' normalizer$normalize(15)
#'
#' normalizer$denormalize(-0.4858841)
#'
#' @export

AdaptiveNormalizer <- R6::R6Class("AdaptiveNormalizer", cloneable = FALSE,

  public = list(

    #' @description Create a new AdaptiveNormalizer object.
    #' @param wl Length of the window with historical data values.
    #' @param maxmin If \code{TRUE} standardization and normalization are applied, else only
    #' standardization is applied.
    #' @param l Times IQR to anomaly removing. It must be a number greater than 0. By default 3,
    #' but other common values could be 1.5 and 6.
    #' @param returnPoint If \code{FALSE} normalized then the current window is returned, else, the
    #'  normalized current data point is returned.
    #' @return A new AdaptiveNormalizer object.
    initialize = function(wl = 10, maxmin = T, l = 3, returnPoint = F) {

      if (!is.numeric(wl) | wl < 1) {
        stop("wl must be an integer greater than 0.")
      }
      if (!is.logical(maxmin)) {
        stop("maxmin must be logical.")
      }
      if (!is.numeric(l) | l < 0) {
        stop("l must be an integer greater than 0.")
      }
      if (!is.logical(returnPoint)) {
        stop("returnPoint must be logical.")
      }

      private$w <- wl
      private$n <- wl + 1
      private$ks <- 1:ceiling(private$n * 0.1)
      print(private$ks)
      private$maxmin <- maxmin
      private$returnPoint <- returnPoint
      private$l <- 3

      # private$w <- 6
      # private$n <- 13
      # private$ks <- 1:5
      # private$maxmin <- TRUE
      # private$returnPoint <- FALSE
      # private$l <- 1.5
    },

    #' @description
    #' Normalizes the current data value.
    #' @param x Current data value to be normalized.
    #' @return If \code{returnPoint = FALSE} then normalized current train and test windows are
    #' returned, else, normalized current data point is returned.
    normalize = function(x) {

      private$s <- c(private$s, x)

      if (length(private$s) < private$w) {
        return(0)
      }
      else if (length(private$s) == private$w) {
        private$initializeTrain()
        return(0)
      }
      else {

        # Add new ss and se vlaues
        nks <- length(private$ks)
        private$ss <- rbind(private$ss, rep(NA, nks))
        private$se <- rbind(private$se, rep(NA, nks))
        iant <- inew <- cbind(nrow(private$ss) - private$ks, private$ks)
        inew[,1] <- inew[,1] + 1

        # SMA exponential moving average
        private$ss[inew] <<- sapply(1:nks, function(i) {
          mean(private$s[inew[i,1]:(inew[i,1] + inew[i,2] - 1)])
        })

        # EMA exponential moving average
        alpha <- round(2 / (private$ks + 1), 3)
        private$se[inew] <<- (1 - alpha) * private$se[iant] + alpha * private$s[iant[,1] + private$ks]

        if (length(private$s) >= private$n) {

          if (length(private$s) > private$n) {
            # Remove previous s, ss, se values
            private$s <- private$s[-1]
            private$ss <- matrix(private$ss[-1,], ncol = nks)
            private$se <- matrix(private$se[-1,], ncol = nks)
          }

          # Adjustment level
          n.train <- length(private$s) - private$w
          adjs <- t(sapply(1:nks, function(j) {

            adjss <- mean(sapply(1:n.train, function(i) {
              mean(private$s[i:(i + private$w - 1)] - private$ss[i, j]) ^ 2
            }))

            adjse <- mean(sapply(1:n.train, function(i) {
              mean(private$s[i:(i + private$w - 1)] - private$se[i, j]) ^ 2
            }))

            return(c(adjss, adjse))

          }))

          # Best adjudstement params
          if (min(adjs[,1]) < min(adjs[,1])) {
            s_aux <- private$ss
            j <- which.min(adjs[,1])
            # print("ss")
          }
          else {
            s_aux <- private$se
            j <- which.min(adjs[,2])
            # print("se")
          }

          # R matrix
          s_aux[s_aux < 0.03] <- 0.03
          matrixR <- t(sapply(1:(n.train + 1), function(i) {
            private$s[i + (0:(private$w - 1))] / s_aux[i, j]
          }))

          if (private$maxmin) {

            v <- round(as.vector(matrixR[1:(n.train + 1),]),3)
            qs <- round(quantile(v),3)
            q1 <- qs[2]
            q3 <- qs[4]
            iqr <- ifelse(q1 == q3, 1, q3 - q1)
            private$globalMin <- max(q1 - private$l * iqr, min(v))
            private$globalMax <- min(q3 + private$l * iqr, max(v))

            # private$globalMin <- 0.981

            private$bestS <- s_aux[(n.train + 1), j]
            if (private$globalMax == private$globalMin) {
              normR <- matrix(private$globalMax, nrow = (n.train + 1), ncol = private$w)
            } else {
              normR <- 2 * (matrixR - private$globalMin) / (private$globalMax - private$globalMin) - 1
              normR[normR < -1] <- -1
              normR[normR > 1] <- 1

            }
          }
          else {
            normR <- matrixR
          }

          if (private$returnPoint) {
            return(normR[nrow(normR), ncol(normR)])
          } else {
            return(normR)
          }
        }
        else{
          return(0)
        }

      }
    },

    #' @description
    #' Denormalizes the current data value.
    #' @param y Current data value to be denormalized.
    #' @return Denormalized current data point.
    denormalize = function(y) {
      n.train <- private$n - private$w + 1
      denorm <- ((y + 1) / 2) * (private$globalMax - private$globalMin) + private$globalMin
      return(denorm * private$bestS)
    }

  ),
  private = list(
    s = NULL, # raw time serie
    w = NULL, # window size
    ks = NULL, # k orders
    n = NULL, # size of s
    ss = NULL, # matrix with k-SMAs of s
    se = NULL, # matrix with k-EMAs of s
    globalMax = NA,
    globalMin = NA,
    bestR = NA,
    bestS = NA,
    maxmin = NULL,
    returnPoint = F,
    l = NULL,

    initializeTrain = function() {

      alpha <- round(2 / (private$ks + 1), 3)
      maxk <- max(private$ks)

      # SMA and EMA
      ss <- matrix(data = NA, nrow = private$w, ncol = maxk)
      ss[1,] <- (1 / private$ks) * cumsum(private$s[1:maxk])
      se <- ss
      sapply(2:private$w, function(i){
        ix <- sum((i + private$ks - 1) <= private$n)
        ss[i,1:ix] <<- round((1 / private$ks[1:ix]) * cumsum(private$s[i:(i + private$ks[ix] - 1)]), 3)
        se[i,] <<- round((1 - alpha) * se[i - 1,] + alpha * private$s[i + private$ks - 1], 3)
      })

      private$ss <- matrix(ss, ncol = length(private$ks))
      private$se <- matrix(se, ncol = length(private$ks))

    }
  )
)


# s <- c(1.734, 1.720, 1.707, 1.708, 1.735, 1.746, 1.744, 1.759, 1.751, 1.749, 1.763, 1.753, 1.774)
# normalizer <- AdaptiveNormalizer$new()
# normalizer$normalize(s[1])
# normalizer$normalize(s[2])
# normalizer$normalize(s[3])
# normalizer$normalize(s[4])
# normalizer$normalize(s[5])
# normalizer$normalize(s[6])
# normalizer$normalize(s[7])
# normalizer$normalize(s[8])
# normalizer$normalize(s[9])
# normalizer$normalize(s[10])
# normalizer$normalize(s[11])
# normalizer$normalize(s[12])
# normalizer$normalize(s[13])

