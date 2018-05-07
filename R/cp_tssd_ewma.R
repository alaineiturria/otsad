#' Classic Processing Two-Stage Shift-Detection based on EWMA
#'
#' @description \code{CpTsSdEwma} calculates the anomalies of a data set using
#' classical processing based on the SD-EWMA algorithm. This algorithm is a
#' novel method for covariate shift-detection tests based on a two-stage
#' structure for univariate time-series. This algorithm works in two phases. In
#' the first phase, it detects anomalies using the SD-EWMA
#' \code{\link{CpSdEwma}} algorithm. In the second phase, it checks the veracity
#' of the anomalies using the Kolmogorov-Simirnov test to reduce false alarms.
#' See also \code{\link{OcpTsSdEwma}} the optimized and faster function of the
#' same.
#'
#' @param train.data Numerical vector that conforms the training set.
#' @param test.data Numerical vector that conforms the test set.
#' @param threshold Error smoothing constant.
#' @param l Control limit multiplier.
#' @param m Length of the subsequences for applying the Kolmogorov-Smirnov test.
#'
#' @details \code{train.data} and \code{test.data} must be numerical vectors
#' without NA values. \code{threshold} must be a numeric value between 0 and 1.
#' It is recommended to use low values such as 0.01 or 0.05. By default, 0.01 is
#' used. Finally, \code{l} is the parameter that determines the control limits.
#' By default, 3 is used. \code{m} is the length of the subsequences for
#' applying the Kolmogorov-Smirnov test. By default, 5 is used. It should be
#' noted that the last m values have not been verified because you need other m
#' values to be able to perform the verification.
#'
#' @return Data set conformed by the following columns:
#'
#'   \item{is.anomaly}{1 if the value is anomalous 0 otherwise.}
#'   \item{ucl}{Upper control limit.}
#'   \item{lcl}{Lower control limit.}
#'
#' @references Raza, H., Prasad, G., & Li, Y. (03 de 2015). EWMA model based
#' shift-detection methods for detecting covariate shifts in non-stationary
#' environments. Pattern Recognition, 48(3), 659-669.
#'
#' @example examples/cp_tssd_ewma_example.R


CpTsSdEwma <- function(train.data, test.data, threshold, l = 3, m = 5) {

  ApplyKolmogorovTest <- function(pos, all.data) {
    if ((pos - (m - 1)) > 0 | (pos + m) <= length(all.data)) {
      part1 <- all.data[(pos - (m - 1)):pos]
      part2 <- all.data[(pos + 1):(pos + m)]
      res.test <- ks.test(part1, part2, exact = NULL)
      return(ifelse(res.test$p.value > 0.05, 0, 1))
    } else {
      return(1)
    }
  }

  result <- CpSdEwma(train.data, test.data, threshold, l)
  all.data <- c(train.data, test.data)
  anomally.pos <- which(result$is.anomaly == 1)
  result[anomally.pos, "is.anomaly"] <-
  sapply((anomally.pos + length(train.data)), ApplyKolmogorovTest, all.data)
  return(result)
}
