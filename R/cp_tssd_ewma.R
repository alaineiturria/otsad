#' Classic Processing Two-Stage Shift-Detection based on EWMA
#'
#' @description \code{CpTsSdEwma} calculates the anomalies of a dataset using
#' classical processing based on the SD-EWMA algorithm. This algorithm is a
#' novel method for covariate shift-detection tests based on a two-stage
#' structure for univariate time-series. This algorithm works in two phases. In
#' the first phase, it detects anomalies using the SD-EWMA
#' \code{\link{CpSdEwma}} algorithm. In the second phase, it checks the veracity
#' of the anomalies using the Kolmogorov-Simirnov test to reduce false alarms.
#' See also \code{\link{OcpTsSdEwma}}, the optimized and faster function of this
#' function.
#'
#' @param data Numerical vector with training and test dataset.
#' @param n.train Number of points of the dataset that correspond to the training set.
#' @param threshold Error smoothing constant.
#' @param l Control limit multiplier.
#' @param m Length of the subsequences for applying the Kolmogorov-Smirnov test.
#'
#' @details \code{data} must be a numerical vector without NA values.
#' \code{threshold} must be a numeric value between 0 and 1.
#' It is recommended to use low values such as 0.01 or 0.05. By default, 0.01 is
#' used. Finally, \code{l} is the parameter that determines the control limits.
#' By default, 3 is used. \code{m} is the length of the subsequences for
#' applying the Kolmogorov-Smirnov test. By default, 5 is used. It should be
#' noted that the last \code{m} values will not been verified because another
#' \code{m} values are needed to be able to perform the verification.
#'
#' @return dataset conformed by the following columns:
#'
#'   \item{is.anomaly}{1 if the value is anomalous, 0 otherwise.}
#'   \item{ucl}{Upper control limit.}
#'   \item{lcl}{Lower control limit.}
#'
#' @references Raza, H., Prasad, G., & Li, Y. (03 de 2015). EWMA model based
#' shift-detection methods for detecting covariate shifts in non-stationary
#' environments. Pattern Recognition, 48(3), 659-669.
#'
#' @example tests/examples/cp_tssd_ewma_example.R
#'
#' @export


CpTsSdEwma <- function(data, n.train, threshold, l = 3, m = 5) {

  # validate parameters
  if (!is.numeric(data) | (sum(is.na(data)) > 0)) {
    stop("data argument must be a numeric vector and without NA values.")
  }
  if (!is.numeric(n.train) | n.train > length(data)) {
    stop("n.train argument must be a numeric value greater then data length.")
  }
  if (!is.numeric(threshold) | threshold <= 0 |  threshold > 1) {
    stop("threshold argument must be a numeric value in (0,1] range.")
  }
  if (!is.numeric(l)) {
    stop("l argument must be a numeric value.")
  }
  if (!is.numeric(m) | m > length(data)) {
    stop("m argument must be a numeric value and smaller than all dataset length.")
  }

  # Auxiliar function to apply Kolmogorov Test
  ApplyKolmogorovTest <- function(pos) {
    if ((pos - (m - 1)) > 0 & (pos + m) <= length(data)) {
      part1 <- data[(pos - (m - 1)):pos]
      part2 <- data[(pos + 1):(pos + m)]
      res.test <- suppressWarnings(stats::ks.test(part1, part2, exact = FALSE))
      return(ifelse(res.test$p.value > 0.05, 0, 1))
    } else {
      return(1)
    }
  }

  result <- CpSdEwma(data, n.train, threshold, l)
  anomally.pos <- which(result$is.anomaly == 1)
  result[anomally.pos, "is.anomaly"] <- sapply(anomally.pos, ApplyKolmogorovTest)
  return(result)
}
