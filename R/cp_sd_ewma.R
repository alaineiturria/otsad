#' Classic Processing Shift-Detection based on EWMA (SD-EWMA).
#'
#' @description \code{CpSdEwma} calculates the anomalies of a dataset using
#' classical processing based on the SD-EWMA algorithm. This algorithm is a
#' novel method for covariate shift-detection tests based on a two-stage
#' structure for univariate time-series. It works in an online mode and it uses
#' an exponentially weighted moving average (EWMA) model based control chart to
#' detect the covariate shift-point in non-stationary time-series. See also
#' \code{\link{OcpSdEwma}}, the optimized and faster function of this function.
#'
#' @param data Numerical vector with training and test dataset.
#' @param n.train Number of points of the dataset that correspond to the training set.
#' @param threshold Error smoothing constant.
#' @param l Control limit multiplier.
#'
#' @details \code{data} must be a numerical vector without NA values.
#' \code{threshold} must be a numeric value between 0 and 1.
#' It is recommended to use low values such as 0.01 or 0.05. By default, 0.01 is
#' used. Finally, \code{l} is the parameter that determines the control limits.
#' By default, 3 is used.
#'
#' @return dataset conformed by the following columns:
#'
#'   \item{is.anomaly}{1 if the value is anomalous 0, otherwise.}
#'   \item{ucl}{Upper control limit.}
#'   \item{lcl}{Lower control limit.}
#'
#' @references Raza, H., Prasad, G., & Li, Y. (03 de 2015). EWMA model based
#' shift-detection methods for detecting covariate shifts in non-stationary
#' environments. Pattern Recognition, 48(3), 659-669.
#'
#' @example tests/examples/cp_sd_ewma_example.R
#'
#' @export


CpSdEwma <- function(data, n.train, threshold = 0.01, l = 3) {

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

  train.data <- data[1:n.train]
  test.data <- data[(n.train + 1):length(data)]

  # Auxiliar function SdEwma train phase
  SdEwmaTrain <- function(row, x) {
    row$x <- x
    row$i <- row$i + 1
    row$z.ant <- row$z
    row$std.ant <- row$std
    row$z <- row$lambda * row$x + (1-row$lambda) * row$z.ant
    row$error <- row$x - row$z.ant
    row$error.sum <- row$error.sum + row$error ^ 2
    row$std <- row$error.sum / row$i
    row
  }

  # Auxiliar function SdEwma test phase
  SdEwmaTest <- function(row, x) {
    row$i <- row$i + 1
    row$x <- x
    row$z.ant <- row$z
    row$std.ant <- row$std
    row$z <- row$lambda * row$x + (1 - row$lambda) * row$z.ant
    row$error <- row$x - row$z.ant
    row$std <- threshold * row$error ^ 2 + (1 - threshold) * row$std.ant
    row$ucl <- row$z.ant + l[1] * sqrt(row$std.ant)
    row$lcl <- row$z.ant - l[1] * sqrt(row$std.ant)
    row$is.anomaly <- row$x < row$lcl | row$x > row$ucl
    row
  }

  # Initialize the parameters
  lambdas <- seq(0.1, 1, 0.1)
  train.set <- data.frame(lambda = lambdas,
                         i = 1,
                         x = train.data[1],
                         z.ant = 0,
                         std.ant = 0,
                         z = mean(train.data),
                         error = 0,
                         std = 0,
                         error.sum = 0)

  # Training phase
  for (i in 1:length(train.data)) {
    train.set <- SdEwmaTrain(train.set, train.data[i])
  }

  # Testing phase
  res <- NULL
  last.res <- train.set[train.set$error.sum == min(train.set$error.sum),]
  for (i in 1:length(test.data)) {
    last.res <- SdEwmaTest(last.res, test.data[i])
    res <- rbind(res, last.res[,c("is.anomaly", "lcl", "ucl")])
  }

  v <- rep(0, n.train)
  tra <- data.frame(is.anomaly = v, lcl = train.data, ucl = train.data, stringsAsFactors = FALSE)
  res <- rbind(tra, res)

  return(res)
}

