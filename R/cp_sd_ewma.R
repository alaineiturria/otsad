#' Classic Processing Shift-Detection based on EWMA (SD-EWMA).
#'
#' \code{CpSdEwma} Calculates the anomalies of a data set using classical
#' processing based on the SD-EWMA algorithm.
#'
#' @param train.data Numerical vector that conforms the training set.
#' @param test.data Numerical vector that conforms the test set.
#' @param threshold Error threshold.
#' @param l Sigma multiplier to calculate the control limits.
#'
#' @details bla bla bla
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
#' @example examples/cp_sd_ewma_example.R


CpSdEwma <- function(train.data, test.data, threshold = 0.01, l = 3) {

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

  return(res)
}

