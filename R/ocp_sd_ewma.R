#' Optimized Classic Processing Shift-Detection based on EWMA (SD-EWMA).
#'
#' @description \code{OcpSdEwma} calculates the anomalies of a data set using
#' optimized version of classical processing based on the SD-EWMA algorithm.
#' Is an optimized implementation of the \code{\link{CpSdEwma}} algorithm using
#' environment variables. It has been shown that in long data sets it can
#' reduce runtime by up to 50\%. SD-EWMA algorithm is a novel method for
#' covariate shift-detection tests based on a two-stage structure for univariate
#' time-series. It works in an online mode and it uses an exponentially weighted
#' moving average (EWMA) model based control chart to detect the covariate
#' shift-point in non-stationary time-series.
#'
#' @param train.data Numerical vector that conforms the training set.
#' @param test.data Numerical vector that conforms the test set.
#' @param threshold Error smoothing constant.
#' @param l Control limit multiplier.
#'
#' @details \code{train.data} and \code{test.data} must be numerical vecters
#' without NA values. \code{threshold} must be a numeric value between 0 and 1.
#' It is recommended to use low values such as 0.01 or 0.05. By default, 0.01 is
#' used. Finally, \code{l} is the parameter that determines the control limits.
#' By default, 3 is used.
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
#' @example examples/ocp_sd_ewma_example.R
#' @export

OcpSdEwma <- function(train.data, test.data, threshold, l = 3) {

  SdEwmaTrain <- function(x, env) {
    train.set <- get("last.res", envir = env)
    train.set$x <- x
    train.set$i <- train.set$i + 1
    train.set$z.ant <- train.set$z
    train.set$std.ant <- train.set$std
    train.set$z <- train.set$lambda * train.set$x +
                   (1 - train.set$lambda) * train.set$z.ant
    train.set$error <- train.set$x - train.set$z.ant
    train.set$error.sum <- train.set$error.sum + train.set$error ^ 2
    train.set$std <- train.set$error.sum  /train.set$i
    assign("last.res", train.set, env)
    row <- get("last.res", envir = env)
    train.set
  }

  SdEwmaTest <- function(x, env) {
    row <- get("last.res", envir = env)
    row$i <- row$i + 1
    row$x <- x
    row$z.ant <- row$z
    row$std.ant <- row$std
    row$z <- row$lambda * row$x + (1 - row$lambda) * row$z.ant
    row$error <- row$x - row$z.ant
    row$std <- threshold * row$error ^ 2 + (1 - threshold) * row$std.ant
    row$ucl <- row$z.ant + l[1] * sqrt(row$std.ant)
    row$lcl <- row$z.ant - l[1] * sqrt(row$std.ant)
    row$is.anomaly <- ifelse(row$x < row$lcl | row$x > row$ucl, 1, 0)
    assign("last.res", row, env)
    row[,c("is.anomaly", "ucl", "lcl")]
  }

  # Initialize the parameters
  new.enviroment <- new.env()
  lambdas <- seq(0.1, 1, 0.1)
  last.res <- data.frame(lambda = lambdas,
                        i = 1,
                        x = train.data[1],
                        z.ant = 0,
                        std.ant = 0,
                        z = mean(train.data),
                        error = 0,
                        std = 0,
                        error.sum = 0)
  assign("last.res", last.res, envir = new.enviroment)

  # Training phase
  sapply(train.data, SdEwmaTrain, new.enviroment)
  last.res <- get("last.res", envir = new.enviroment)
  last.res <- last.res[last.res$error.sum == min(last.res$error.sum),]
  assign("last.res", last.res, envir = new.enviroment)

  # Testing phase
  res <- as.data.frame(t(sapply(test.data, SdEwmaTest, new.enviroment)))
  res <- as.data.frame(lapply(res, unlist))

  return(res)
}
