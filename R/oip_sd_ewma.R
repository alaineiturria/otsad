#' Optimized Incremental Processing Shift-Detection based on EWMA (SD-EWMA).
#'
#' @description \code{OipSdEwma} is the optimized implementation of the
#' \code{IpSdEwma} function using environment variables. This function  allows
#' you to calculate anomalies using SD-EWMA alogrithm in an incremental
#' processing mode. It has been shown that in long data sets it can reduce
#' runtime by up to 50\%. SD-EWMA algorithm is a novel method for covariate
#' shift-detection tests based on a two-stage structure for univariate
#' time-series. It works in an online mode and it uses an exponentially weighted
#' moving average (EWMA) model based control chart to detect the covariate
#' shift-point in non-stationary time-series.
#'
#' @param data Numerical vector that conforms the training and test data set.
#' @param n.train Number of points of the data set that correspond to the
#' training set.
#' @param threshold Error smoothing constant.
#' @param l Control limit multiplier.
#' @param last.res Last result returned by the algorithm.
#'
#' @details \code{data} must be numerical vectors without NA values.
#' \code{threshold} must be a numeric value between 0 and 1. It is recommended
#' to use low values such as 0.01 or 0.05. By default, 0.01 is used. \code{l} is
#' the parameter that determines the control limits. By default, 3 is used.
#' Finally \code{last.res} is the last result returned by some previous
#' execution of this algorithm. The first time the algorithm is executed its
#' value is NULL. However, if you want to run a new batch of data without having
#' to include it in the old data set and restart the process you only need to
#' add the last results returned by the last run.
#'
#' This algorithm can be used for both classical and incremental processing. It
#' should be noted that in case of having a finite data set the
#' \code{\link{CpSdEwma}} or \code{\link{OcpSdEwma}} algorithms are faster.
#' Incremental processing can be used in two ways. 1) Processing all available
#' data and saving \code{last.res} for future runs in which you have new data.
#' 2) Using the \href{https://CRAN.R-project.org/package=stream}{stream} library
#' for when you have too much data and it does not fit into memory. An example
#' has been made for this use case.
#'
#' @return A list of the following items.
#'
#'   \item{result}{Data set conformed by the following columns.}
#'   \itemize{
#'      \item \code{is.anomaly} 1 if the value is anomalous 0 otherwise.
#'      \item \code{ucl} Upper control limit.
#'      \item \code{lcl} Lower control limit.
#'  }
#'  \item{last.res}{Last result returned by the algorithm. Is a data set
#'  containing the parameters calculated in the last iteration and necessary
#'  for the next one.}
#'
#' @references Raza, H., Prasad, G., & Li, Y. (03 de 2015). EWMA model based
#' shift-detection methods for detecting covariate shifts in non-stationary
#' environments. Pattern Recognition, 48(3), 659-669.
#'
#' @example examples/oip_sd_ewma_example.R

OipSdEwma <- function(data, n.train, threshold, l = 3, last.res = NULL) {

  SdEwmaTrain <- function(x, env) {
    train.set <- get("last.res", envir = env)
    train.set$x <- x
    train.set$i <- train.set$i + 1
    train.set$z.ant <- train.set$z
    train.set$std.ant <- train.set$std
    train.set$z <-
    train.set$lambda * train.set$x + (1 - train.set$lambda) * train.set$z.ant
    train.set$error <- train.set$x - train.set$z.ant
    train.set$error.sum <- train.set$error.sum + train.set$error ^ 2
    train.set$std <- train.set$error.sum / train.set$i
    assign("last.res", train.set, env)
    return (NULL)
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
    return (row[,c("is.anomaly", "lcl", "ucl")])
  }

  # Initialize the parameters
  new.enviroment <- new.env()
  lambdas <- seq(0.1, 1, 0.1)
  if (is.null(last.res)) {
    last.res <- data.frame(lambda = lambdas,
                          i = 0,
                          x = data[1],
                          z.ant = 0,
                          std.ant = 0,
                          z = data[1],
                          error = 0,
                          std = 0,
                          error.sum = 0)
  }
  assign("last.res", last.res, envir = new.enviroment)

  # prepare train and test
  n <- length(data)
  aux <- n.train - unique(last.res$i)
  if (n <= aux) {
    train.data <- data
    test.data <- NULL
  } else if (aux <= 0) {
    train.data <- NULL
    test.data <- data
  } else {
    train.data <- data[1:aux]
    test.data <- data[(aux + 1):n]
  }


  # Training phase
  if (!is.null(train.data)) {
    sapply(train.data, SdEwmaTrain, new.enviroment)
    res <- NULL
  }

  last.res <- get("last.res", envir = new.enviroment)
  if (unique(last.res$i) == n.train) {
    last.res <- last.res[last.res$error.sum == min(last.res$error.sum),]
    assign("last.res", last.res, envir = new.enviroment)
  }

  # Testing phase
  if (!is.null(test.data)) {
    res <- as.data.frame(t(sapply(test.data, SdEwmaTest, new.enviroment)))
    res <- data.frame(is.anomaly = unlist(res$is.anomaly),
                      lcl = unlist(res$lcl), ucl = unlist(res$ucl))
  }

  last.res <- get("last.res", envir = new.enviroment)
  return(list(result = res, last.res = last.res))
}
