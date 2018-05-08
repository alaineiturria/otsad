#' Optimized Classic Processing Probabilistic-EWMA (PEWMA).
#'
#' @description \code{OcpPewma} calculates the anomalies of a data set using
#' optimized version of classical processing Probabilistic-EWMA algorithm.
#' Is an optimized implementation of the \code{\link{CpPewma}} algorithm using
#' environment variables. It has been shown that in long data sets it can
#' reduce runtime by up to 50\%. This algorithm is probabilistic method of EWMA
#' which dynamically adjusts the parameterization based on the probability of
#' the given observation. This method produces dynamic, data-driven anomaly
#' thresholds which are robust to abrupt transient changes, yet quickly adjust
#' to long-term distributional shifts. See also \code{\link{OcpPewma}} the
#' optimized and faster function of the same.
#'
#' @param data Numerical vector that conforms the training and test data set.
#' @param n.train Number of points of the data set that correspond to the
#' training set.
#' @param alpha0  Maximal weighting parameter.
#' @param beta Weight placed on the probability of the given observation.
#' @param l Control limit multiplier.
#'
#' @details \code{data} must be numerical vectors without NA values.
#' \code{alpha0} must be a numeric value where 0 < \code{alpha0} < 1. If a
#' faster adjustment to the initial shift is desirable, simply lowering α will
#' suffice. \code{beta} is the weight placed on the probability of the given
#' observation. it must be a numeric value where 0 \leq \code{beta} \leq. Note
#' that \code{beta} equals 0, PEWMA converges to a standard EWMA. Finally
#' \code{l} is the parameter that determines the control limits. By default, 3
#' is used.
#'
#' @return Data set conformed by the following columns:
#'
#'   \item{is.anomaly}{1 if the value is anomalous 0 otherwise.}
#'   \item{ucl}{Upper control limit.}
#'   \item{lcl}{Lower control limit.}
#'
#' @references M. Carter, Kevin y W. Streilein. Probabilistic reasoning for
#' streaming anomaly detection. 2012 IEEE Statistical Signal Processing Workshop
#' (SSP), pp. 377-380, Aug 2012.
#'
#' @example examples/ocp_pewma_example.R
#'
#' @export

OcpPewma <- function(data, alpha0 = 0.2, beta = 0, n.train = 5, l = 3) {

  # Pewma
  Pewma <- function(x, env) {
    row <- get("last.res", envir = env)
    row$i <- row$i + 1
    row$x <- x
    row$s1 <- row$s1.next
    row$std <- row$std.next
    row$z <- ifelse(row$std == 0, 0, (row$x - row$s1) / row$std)
    row$p <- 1 / sqrt(2 * pi)*exp(-(row$z ^ 2) / 2)
    row$alpha <- ifelse(row$i <= n.train, 1 - 1/row$i, (1 - beta * row$p) * alpha0)
    row$s1 <- row$alpha * row$s1 + (1 - row$alpha) * row$x
    row$s2 <- row$alpha * row$s2 + (1 - row$alpha) * row$x ^ 2
    row$s1.next <- row$s1
    row$std.next <- sqrt(abs(row$s2 - row$s1 ^ 2))
    row$ucl <- row$s1 + l[1] * row$std
    row$lcl <- row$s1 - l[1] * row$std
    row$is.anomaly <- row$x < row$lcl | row$x > row$ucl
    assign("last.res", row, env)
    return(row[c("is.anomaly", "ucl", "lcl")])
  }

  # inicializamos las variables
  new.enviroment <- new.env()
  last.res <- data.frame(value = data[1],
                         i = 0,
                         s1 = data[1],
                         s2 = data[1] ^ 2,
                         s1.next = data[1],
                         std.next = 0,
                         std = 0,
                         z = 0,
                         p = 0,
                         is.anomaly = 0,
                         lcl = 0,
                         ucl = 0,
                         alpha = alpha0)

  assign("last.res", last.res, envir = new.enviroment)
  res <- as.data.frame(t(sapply(data, Pewma, new.enviroment)))

  return(res)
}
