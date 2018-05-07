#' Incremental Processing Probabilistic-EWMA (PEWMA).
#'
#' @description \code{IpPewma} allows you to calculate anomalies using PEWMA in
#' an incremental processing mode. See also \code{\link{OipPewma}} the optimized
#' and faster function of the same. This algorithm is probabilistic method of
#' EWMA which dynamically adjusts the parameterization based on the probability
#' of the given observation. This method produces dynamic, data-driven anomaly
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
#' @param last.res Last result returned by the algorithm.
#'
#' @details \code{data} must be numerical vectors without NA values.
#' \code{alpha0} must be a numeric value where 0 < \code{alpha0} < 1. If a
#' faster adjustment to the initial shift is desirable, simply lowering α will
#' suffice. \code{beta} is the weight placed on the probability of the given
#' observation. it must be a numeric value where 0 \leq \code{beta} \leq. Note
#' that \code{beta} equals 0, PEWMA converges to a standard EWMA. Finally
#' \code{l} is the parameter that determines the control limits. By default, 3
#' is used. \code{last.res} is the last result returned by some previous
#' execution of this algorithm. The first time the algorithm is executed its
#' value is NULL. However, if you want to run a new batch of data without having
#' to include it in the old data set and restart the process you only need to
#' add the last results returned by the last run.
#'
#' This algorithm can be used for both classical and incremental processing. It
#' should be noted that in case of having a finite data set the
#' \code{\link{CpPewma}} or \code{\link{OcpPEwma}} algorithms are faster.
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
#' @references M. Carter, Kevin y W. Streilein. Probabilistic reasoning for
#' streaming anomaly detection. 2012 IEEE Statistical Signal Processing Workshop
#' (SSP), pp. 377-380, Aug 2012.
#'
#' @example examples/ip_pewma_example.R


# Pewma CONTROL CHART
IpPewma <- function(data, n.train = 5, alpha0 = 0.8, beta = 0, l = 3, last.res = NULL) {

  # Pewma
  Pewma <- function(row, x) {
    row$i <- row$i + 1
    row$x <- x
    row$s1 <- row$s1.next
    row$std <- row$std.next
    row$z <- ifelse(row$std == 0, 0, (row$x - row$s1) / row$std)
    row$p <- 1 / sqrt(2 * pi) * exp(-(row$z ^ 2) / 2)
    row$alpha <- ifelse(row$i <= n.train, 1 - 1/row$i, (1 - beta * row$p) * alpha0)
    row$s1 <- row$alpha*row$s1 + (1 - row$alpha) * row$x
    row$s2 <- row$alpha*row$s2 + (1 - row$alpha) * row$x ^ 2
    row$s1.next <- row$s1
    row$std.next <- sqrt(abs(row$s2 - row$s1 ^ 2))
    row$ucl <- row$s1 + l[1] * row$std
    row$lcl <- row$s1 - l[1] * row$std
    row$is.anomaly <- row$x < row$lcl | row$x > row$ucl
    row
  }

  # inicializamos las variables
  n <- length(data)
  res <- NULL
  if (is.null(last.res)) {
    last.res <- data.frame(value = data[1],
                          i = 0,
                          s1 = data[1],
                          s2 = data[1]^2,
                          s1.next = data[1],
                          std.next = 0,
                          std = 0,
                          z = 0,
                          p = 0,
                          is.anomaly = 0,
                          lcl = 0,
                          ucl = 0,
                          alpha = alpha0)
  }

  for (i in 1:n) {
    last.res <- Pewma(last.res, data[i])
    res <- rbind(res, last.res[,c("is.anomaly", "lcl", "ucl")])
  }

  res <- data.frame(is.anomaly = unlist(res$is.anomaly),
    lcl = unlist(res$lcl), ucl = unlist(res$ucl))

  return(list(result = res, last.res = last.res))
}
