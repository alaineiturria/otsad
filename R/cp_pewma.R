#' Classic Processing Probabilistic-EWMA (PEWMA)
#'
#' \code{CpPewma} calculates the anomalies of a data set using classical
#' processing based on the PEWMA algorithm.
#'
#' @param data Numeric vector that includes train and test data to analyze.
#' @param n.train Number of points of the data set that correspond to the training set.
#' @param alpha0 initial alpha value
#' @param beta beta value
#' @param l Sigma multiplier to calculate the control limits.
#'
#' @details \code{data} must be numerical vector without NA values. BLA BLA BLA
#' Finally, \code{l} is the parameter that determines the control limits.
#' By default, 3 is used.
#'
#' @return Data set conformed by the following columns:
#'
#'   \item{is.anomaly}{1 if the value is anomalous 0 otherwise.}
#'   \item{ucl}{Upper control limit.}
#'   \item{lcl}{Lower control limit.}
#'
#' @references BLA BLA BLA
#'
#' @example examples/cp_pewma_example.R


# Pewma CONTROL CHART
CpPewma <- function(data, n.train = 5, alpha0 = 0.2, beta = 0, l = 3) {

  # Pewma
  Pewma <- function(row, x) {
    row$i <- row$i + 1
    row$x <- x
    row$s1 <- row$s1.next
    row$std <- row$std.next
    row$z <- ifelse(row$std == 0, 0, (row$x - row$s1) / row$std)
    row$p <- 1 / sqrt(2 * pi) * exp(-(row$z ^ 2) / 2)
    row$alpha <- ifelse(row$i <= n.train, 1 - 1 / row$i, (1 - beta * row$p) * row$alpha)
    row$s1 <- row$alpha * row$s1 + (1 - row$alpha) * row$x
    row$s2 <- row$alpha * row$s2 + (1 - row$alpha) * row$x ^ 2
    row$s1.next <- row$s1
    row$std.next <- sqrt(abs(row$s2 - row$s1^2))
    row$ucl <- row$s1 + l[1] * row$std
    row$lcl <- row$s1 - l[1] * row$std
    row$is.anomaly <- row$x < row$lcl | row$x > row$ucl
    row
  }

  # inicializamos las variables
  n <- length(data)
  res <- NULL
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

  for (i in 1:n) {
    last.res <- Pewma(last.res, data[i])
    res <- rbind(res, last.res[,c("is.anomaly", "lcl", "ucl")])
  }

  return(res)
}
