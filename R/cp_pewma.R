#' Classic Processing Probabilistic-EWMA (PEWMA).
#'
#' @description \code{CpPewma} calculates the anomalies of a dataset using
#' classical processing based on the PEWMA algorithm. This algorithm is
#' a probabilistic method of EWMA which dynamically adjusts the parameterization
#' based on the probability of the given observation. This method produces
#' dynamic, data-driven anomaly thresholds which are robust to abrupt transient
#' changes, yet quickly adjust to long-term distributional shifts. See also
#' \code{\link{OcpPewma}}, the optimized and faster function of the this
#' function.
#'
#' @param data Numerical vector with training and test dataset.
#' @param n.train Number of points of the dataset that correspond to the
#' training set.
#' @param alpha0  Maximal weighting parameter.
#' @param beta Weight placed on the probability of the given observation.
#' @param l Control limit multiplier.
#'
#' @details \code{data} must be a numerical vector without NA values.
#' \code{alpha0} must be a numeric value where 0 < \code{alpha0} < 1. If a
#' faster adjustment to the initial shift is desirable, simply lowering
#' \code{alpha0} will suffice. \code{beta} is the weight placed on the
#' probability of the given observation. It must be a numeric value where
#' 0 <= \code{beta} <= 1. Note that if \code{beta} equals 0, PEWMA converges to
#' a standard EWMA. Finally \code{l} is the parameter that determines the
#' control limits. By default, 3 is used.
#'
#' @return dataset conformed by the following columns:
#'
#'   \item{is.anomaly}{1 if the value is anomalous 0, otherwise.}
#'   \item{ucl}{Upper control limit.}
#'   \item{lcl}{Lower control limit.}
#'
#' @references M. Carter, Kevin y W. Streilein. Probabilistic reasoning for
#' streaming anomaly detection. 2012 IEEE Statistical Signal Processing Workshop
#' (SSP), pp. 377-380, Aug 2012.
#'
#' @example tests/examples/cp_pewma_example.R
#'
#' @export

CpPewma <- function(data, n.train = 5, alpha0 = 0.8, beta = 0.3, l = 3) {

  # validate parameters
  if (!is.numeric(data) | (sum(is.na(data)) > 0)) {
    stop("data argument must be a numeric vector and without NA values.")
  }
  if (!is.numeric(n.train) | n.train >= length(data)) {
    stop("n.train argument must be a numeric value and less than data length.")
  }
  if (!is.numeric(alpha0) | alpha0 <= 0 |  alpha0 > 1) {
    stop("alpha0 argument must be a numeric value in (0,1] range.")
  }
  if (!is.numeric(beta) | beta < 0 |  beta > 1) {
    stop("beta argument must be a numeric value in [0,1] range.")
  }
  if (!is.numeric(l)) {
    stop("l argument must be a numeric value.")
  }

  # Auxiliar function Pewma
  Pewma <- function(row, x) {
    row$i <- row$i + 1
    row$x <- x
    row$s1 <- row$s1.next
    row$std <- row$std.next
    row$z <- ifelse(row$std == 0, 0, (row$x - row$s1) / row$std)
    row$p <- 1 / sqrt(2 * pi) * exp(-(row$z ^ 2) / 2)
    row$alpha <- ifelse(row$i <= n.train, 1 - 1 / row$i, (1 - beta * row$p) * alpha0)
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
  last.res <- data.frame(i = 0,
                         s1 = mean(data[1:n.train]),
                         s2 = mean(data[1:n.train]) ^ 2,
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

  res[1:n.train, "is.anomaly"] <- 0
  res[1:n.train, "lcl"] <- data[1:n.train]
  res[1:n.train, "ucl"] <- data[1:n.train]

  return(res)
}
