#' Incremental Processing Probabilistic-EWMA (PEWMA).
#'
#' @description \code{IpPewma} allows the calculation of anomalies using PEWMA
#' in an incremental processing mode. See also \code{\link{OipPewma}}, the
#' optimized and faster function of this function This algorithm is a
#' probabilistic method of EWMA which dynamically adjusts the parameterization
#' based on the probability of the given observation. This method produces
#' dynamic, data-driven anomaly thresholds which are robust to abrupt transient
#' changes, yet quickly adjust to long-term distributional shifts.
#'
#' @param data Numerical vector with training and test dataset.
#' @param n.train Number of points of the dataset that correspond to the
#' training set.
#' @param alpha0  Maximal weighting parameter.
#' @param beta Weight placed on the probability of the given observation.
#' @param l Control limit multiplier.
#' @param last.res Last result returned by the algorithm.
#'
#' @details \code{data} must be a numerical vector without NA values.
#' \code{alpha0} must be a numeric value where 0 < \code{alpha0} < 1. If a
#' faster adjustment to the initial shift is desirable, simply lowering
#' \code{alpha0} will suffice. \code{beta} is the weight placed on the
#' probability of the given observation. it must be a numeric value where
#' 0 <= \code{beta} <= 1. Note that \code{beta} equals 0, PEWMA converges to a
#' standard EWMA. Finally \code{l} is the parameter that determines the control
#' limits. By default, 3 is used. \code{last.res} is the last result returned
#' by some previous execution of this algorithm. The first time the algorithm
#' is executed its value is NULL. However, to run a new batch
#' of data without having to include it in the old dataset and restart the
#' process, the two parameters returned by the last run are only needed.
#'
#' This algorithm can be used for both classical and incremental processing. It
#' should be noted that in case of having a finite dataset the
#' \code{\link{CpPewma}} or \code{\link{OcpPewma}} algorithms are faster.
#' Incremental processing can be used in two ways. 1) Processing all available
#' data and saving \code{last.res} for future runs in which there is new data.
#' 2) Using the \href{https://CRAN.R-project.org/package=stream}{stream} library
#' for when there is too much data and it does not fit into the memory.
#' An example has been made for this use case.
#'
#' @return A list of the following items.
#'
#'   \item{result}{dataset conformed by the following columns.}
#'   \itemize{
#'      \item \code{is.anomaly} 1 if the value is anomalous 0, otherwise.
#'      \item \code{ucl} Upper control limit.
#'      \item \code{lcl} Lower control limit.
#'  }
#'  \item{last.res}{Last result returned by the algorithm. Is a dataset
#'  containing the parameters calculated in the last iteration and necessary
#'  for the next one.}
#'
#' @references M. Carter, Kevin y W. Streilein. Probabilistic reasoning for
#' streaming anomaly detection. 2012 IEEE Statistical Signal Processing Workshop
#' (SSP), pp. 377-380, Aug 2012.
#'
#' @example tests/examples/ip_pewma_example.R
#'
#' @export


# Pewma CONTROL CHART
IpPewma <- function(data, n.train = 5, alpha0 = 0.8, beta = 0, l = 3, last.res = NULL) {

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
  if (!is.null(last.res) & !is.data.frame(last.res)) {
    stop("last.res argument must be NULL or a data.frame with las execution result.")
  }

  # Auxiliar function Pewma
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
    row$ucl <- row$s1 + l[1] * row$std.next
    row$lcl <- row$s1 - l[1] * row$std.next
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
