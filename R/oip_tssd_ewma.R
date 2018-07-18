#' Optimized Incremental Processing Two-Stage Shift-Detection based on EWMA
#'
#' @description \code{OipTsSdEwma} is the optimized implementation of the
#' \code{IpTsSdEwma} function using environmental variables. This function
#' allows the calculation of anomalies using TSSD-EWMA in an incremental
#' processing mode. It has been shown that in long datasets it can reduce
#' runtime by up to 50\%. This algorithm is a novel method for covariate
#' shift-detection tests based on a two-stage structure for univariate
#' time-series. TSSD-EWMA works in two phases. In the first phase, it detects
#' anomalies using the SD-EWMA \code{\link{CpSdEwma}} algorithm. In the second
#' phase, it checks the veracity of the anomalies using the Kolmogorov-Simirnov
#' test to reduce false alarms.
#'
#' @param data Numerical vector with training and test dataset.
#' @param n.train Number of points of the dataset that correspond to the
#' training set.
#' @param threshold Error smoothing constant.
#' @param l Control limit multiplier.
#' @param m Length of the subsequences for applying the Kolmogorov-Smirnov test.
#' @param to.next.iteration list with the necessary parameters to execute in
#' the next iteration
#'
#' @details \code{data} must be a numerical vector without NA values.
#' \code{threshold} must be a numeric value between 0 and 1.
#' It is recommended to use low values such as 0.01 or 0.05. By default, 0.01 is
#' used. Finally, \code{l} is the parameter that determines the control limits.
#' By default, 3 is used. \code{m} is the length of the subsequences for
#' applying the Kolmogorov-Smirnov test. By default, 5 is used. It should be
#' noted that the last m values have not been verified because you need other m
#' values to be able to perform the verification. Finally
#' \code{to.next.iteration} is the last result returned by some previous
#' execution of this algorithm. The first time the algorithm is executed its
#' value is NULL. However, to run a new batch of data without having to include
#' it in the old dataset and restart the process, the two parameters returned by
#' the last run are only needed.
#'
#' @return A list of the following items.
#'
#'   \item{last.data.checked}{Anomaly results of the last \code{m} results of
#'   the previous iteration. dataset conformed by the following columns.}
#'   \itemize{
#'      \item \code{is.anomaly} 1 if the value is anomalous 0 otherwise.
#'      \item \code{ucl} Upper control limit.
#'      \item \code{lcl} Lower control limit.
#'  }
#'  \item{checked.results}{Anomaly results of the dataset excluding the
#'  last \code{m} values because they could not be verified. dataset conformed
#'  by the following columns: \code{is.anomaly}, \code{ucl}, \code{lcl}.}
#'  \item{to.next.iteration}{Last result returned by the algorithm. It is a list
#'  containing the following items.}
#'  \itemize{
#'      \item \code{last.res} Last result returned by the aplicaction of
#'      SD-EWMA function with the calculations of the parameters of the last run
#'      . These are necessary for the next run.
#'      \item \code{to.check} Subsequence of the last remaining unchecked
#'      values to be checked in the next iteration. dataset conformed by the
#'      following columns: \code{is.anomaly}, \code{ucl}, \code{lcl},
#'      \code{value}.
#'      \item \code{last.m} Subsequence of the m values prior to the to.check
#'      subsecuence necessary to verify the values in to.check.
#'  }
#'
#' @references Raza, H., Prasad, G., & Li, Y. (03 de 2015). EWMA model based
#' shift-detection methods for detecting covariate shifts in non-stationary
#' environments. Pattern Recognition, 48(3), 659-669.
#'
#' @example tests/examples/oip_tssd_ewma_example.R
#'
#' @export


OipTsSdEwma <- function(data, n.train, threshold, l = 3, m = 5,
                        to.next.iteration = list(last.res = NULL,
                                                 to.check = NULL,
                                                 last.m = NULL)) {

  # validate parameters
  if (!is.numeric(data) | (sum(is.na(data)) > 0)) {
    stop("data argument must be a numeric vector and without NA values.")
  }
  if (!is.numeric(n.train) | n.train >= length(data)) {
    stop("n.train argument must be a numeric value and less than data length.")
  }
  if (!is.numeric(threshold) | threshold <= 0 |  threshold > 1) {
    stop("threshold argument must be a numeric value in (0,1] range.")
  }
  if (!is.numeric(l)) {
    stop("l argument must be a numeric value.")
  }
  if (!is.numeric(m)) {
    stop("m argument must be a numeric value and smaller than all dataset
          length.")
  }
  if (!is.null(to.next.iteration) & !is.list(to.next.iteration)) {
    stop("to.next.iteration argument must be NULL or a list with las execution
          result.")
  }

  ApplyKolmogorovTest <- function(pos, all.data) {
    if ((pos - (m - 1)) > 0 & (pos + m) <= length(all.data)) {
      part1 <- all.data[(pos - (m - 1)):pos]
      part2 <- all.data[(pos + 1):(pos + m)]
      res_test <- suppressWarnings(stats::ks.test(part1, part2, exact = NULL))
      return(ifelse(res_test$p.value > 0.05, 0, 1))
    } else {
      return(1)
    }
  }

  # get anomalous rows
  result <- OipSdEwma(data, n.train, threshold, l, to.next.iteration$last.res)
  # merge result and to.check data and check anomalous rows
  result$result$value <-
  data[(length(data) - nrow(result$result) + 1):length(data)]
  all.data <- rbind(to.next.iteration$to.check, result$result)
  rownames(all.data) <- 1:nrow(all.data)
  anomaly.pos <- which(all.data$is.anomaly == 1)
  if (length(anomaly.pos) != 0) {
    all.data[anomaly.pos, "is.anomaly"] <-
    sapply((anomaly.pos + length(to.next.iteration$last.m)), ApplyKolmogorovTest,
           c(to.next.iteration$last.m, all.data$value))
  }
  # prepare result
  n <- nrow(all.data)
  if (is.null(to.next.iteration$to.check)) {
    last.data.checked <- NULL
    checked.results <- all.data[1:(n - m), names(all.data) != "value"]
  } else {
    last.data.checked.n <- nrow(to.next.iteration$to.check)
    last.data.checked <-
    all.data[1:last.data.checked.n, names(all.data) != "value"]
    checked.results <-
    all.data[(last.data.checked.n + 1):(n - m), names(all.data) != "value"]
  }
  to.next.iteration$to.check <- all.data[(n - m + 1):n,]
  to.next.iteration$last.m <- all.data[(n - 2 * m + 1):(n - m), "value"]
  to.next.iteration$last.res <- result$last.res


  return(list(last.data.checked = last.data.checked,
              checked.results = checked.results,
              to.next.iteration = to.next.iteration))
}
