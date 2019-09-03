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
#'   \item{result}{Dataset conformed by the following columns:}
#'   \itemize{
#'      \item \code{is.anomaly} 1 if the value is anomalous 0 otherwise.
#'      \item \code{ucl} Upper control limit.
#'      \item \code{lcl} Lower control limit.
#'      \item \code{i} row id or index
#'  }
#'  \item{last.data.checked}{Data frame with checked anomalies. \code{i} column is the id or
#'  index and \code{is.anomaly} is its new is.anomaly value.}
#'  \item{to.next.iteration}{Last result returned by the algorithm. It is a list
#'  containing the following items.}
#'  \itemize{
#'      \item \code{last.res} Last result returned by the aplicaction of
#'      SD-EWMA function with the calculations of the parameters of the last run
#'      . These are necessary for the next run.
#'      \item \code{to.check} Subsequence of the last remaining unchecked
#'      values to be checked in the next iterations.
#'      \item \code{last.m} Subsequence of the last m values.
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
                        to.next.iteration = list(last.res = NULL, to.check = NULL, last.m = NULL)) {

  # validate parameters
  if (!is.numeric(data) | (sum(is.na(data)) > 0)) {
    stop("data argument must be a numeric vector and without NA values.")
  }
  if (!is.numeric(n.train) | n.train <= 0) {
    stop("n.train argument must be a positive numeric value.")
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

  ApplyKolmogorovTest <- function(part1, part2) {
    res.test <- suppressWarnings(stats::ks.test(part1, part2, exact = NULL))
    return(ifelse(res.test$p.value > 0.05, 0, 1))
  }

  OnePointTssdEwma <- function(data, n.train, threshold, l, m, to.next.iteration) {
    result <- OipSdEwma(data, n.train, threshold, l, to.next.iteration$last.res)

    if (is.null(to.next.iteration)) {
      last.m <- NULL
      to.check <- list()
    } else {
      last.m <- to.next.iteration$last.m
      to.check <- to.next.iteration$to.check
    }

    last.m.values <- ifelse(is.null(last.m), 0, length(last.m))
    result$result$i <- result$last.res[1,"i"]

    if (last.m.values < m) {
      last.m <- c(last.m, data)
      to.check <- NULL
      last.data.checked <- NULL
    } else {
      last.m <- last.m[-1]
      last.m <- c(last.m, data)
      to.check.len <- length(to.check)
      if (result$result$is.anomaly) {
        to.check[[to.check.len + 1]] <- list(index = result$result$i,
                                             check.in = result$result$i + m,
                                             last.m = last.m)
      }
      if (to.check.len >= 1) {
        if (to.check[[1]]$check.in == result$result$i) {
          is.anomaly <- ApplyKolmogorovTest(to.check[[1]]$last.m, last.m)
          last.data.checked <- data.frame(i = to.check[[1]]$index, is.anomaly = is.anomaly)
          to.check <- to.check[-1]
        } else {
          last.data.checked <- NULL
        }
      } else {
        last.data.checked <- NULL
      }
    }

    to.next.iteration <- list(last.res = result$last.res, to.check = to.check, last.m = last.m)

    return(list(result = result$result, last.data.checked = last.data.checked,
                to.next.iteration = to.next.iteration))
  }

  if (length(data) == 1) {
    return(OnePointTssdEwma(data, n.train, threshold, l, m, to.next.iteration))
  } else {
    n <- length(data)
    last.res <- list()
    last.res$result <- NULL
    last.res$to.next.iteration <- to.next.iteration
    res <- NULL
    last.data.checked <- NULL
    ## Calculate anomalies
    for (i in 1:n) {
      last.res <- OnePointTssdEwma(data = data[i], n.train, threshold, l, m,
                                   last.res$to.next.iteration)
      res <- rbind(res, last.res$result)
      if (!is.null(last.res$last.data.checked)) {
        last.data.checked <- rbind(last.data.checked, last.res$last.data.checked)
      }
    }
    if (!is.null(last.data.checked)) {
      res[res$i %in% last.data.checked$i, "is.anomaly"] <- last.data.checked$is.anomaly
      last.data.checked <- last.data.checked[!(last.data.checked$i %in% res$i),]
      if (nrow(last.data.checked) == 0) last.res[2] <- list(NULL)
      else last.res$last.data.checked <- last.data.checked
    }

    last.res$result <- res
    return(last.res)
  }
}
