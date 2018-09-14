#' Get Lables
#'
#' @description \code{GetLabels} Calculates the start and end positions of each window that
#' are focused on the real anomalies. This windows can be used to know if the detected anomaly is a
#' true positive or not.
#'
#' @param data All dataset with training and test datasets with at least \code{timestamp},
#' \code{value}, \code{is.anomaly}, \code{is.real.anomaly}, \code{start.limit} and
#' \code{end.limit} columns.
#'
#' @details \code{data} must be a data.frame with  \code{timestamp}, \code{value}, \code{is.anomaly}
#' and \code{is.real.anomaly} columns. \code{timestamp} column can be numeric, of type POSIXct, or a
#' character type date convertible to POSIXct. see  \code{\link{GetWindowsLimits}} to know more
#' about how to get \code{start.limit} and \code{end.limit} columns.
#'
#' @return Same data set with two additional columns \code{label} and \code{first.tp}.
#' \code{first.tp} indicates for each window Which is the position of first true positive.
#' \code{label} indicates for each detection if it is a TP, FP, TN or FN.
#'
#' @references A. Lavin and S. Ahmad, “Evaluating Real-time Anomaly Detection Algorithms – the
#' Numenta Anomaly Benchmark,” in 14th International Conference on Machine Learning and
#' Applications (IEEE ICMLA’15), 2015.
#'
#' @export

GetLabels <- function(data) {

  col.names <- names(data)

  if (sum(c("timestamp", "value", "is.anomaly", "is.real.anomaly") %in% col.names) != 4) {
    stop("data argument must be a data.frame with timestamp}, value, is.anomaly and is.real.anomaly
         columns.")
  }

  calculate.tp <- function(index) {
    start <- data[index, "start.limit"]
    end <- data[index, "end.limit"]
    anomaly.index <- which(data$is.anomaly == 1)
    anomaly.pos <- which(anomaly.index >= start & anomaly.index <= end)
    if (length(anomaly.pos) == 0) {
      return(-1)
    } else {
      return(anomaly.index[anomaly.pos])
    }
  }

  real.anomaly.index <- which(data$is.real.anomaly == 1 & data$start.limit != 0)
  tp.index <- lapply(real.anomaly.index, calculate.tp)
  # set first tp
  data$first.tp <- 0
  data[real.anomaly.index, "first.tp"] <- sapply(tp.index, function(elem) return(elem[1]))
  # set tn label
  data$label <- "tn"
  tp.index <- unlist(tp.index)
  # set tp label
  tp.index <- tp.index[tp.index != -1]
  data[tp.index, "label"] <- "tp"
  tp.index <- which(data$first.tp != -1 & data$first.tp != 0)
  data[tp.index, "label"] <- "tp"
  # set fn label
  tp.index <- which(data$first.tp == -1)
  data[tp.index, "label"] <- "fn"
  # set fp label
  fp.index <- which(data$is.anomaly == 1 & data$label != "tp")
  data[fp.index, "label"] <- "fp"

  return(data)

}
