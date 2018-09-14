#' Get detector score
#'
#' @description \code{GetDetectorScore} Calculates the start and end positions of each window that
#' are focused on the real anomalies. This windows can be used to know if the detected anomaly is a
#' true positive or not.
#'
#' @param data All dataset with training and test datasets and with at least \code{timestamp},
#' \code{value}, \code{is.anomaly} and \code{is.real.anomaly} columns.
#' @param print If TRUE shows a graph with results.
#' @param title Title of the graph.
#'
#' @details \code{data} must be a data.frame with  \code{timestamp}, \code{value}, \code{is.anomaly}
#' and \code{is.real.anomaly} columns. \code{timestamp} column can be numeric, of type POSIXct, or a
#' character type date convertible to POSIXct.
#'
#' This function calculates the scores based on three different profiles. Each label tp, fp, tn,
#' fn is associated with a weight to give a more realistic score.
#' For the standard profile weights are tp = 1, tn = 1, fp, = 0.11, and fn = 1.
#' For the reward_low_FP_rate profile weights are tp = 1, tn = 1, fp, = 0.22, and fn = 1.
#' For the reward_low_FN_rate profile weights are tp = 1, tn = 1, fp, = 0.11, and fn = 2.
#'
#' @return List conformed by the following items:
#'   \item{data}{Same data set with additional columns such as \code{label}, \code{start.limit},
#'   \code{end.limit}, \code{standard.score} and etc.}
#'   \item{all.score.standard}{Total score obtained by the detector using the weights of the
#'   standard profile.}
#'   \item{all.score.low_FP_rate}{Total score obtained by the detector using the weights of the
#'   reward_low_FP_rate profile.}
#'   \item{all.score.low_FN_rate}{Total score obtained by the detector using the weights of the
#'   reward_low_FN_rate profile.}
#'   \item{tp}{Number of true positives}
#'   \item{tn}{Number of true negatives}
#'   \item{fp}{Number of false positives}
#'   \item{fn}{Number of false negatives}
#'
#'   @references A. Lavin and S. Ahmad, “Evaluating Real-time Anomaly Detection Algorithms – the
#'   Numenta Anomaly Benchmark,” in 14th International Conference on Machine Learning and
#'   Applications (IEEE ICMLA’15), 2015.
#'
#' @export

GetDetectorScore <- function(data, print = FALSE, title = ""){

  col.names <- names(data)

  if (sum(c("timestamp", "value", "is.anomaly", "is.real.anomaly") %in% col.names) != 4) {
    stop("data argument must be a data.frame with timestamp}, value, is.anomaly and is.real.anomaly
         columns.")
  }

  data <- GetWindowsLimits(data)
  data <- GetLabels(data)

  # Auxiliar function
  Scale.sigmoid <- function(relative.position) {
    if (relative.position > 3.0) {
      return(-1.0)
    } else {
      return (2 * sigmoid::sigmoid(-5 * relative.position) - 1.0)
    }
  }

  # Calculate tp and fn scores
  Calculate.tpfn.scores <- function(index, cost.type) {
    costs <- profiles
    if (data[index, "label"] == "fn") {
      score <- -costs[costs$cost.type == cost.type, "fn.weight"]
    } else {
      windowLength <- data[index, "end.limit"] - data[index, "start.limit"] + 1
      position <- - (data[index, "end.limit"] - data[index, "first.tp"] + 1) / c(windowLength)
      score <- Scale.sigmoid(position) * costs[costs$cost.type == cost.type, "tp.weight"] /
        Scale.sigmoid(-1.0)
    }
    return(score)
  }

  data$standard.score <- 0
  data$low_FP_rate.score <- 0
  data$low_FN_rate.score <- 0

  real.anomaly.index <-  which(data$is.real.anomaly == 1 & data$start.limit != 0)
  data[real.anomaly.index, "standard.score"] <- sapply(real.anomaly.index, Calculate.tpfn.scores,
                                                       "standard")
  data[real.anomaly.index, "low_FP_rate.score"] <- sapply(real.anomaly.index, Calculate.tpfn.scores,
                                                          "reward_low_FP_rate")
  data[real.anomaly.index, "low_FN_rate.score"] <- sapply(real.anomaly.index, Calculate.tpfn.scores,
                                                          "reward_low_FN_rate")

  # Calculate fp scores
  calculate.fp.scores <- function(index, cost.type) {

    costs <- profiles

    # get nearest window
    real.anomaly.index <- which(data$is.real.anomaly == 1 & data$start.limit != 0)
    smaller.anomaly.index <- real.anomaly.index[real.anomaly.index < index]
    if (length(smaller.anomaly.index) == 0) {
      window.index <- -1
    } else {
      window.index <- smaller.anomaly.index[length(smaller.anomaly.index)]
    }

    # calculate score
    if (window.index == -1) {
      score <- - costs[costs$cost.type == cost.type, "fp.weight"]
    } else {
      windowLength <- data[window.index, "end.limit"] - data[window.index, "start.limit"] + 1
      position <- abs((data[window.index, "end.limit"] - index)) / (windowLength - 1)
      score <- Scale.sigmoid(position) * costs[costs$cost.type == cost.type, "fp.weight"]
    }
    return(score)
  }

  fp.index <- which(data$label == "fp")
  data[fp.index, "standard.score"] <- sapply(fp.index, calculate.fp.scores, "standard")
  data[fp.index, "low_FP_rate.score"] <- sapply(fp.index, calculate.fp.scores, "reward_low_FP_rate")
  data[fp.index, "low_FN_rate.score"] <- sapply(fp.index, calculate.fp.scores, "reward_low_FN_rate")

  # Plot results
  if (print) {
    d <- data
    d$timestamp <- as.POSIXct(d$timestamp, tz="UTC")
    d[d$is.real.anomaly == 1 & d$start.limit == 0, "is.real.anomaly"] <- 0
    d[d$is.real.anomaly == 1, "start.limit"] <-
      as.character(d[d[d$is.real.anomaly == 1,"start.limit"],"timestamp"])
    d[d$is.real.anomaly == 1,"end.limit"] <-
      as.character(d[d[d$is.real.anomaly == 1,"end.limit"],"timestamp"])
    d[d$is.real.anomaly != 1,"start.limit"] <- "1900-01-01 00:00:00"
    d[d$is.real.anomaly != 1,"end.limit"] <- "1900-01-01 00:00:00"
    d$start.limit <- as.POSIXct(d$start.limit, tz="UTC")
    d$end.limit <- as.POSIXct(d$end.limit, tz="UTC")
    d$standard.score <- round(d$standard.score,2)
    d$low_FP_rate.score <-round(d$low_FP_rate.score,2)
    d$low_FN_rate.score <- round(d$low_FN_rate.score,2)

    d2 <- d[(d$is.real.anomaly == 1 & d$label %in% c("tp", "fn")) |
        (d$is.anomaly == 1 & d$label == "fp"),]

    myPlot <- ggplot2::ggplot(d, ggplot2::aes_string("timestamp", "value")) +
      ggplot2::geom_rect(
        data = d[d$is.real.anomaly == 1,],
        ggplot2::aes(
          xmin = d[d$is.real.anomaly == 1,"start.limit"],
          xmax = d[d$is.real.anomaly == 1,"end.limit"],
          ymin = min(d$value), ymax = max(d$value)
        ),
        fill = "orange", alpha = 0.2, colour = NA
      ) +
      ggplot2::geom_line(na.rm=TRUE, color = "deepskyblue3") +
      ggplot2::ggtitle(title) +
      ggplot2::xlab("Time") +
      ggplot2::ylab("Value") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(lineheight=.8, face="bold", size = 20),
        text = ggplot2::element_text(size=18)
      ) +
      ggplot2::geom_point(
        data = d2,
        ggplot2::aes(colour = d2$label,
                     text = paste0("Standar Score: ", d2$standard.score,
                                   "</br> Low_FP_rate Score: ", d2$low_FP_rate.score,
                                   "</br> Low_FN_rate Score: ", d2$low_FN_rate.score)),
        size = 4,
        alpha = 0.5
      ) +
      ggplot2::scale_colour_manual('Label:',
                                   values = c("tp"= "forestgreen", "fn" = "orange", "fp" = "red"))

    myPlot <- plotly::ggplotly(myPlot)
    print(myPlot)
  }

  return(list(data = data,
              all.score.standard = sum(data$standard.score),
              all.score.low_FP_rate = sum(data$low_FP_rate.score),
              all.score.low_FN_rate = sum(data$low_FN_rate.score),
              tp = sum(data$label == "tp" & data$is.real.anomaly == 1),
              tn = sum(data$label == "tn"),
              fp = sum(data$label == "fp"),
              fn = sum(data$label == "fn")
              )
         )

}
