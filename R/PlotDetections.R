#' PLOT DETECTIONS
#'
#' @description \code{PlotDetections} shows in a graph the results obtained after the application
#' of one of the anomaly detectors included in this package.
#'
#' @param data data.frame composed of at least one column called timestamp and another column
#' called value. You can also include other columns such as is.anomaly, is.real.anomaly, ucl, lcl,
#' anomaly.score. Any of these columns except is.real.anomaly that are included in the dataset will
#' be shown in the graph automatically.
#' @param print.real.anomaly If TRUE adds the real anomalies to the graph.
#' @param print.time.window If TRUE shows a time band centered on the real anomaly. According to the
#' article shown in the reference, if the detected anomaly remains within it would be considered a
#' true positive.
#' @param title Title of the graph.
#' @param xlab X Axis Name.
#' @param ylab Y Axis Name.
#' @param return.ggplot If TRUE the function returns a ggplot object.
#'
#' @details \code{data} must be a data.frame. The \code{timestamp} column can be numeric, of type POSIXlt,
#' or a character type date convertible  to POSIXlt. The \code{value} column must be numeric.
#' \code{is.anomaly}, \code{is.real.anomaly}, \code{ucl}, \code{lcl}, \code{anomaly.score} are
#' some of the variables returned by the algorithms included in this package and must be numeric
#' or boolean in the case of columns is.anomaly, is.real.anomaly .
#'
#' @return plotly object.
#'
#' @references A. Lavin and S. Ahmad, “Evaluating Real-time Anomaly Detection Algorithms – the
#' Numenta Anomaly Benchmark,” in 14th International Conference on Machine Learning and
#' Applications (IEEE ICMLA’15), 2015.
#'
#' @export

PlotDetections <- function(data, print.real.anomaly = FALSE, print.time.window = FALSE, title = "",
                           xlab = "Time", ylab = "Value", return.ggplot = FALSE) {

  column.names <- names(data)

  if (sum(c("timestamp", "value") %in% column.names) != 2) {
    stop("the data.frame must have timestamp and value columns")
  }

  if (is.character(data$timestamp)) {
    data$timestamp <- as.POSIXct(data$timestamp, tz="UTC")
  }


  myPlot <- ggplot2::ggplot(data = data, ggplot2::aes_string("timestamp", "value")) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_light() +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold", size = 20)) +
    ggplot2::theme(text = ggplot2::element_text(size=18))

  traza <- 0
  eliminate <- NULL

  if (print.time.window) {
    if ("is.real.anomaly" %in% column.names) {

      data <- GetWindowsLimits(data)
      data[data$is.real.anomaly == 1 & data$start.limit != 0,"start.limit"] <-
        as.character(data[data[data$is.real.anomaly == 1,"start.limit"],"timestamp"])
      data[data$is.real.anomaly == 1 & data$end.limit != 0,"end.limit"] <-
        as.character(data[data[data$is.real.anomaly == 1,"end.limit"],"timestamp"])
      data[data$is.real.anomaly != 1,"start.limit"] <- "1900-01-01 00:00:00"
      data[data$is.real.anomaly != 1,"end.limit"] <- "1900-01-01 00:00:00"
      data[data$is.real.anomaly == 1 & data$start.limit == 0,"start.limit"] <- "1900-01-01 00:00:00"
      data[data$is.real.anomaly == 1 & data$end.limit == 0,"end.limit"] <- "1900-01-01 00:00:00"
      data$start.limit <- as.POSIXct(data$start.limit, tz="UTC")
      data$end.limit <- as.POSIXct(data$end.limit, tz="UTC")


      myPlot <- myPlot + ggplot2::geom_rect(
        data = data[data$is.real.anomaly == 1,],
        ggplot2::aes(
          xmin = data[data$is.real.anomaly == 1,"start.limit"],
          xmax = data[data$is.real.anomaly == 1,"end.limit"],
          ymin = min(data$value), ymax = max(data$value)
        ),
        fill = "orange", alpha = 0.2, colour = NA
      )
      traza <- traza + 1
      eliminate <- c(eliminate, traza)

    } else {
      stop("It isn't posible print time windows without is.real.anomaly column")
    }
  }

  if (sum(c("lcl", "ucl") %in% column.names) > 1){
    myPlot <- myPlot + ggplot2::geom_ribbon(
        data = data,
        ggplot2::aes_string(ymin="lcl", ymax="ucl"),
        fill = "deepskyblue2",
        colour = "white",
        alpha = 0.15
      ) + ggplot2::guides(fill=FALSE)
    traza <- traza + 1
    eliminate <- c(eliminate, traza)
  }

  myPlot <- myPlot + ggplot2::geom_line(na.rm=TRUE, colour = "deepskyblue3")

  if ("is.anomaly" %in% column.names) {
    if ("anomaly.score" %in% column.names) {
      myPlot <- myPlot +
        ggplot2::geom_point(data = data[data$is.anomaly == 1, ],
                            colour = "red", size = 3, alpha = 0.5) +
        ggplot2::geom_text(data = data[data$is.anomaly == 1, ],
                           ggplot2::aes(label = paste("Score:", data[data$is.anomaly == 1, "anomaly.score"])))
    } else {
      myPlot <- myPlot + ggplot2::geom_point(
        data = data[data$is.anomaly == 1,],
        colour = "red", size = 3, alpha = 0.5
      )
    }
  }

  if (print.real.anomaly) {
    if ("is.real.anomaly" %in% column.names) {
      myPlot <- myPlot + ggplot2::geom_point(
        data = data[data$is.real.anomaly == 1,],
        colour = "orange", size = 3, alpha = 0.5
      )
    } else {
      stop("It isn't posible print time windows without is.real.anomaly column")
    }
  }

  if (return.ggplot) {
    return(myPlot)
  } else {
    myPlot <- plotly::ggplotly(myPlot)
    if (!is.null(eliminate)) {
      myPlot <- plotly::style(myPlot, hoverinfo = "none", traces = eliminate)
    }
    # myPlot <- plotly::rangeslider(myPlot)
    return(myPlot)
  }

}
