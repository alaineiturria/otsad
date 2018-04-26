#' @function OipTsSdEwma
#' @description Optimized Incremental processing two-stage Shift-Detection based on EWMA
#' @param data data values
#' @param n.train number of training values
#' @param threshold threshold
#' @param l sigma multiplier
#' @param m number of values to take into account to verify the non-stationarity
#' @param to.next.iteration list with the necessary parameters to execute in the next iteration  
#'
#' @return list with the following parameters: dataset verified results (checked.results) and dataset verified previous results (last.data.checked) and to.next.iteration


OipTsSdEwma <- function(data, n.train, threshold, l = 3, m = 5, to.next.iteration = list(last.res = NULL, to.check = NULL, last.m = NULL)) {
  
  ApplyKolmogorovTest <- function(pos, all.data) {
    if ((pos - (m - 1)) > 0 & (pos + m) <= length(all.data)) {
      part1 <- all.data[(pos - (m - 1)):pos]
      part2 <- all.data[(pos + 1):(pos + m)]
      res_test <- ks.test(part1, part2, exact = NULL)
      return(ifelse(res_test$p.value > 0.05, 0, 1))
    } else {
      return(1)
    }
  }
  
  # get anomalous rows
  result <- OipSdEwma(data, n.train, threshold, l, to.next.iteration$last.res)
  # merge result and to.check data and check anomalous rows
  result$result$value <- data[(length(data) - nrow(result$result) + 1):length(data)]
  all.data <- rbind(to.next.iteration$to.check, result$result)
  rownames(all.data) <- 1:nrow(all.data)
  anomaly.pos <- which(all.data$is.anomaly == 1)
  if (length(anomaly.pos) != 0) {
    all.data[anomaly.pos, "is.anomaly"] <- sapply((anomaly.pos + length(to.next.iteration$last.m)), ApplyKolmogorovTest,
      c(to.next.iteration$last.m, all.data$value))
  }
  # prepare result
  n <- nrow(all.data)
  if (is.null(to.next.iteration$to.check)) {
    last.data.checked <- NULL
    checked.results <- all.data[1:(n - m), names(all.data) != "value"]
  } else {
    last.data.checked.n <- nrow(to.next.iteration$to.check)
    last.data.checked <- all.data[1:last.data.checked.n, names(all.data) != "value"]
    checked.results <- all.data[(last.data.checked.n + 1):(n - m), names(all.data) != "value"]
  }
  to.next.iteration$to.check <- all.data[(n - m + 1):n,]
  to.next.iteration$last.m <- all.data[(n - 2 * m + 1):(n - m), "value"]
  to.next.iteration$last.res <- result$lastRes
  
  
  return(list(last.data.checked = last.data.checked, checked.results = checked.results, to.next.iteration = to.next.iteration))
}
