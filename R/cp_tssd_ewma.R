#' @function CpTsSdEwma
#' @description classic processing two-stage Shift-Detection based on EWMA
#' @param train.data training data
#' @param test.data test data
#' @param beta beta value
#' @param threshold threshold
#' @param l sigma multiplier
#' @param m
#'
#' @return dataset containing if anomaly and upper and lower limit


CpTsSdEwma <- function(train.data, test.data, threshold, l = 3, m = 5) {
  
  ApplyKolmogorovTest <- function(pos, all.data) {
    if ((pos - (m - 1)) > 0 | (pos + m) <= length(all.data)) {
      part1 <- all.data[(pos - (m - 1)):pos]
      part2 <- all.data[(pos + 1):(pos + m)]
      res.test <- ks.test(part1, part2, exact = NULL)
      return(ifelse(res.test$p.value > 0.05, 0, 1))
    } else {
      return(1)
    }
  }
  
  result <- CpSdEwma(train.data, test.data, threshold, l)
  all.data <- c(train.data, test.data)
  anomally.pos <- which(result$is.anomaly == 1)
  result[anomally.pos, "is.anomaly"] <- sapply((anomally.pos + length(train.data)), ApplyKolmogorovTest, all.data)
  return(result)
}
