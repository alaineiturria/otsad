#' @function IpSdEwma
#' @description Incremental Processing Shift-Detection based on EWMA
#' @param data data
#' @param n.train test data
#' @param beta beta value
#' @param threshold threshold
#' @param l sigma multiplier
#'
#' @return list composed by Result (dataset containing if anomaly and upper and lower limit) and last.res (dataset containing last computation result)

IpSdEwma <- function(data, n.train, threshold, l = 3, last.res = NULL) {
  
  SdEwmaTrain <- function(row, x) {
    row$x <- x
    row$i <- row$i + 1
    row$z.ant <- row$z
    row$std.ant <- row$std
    row$z <- row$lambda * row$x + (1 - row$lambda) * row$z.ant
    row$error <- row$x - row$z.ant
    row$error.sum <- row$error.sum + row$error^2
    row$std <- row$error.sum / row$i
    row
  }
  
  SdEwmaTest <- function(row, x) {
    row$i <- row$i + 1
    row$x <- x
    row$z.ant <- row$z
    row$std.ant <- row$std
    row$z <- row$lambda * row$x + (1 - row$lambda) * row$z.ant
    row$error <- row$x - row$z.ant
    row$std <- threshold * row$error ^ 2 + (1 - threshold) * row$std.ant
    row$ucl <- row$z.ant + l[1] * sqrt(row$std.ant)
    row$lcl <- row$z.ant - l[1] * sqrt(row$std.ant)
    row$is.anomaly <- row$x < row$lcl | row$x > row$ucl
    row
  }
  
  
  # Initialize the parameters
  lambdas <- seq(0.1, 1, 0.1)
  if (is.null(last.res)) {
    last.res <- data.frame(lambda = lambdas, 
                            i = 0, 
                            x = data[1], 
                            z.ant = 0, 
                            std.ant = 0, 
                            z = data[1], 
                            error = 0, 
                            std = 0, 
                            error.sum = 0)
  }
  res <- NULL
  
  # prepare train and test
  n <- length(data)
  aux <- n.train - unique(last.res$i)
  if (n <= aux) {
    train.data <- data
    test.data <- NULL
  } else if (aux <= 0) {
    train.data <- NULL
    test.data <- data
  } else {
    train.data <- data[1:aux]
    test.data <- data[(aux + 1):n]
  }
  
  # Training phase
  if (!is.null(train.data)) {
    for (i in 1:length(train.data)) {
      last.res <- SdEwmaTrain(last.res, train.data[i])
    }
  }
  if (unique(last.res$i) == n.train) {
    last.res <- last.res[last.res$error.sum == min(last.res$error.sum),]
  }
  
  # Testing phase
  if (!is.null(test.data)) {
    for (i in 1:length(test.data)) {
      last.res <- SdEwmaTest(last.res, test.data[i])
      res <- rbind(res, last.res[,c("is.anomaly", "lcl", "ucl")])
    }
  }
  
  return(list(result = res, last.res = last.res))
}
