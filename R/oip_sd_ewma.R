#' @function OipSdEwma
#' @description Optimized Incremental Processing Shift-Detection based on EWMA
#' @param data data
#' @param n.train test data
#' @param beta beta value
#' @param threshold threshold
#' @param l sigma multiplier
#'
#' @return list composed by Result (dataset containing if anomaly and upper and lower limit) and last.res (dataset containing last computation result)

OipSdEwma <- function(data, n.train, threshold, l = 3, last.res = NULL) {
  
  SdEwmaTrain <- function(x, env) {
    train.set <- get("last.res", envir = env)
    train.set$x <- x
    train.set$i <- train.set$i + 1
    train.set$z.ant <- train.set$z
    train.set$std.ant <- train.set$std
    train.set$z <- train.set$lambda * train.set$x + (1 - train.set$lambda) * train.set$z.ant
    train.set$error <- train.set$x - train.set$z.ant
    train.set$error.sum <- train.set$error.sum + train.set$error ^ 2
    train.set$std <- train.set$error.sum / train.set$i
    assign("last.res", train.set, env)
    return (NULL)
  }
  
  SdEwmaTest <- function(x, env) {
    row <- get("last.res", envir = env)
    row$i <- row$i + 1
    row$x <- x
    row$z.ant <- row$z
    row$std.ant <- row$std
    row$z <- row$lambda * row$x + (1 - row$lambda) * row$z.ant
    row$error <- row$x - row$z.ant
    row$std <- threshold * row$error ^ 2 + (1 - threshold) * row$std.ant
    row$ucl <- row$z.ant + l[1] * sqrt(row$std.ant)
    row$lcl <- row$z.ant - l[1] * sqrt(row$std.ant)
    row$is.anomaly <- ifelse(row$x < row$lcl | row$x > row$ucl, 1, 0)
    assign("last.res", row, env)
    return (row[,c("is.anomaly", "lcl", "ucl")])
  }
  
  # Initialize the parameters
  new.enviroment <- new.env()
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
  assign("last.res", last.res, envir = new.enviroment)
  
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
    sapply(train.data, SdEwmaTrain, new.enviroment)
    res <- NULL
  }
  
  last.res <- get("last.res", envir = new.enviroment)
  if (unique(last.res$i) == n.train) {
    last.res <- last.res[last.res$error.sum == min(last.res$error.sum),]
    assign("last.res", last.res, envir = new.enviroment)
  }
  
  # Testing phase
  if (!is.null(test.data)) {
    res <- as.data.frame(t(sapply(test.data, SdEwmaTest, new.enviroment)))
    res <- data.frame(is.anomaly = unlist(res$is.anomaly), lcl = unlist(res$lcl), ucl = unlist(res$ucl))
  }
  
  last.res <- get("last.res", envir = new.enviroment)
  return(list(result = res, last.res = last.res))
}