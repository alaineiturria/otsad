#' @function OcpSdEwma
#' @description Optimized Classic Processing Shift-Detection based on EWMA
#' @param train.data training data
#' @param test.data test data
#' @param beta beta value
#' @param threshold threshold
#' @param l sigma multiplier
#'
#' @return dataset containing if anomaly and upper and lower limit

OcpSdEwma <- function(train.data, test.data, threshold, l = 3) {
  
  SdEwmaTrain <- function(x, env) {
    train.set <- get("last.res", envir = env)
    train.set$x <- x
    train.set$i <- train.set$i + 1
    train.set$z.ant <- train.set$z
    train.set$std.ant <- train.set$std
    train.set$z <- train.set$lambda * train.set$x + (1 - train.set$lambda) * train.set$z.ant
    train.set$error <- train.set$x - train.set$z.ant
    train.set$error.sum <- train.set$error.sum + train.set$error ^ 2
    train.set$std <- train.set$error.sum  /train.set$i
    assign("last.res", train.set, env)
    row <- get("last.res", envir = env)
    train.set
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
    row[,c("is.anomaly", "ucl", "lcl")]
  }
  
  # Initialize the parameters
  new.enviroment <- new.env()
  lambdas <- seq(0.1, 1, 0.1)
  last.res <- data.frame(lambda = lambdas,
                        i = 1,
                        x = train.data[1], 
                        z.ant = 0, 
                        std.ant = 0, 
                        z = mean(train.data), 
                        error = 0, 
                        std = 0, 
                        error.sum = 0)
  assign("last.res", last.res, envir = new.enviroment)
  
  # Training phase
  sapply(train.data, SdEwmaTrain, new.enviroment)
  last.res <- get("last.res", envir = new.enviroment)
  last.res <- last.res[last.res$error.sum == min(last.res$error.sum),]
  assign("last.res", last.res, envir = new.enviroment)
  
  # Testing phase
  res <- as.data.frame(t(sapply(test.data, SdEwmaTest, new.enviroment)))
  res <- as.data.frame(lapply(res, unlist))
  
  return(res)
}
