#' @function OcpPewma
#' @description Optimized Clasic Processing Probabilistic-EWMA
#' @param data vector of values to analyze
#' @param alpha0 initial alpha value
#' @param beta beta value
#' @param n.train number of points to train
#' @param l sigma multiplier
#'
#' @return dataset containing if it's anomaly and upper and lower limits


# Pewma CONTROL CHART
OcpPewma <- function(data, alpha0 = 0.2, beta = 0, n.train = 5, l = 3) {
  
  # Pewma
  Pewma <- function(x, env) {
    row <- get("last.res", envir = env)
    row$i <- row$i + 1
    row$x <- x
    row$s1 <- row$s1.next
    row$std <- row$std.next
    row$z <- ifelse(row$std == 0, 0, (row$x - row$s1) / row$std)
    row$p <- 1 / sqrt(2 * pi)*exp(-(row$z ^ 2) / 2)
    row$alpha <- ifelse(row$i <= n.train, 1 - 1/row$i, (1 - beta * row$p) * row$alpha)
    row$s1 <- row$alpha * row$s1 + (1 - row$alpha) * row$x
    row$s2 <- row$alpha * row$s2 + (1 - row$alpha) * row$x ^ 2
    row$s1.next <- row$s1
    row$std.next <- sqrt(abs(row$s2 - row$s1 ^ 2))
    row$ucl <- row$s1 + l[1] * row$std
    row$lcl <- row$s1 - l[1] * row$std
    row$is.anomaly <- row$x < row$lcl | row$x > row$ucl
    assign("last.res", row, env)
    return(row[c("is.anomaly", "ucl", "lcl")])
  }
  
  # inicializamos las variables
  new.enviroment <- new.env()
  last.res <- data.frame(value = data[1], 
                         i = 0,
                         s1 = data[1], 
                         s2 = data[1] ^ 2, 
                         s1.next = data[1], 
                         std.next = 0, 
                         std = 0, 
                         z = 0, 
                         p = 0, 
                         is.anomaly = 0, 
                         lcl = 0, 
                         ucl = 0, 
                         alpha = alpha0)
  
  assign("last.res", last.res, envir = new.enviroment)
  res <- as.data.frame(t(sapply(data, Pewma, new.enviroment)))
  
  return(res)
}
