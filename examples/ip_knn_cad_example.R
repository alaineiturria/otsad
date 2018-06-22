## EXAMPLE 1: ----------------------
## It can be used in the same way as with CpKnnCad passing the whole dataset as
## an argument.

## Generate data
set.seed(100)
n <- 500
x <- sample(1:100, n, replace = TRUE)
x[70:90] <- sample(110:115, 21, replace = TRUE)
x[25] <- 200
x[320] <- 170
df <- data.frame(timestamp=1:n,value=x)

## Set parameters
params.KNN <- list(threshold = 1, n.train = 50, l = 19, k = 17)

## Calculate anomalies
result <- IpKnnCad(
  data = df$value,
  n.train = params.KNN$n.train,
  threshold = params.KNN$threshold,
  l = params.KNN$l,
  k = params.KNN$k,
  ncm.type = "ICAD",
  reducefp = TRUE
)

## Plot results
res <- cbind(df[(params.KNN$n.train + 1):n,],
  is.anomaly = result$is.anomaly[(params.KNN$n.train + 1):n])
y.limits <- c(-150,250)
plot(x = res$timestamp, y = res$value, type = "l", ylim = y.limits,
  xlab = "timestamp", ylab = "value", main = "KNN-CAD ANOMALY DETECTOR")
points(x = res[res$is.anomaly == TRUE, "timestamp"],
  y = res[res$is.anomaly == TRUE, "value"], pch=4, col="red", lwd = 2)

## EXAMPLE 2: ----------------------
## You can use it in an incremental way. This is an example using the stream
## library. This library allows the simulation of streaming operation.

# install.packages("stream")
library("stream")

## Generate data
set.seed(100)
n <- 500
x <- sample(1:100, n, replace = TRUE)
x[70:90] <- sample(110:115, 21, replace = TRUE)
x[25] <- 200
x[320] <- 170
df=data.frame(timestamp=1:n,value=x)
dsd_df <- DSD_Memory(df)

## Initialize parameters for the loop
last.res <- NULL
res <- NULL
nread <- 100
numIter <- n%/%nread

## Set parameters
params.KNN <- list(threshold = 1, n.train = 50, l = 19, k = 17)

## Calculate anomalies
for(i in 1:numIter) {
  # read new data
  newRow <- get_points(dsd_df, n = nread, outofpoints = "ignore")
  # calculate if it's an anomaly
  last.res <- IpKnnCad(
    data = newRow$value,
    n.train = params.KNN$n.train,
    threshold = params.KNN$threshold,
    l = params.KNN$l,
    k = params.KNN$k,
    ncm.type = "ICAD",
    reducefp = TRUE,
    to.next.iteration = last.res$to.next.iteration
  )
  # prepare the result
  if(!is.null(last.res$is.anomaly)){
    res <- rbind(res, cbind(newRow, is.anomaly = last.res$is.anomaly))
  }
}

## Plot results
res <- res[(params.KNN$n.train + 1):n,]
y.limits <- c(-150,250)
plot(x = res$timestamp, y = res$value, type = "l", ylim = y.limits,
  xlab = "timestamp", ylab = "value", main = "KNN-CAD ANOMALY DETECTOR")
points(x = res[res$is.anomaly == 1, "timestamp"],
  y = res[res$is.anomaly == 1, "value"], pch=4, col="red", lwd = 2)




