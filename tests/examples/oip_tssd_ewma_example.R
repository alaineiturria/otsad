## EXAMPLE 1: ----------------------
## It can be used in the same way as with OcpTsSdEwma passing the whole dataset
## as an argument.

## Generate data
set.seed(100)
n <- 200
x <- sample(1:100, n, replace = TRUE)
x[70:90] <- sample(110:115, 21, replace = TRUE)
x[25] <- 200
x[150] <- 170
df <- data.frame(timestamp = 1:n, value = x)

## Calculate anomalies
result <- OipTsSdEwma(
  data = df$value,
  n.train = 5,
  threshold = 0.01,
  l = 3,
  m = 20,
  to.next.iteration = NULL
)
res <- cbind(df, result$result)

## Plot results
PlotDetections(res, print.time.window = FALSE, title = "TSSD-EWMA ANOMALY DETECTOR")

## EXAMPLE 2: ----------------------
## You can use it in an incremental way. This is an example using the stream
## library. This library allows the simulation of streaming operation.
\donttest{
# install.packages("stream")
library("stream")


## Generate data
set.seed(100)
n <- 500
x <- sample(1:100, n, replace = TRUE)
x[70:90] <- sample(110:115, 21, replace = TRUE)
x[25] <- 200
x[320] <- 170
df <- data.frame(timestamp = 1:n, value = x)
dsd_df <- DSD_Memory(df)

## Initialize parameters for the loop
last.res <- NULL
res <- NULL
nread <- 50
numIter <- n%/%nread
m <- 20
dsd_df <- DSD_Memory(df)

## Calculate anomalies
for(i in 1:numIter) {
  # read new data
  newRow <- get_points(dsd_df, n = nread, outofpoints = "ignore")
  # calculate if it's an anomaly
  last.res <- OipTsSdEwma(
    data = newRow$value,
    n.train = 5,
    threshold = 0.01,
    l = 3,
    m = 20,
    to.next.iteration = last.res$to.next.iteration
  )
  # prepare result
  res <- rbind(res, cbind(newRow, last.res$result))
  if (!is.null(last.res$last.data.checked)) {
    res[res$i %in% last.res$last.data.checked$i, "is.anomaly"] <-
      last.res$last.data.checked$is.anomaly
  }
}

## Plot results
PlotDetections(res, title = "TSSD-EWMA ANOMALY DETECTOR")
}
