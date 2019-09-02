## EXAMPLE 1: Classic Processing ----------------------

## Generate data
set.seed(100)
n <- 350
x <- sample(1:100, n, replace = TRUE)
x[70:90] <- sample(110:115, 21, replace = TRUE)
x[25] <- 200
x[320] <- 170
df <- data.frame(timestamp = 1:n, value = x)

## Calculate anomalies
result <- IpSdEwma(
  data = df$value,
  n.train = 5,
  threshold = 0.01,
  l = 2
)
res <- cbind(df, result$result)

## Plot results
PlotDetections(res, title = "SD-EWMA ANOMALY DETECTOR")

## Reduce anomalies
res$is.anomaly <- ReduceAnomalies(res$is.anomaly, windowLength = 5)

## Plot results
PlotDetections(res, title = "SD-EWMA ANOMALY DETECTOR")


## EXAMPLE 2: Incremental Processing ----------------------

\donttest{
  # install.packages("stream")
  library("stream")

  # Generate data
  set.seed(100)
  n <- 350
  x <- sample(1:100, n, replace = TRUE)
  x[70:90] <- sample(110:115, 21, replace = TRUE)
  x[25] <- 200
  x[320] <- 170
  df <- data.frame(timestamp = 1:n, value = x)
  dsd_df <- DSD_Memory(df)

  # Initialize parameters for the loop
  last.res <- NULL
  red.res <- NULL
  res <- NULL
  nread <- 100
  numIter <- ceiling(n/nread)

  # Calculate anomalies
  for(i in 1:numIter) {
    # read new data
    newRow <- get_points(dsd_df, n = nread, outofpoints = "ignore")
    # calculate if it's an anomaly
    last.res <- IpSdEwma(
      data = newRow$value,
      n.train = 5,
      threshold = 0.01,
      l = 2,
      last.res = last.res$last.res
    )

    if(!is.null(last.res$result)){
      # reduce anomalies
      red.res <- ReduceAnomalies(last.res$result$is.anomaly,
                                 windowLength = 5, incremental = TRUE, last.res = red.res$last.res)
      last.res$result$is.anomaly <- red.res$result

      # prepare the result
      res <- rbind(res, cbind(newRow, last.res$result))
    }
  }

  # Plot results
  PlotDetections(res, title = "SD-EWMA ANOMALY DETECTOR")
}
