## Generate data
set.seed(100)
n <- 500
x <- sample(1:100, n, replace = TRUE)
x[70:90] <- sample(110:115, 21, replace = TRUE)
x[25] <- 200
x[320] <- 170
df <- data.frame(timestamp = 1:n, value = x)

## Calculate anomalies
result <- OcpSdEwma(
  train.data = df[1:5,"value"],
  test.data = df[6:n,"value"],
  threshold = 0.01,
  l = 3
)
res <- cbind(df[6:n,], result)
rownames(res) <- 1:(n-5)

## Plot results
PlotDetections(res, print.time.window = FALSE, title = "SD-EWMA ANOMALY DETECTOR")
