## Generate data
set.seed(100)
n <- 500
x <- sample(1:100, n, replace = TRUE)
x[70:90] <- sample(110:115, 21, replace = TRUE)
x[25] <- 200
x[320] <- 170
df <- data.frame(timestamp = 1:n, value = x)

## Calculate anomalies
result <- ContextualAnomalyDetector(data = df$value, rest.period = 10, base.threshold = 0.9)

## Plot results
res <- cbind(df, result$result)
PlotDetections(res, title = "CAD_OSE ANOMALY DETECTOR")

