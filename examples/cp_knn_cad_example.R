## Generate data
set.seed(100)
n <- 500
x <- sample(1:100, n, replace = TRUE)
x[70:90] <- sample(110:115, 21, replace = TRUE)
x[25] <- 200
x[320] <- 170
df <- data.frame(timestamp=1:n,value=x)

## Set parameters
params.KNN <- list(threshold = 0.99, n.train = 75, l = 19, n = 30, m = 25, k = 17)

## Calculate anomalies
result <- CpKnnCad(
  data = df$value,
  n.train = params.KNN$n.train,
  threshold = params.KNN$threshold,
  l = params.KNN$l,
  n = params.KNN$n,
  m = params.KNN$m,
  k = params.KNN$k,
  ncm.type = "ICAD",
  reducefp = TRUE
)

## Plot results
res <- cbind(df[(params.KNN$n.train + 1):n,], as.data.frame(result))
y.limits <- c(-150,250)
plot(x = res$timestamp, y = res$value, type = "l", ylim = y.limits,
     xlab = "timestamp", ylab = "value", main = "KNN-CAD ANOMALY DETECTOR")
points(x = res[res$is.anomaly == 1, "timestamp"],
       y = res[res$is.anomaly == 1, "value"], pch=4, col="red", lwd = 2)

