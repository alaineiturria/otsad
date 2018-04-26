## Generate data
set.seed(100)
n <- 500
x <- sample(1:100, n, replace = TRUE)
x[70:90] <- sample(110:115, 21, replace = TRUE)
x[25] <- 200
x[320] <- 170
df=data.frame(timestamp=1:n,value=x)

## Calculate anomalies
result <- CpSdEwma(
  train.data = df[1:5,"value"],
  test.data = df[6:n,"value"],
  threshold = 0.01,
  l = 3
)
res <- cbind(df[6:n,], result)
rownames(res) <- 1:(n-5)

## Plot results
y.limits <- c(-150,250)
plot(x = res$timestamp, y = res$value, type = "l", ylim = y.limits, xlab = "timestamp", ylab = "value", main = "PEWMA ANOMALY DETECTOR")
points(x = res[res$is.anomaly == 1, "timestamp"], y = res[res$is.anomaly == 1, "value"], pch=4, col="red", lwd = 2)
par(new=TRUE)
plot(x = res$timestamp, y = res$ucl, type="l", col="red", xaxt="n", ylim = y.limits, xlab = "", ylab = "")
par(new=TRUE)
plot(x = res$timestamp, y = res$lcl, type="l", col="red", xaxt="n", ylim = y.limits, xlab = "", ylab = "")
