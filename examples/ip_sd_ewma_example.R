## EXAMPLE 1: ----------------------
## It can be used in the same way as with CpSdEwma passing the whole dataset as
## an argument.

## Generate data
set.seed(100)
n <- 500
x <- sample(1:100, n, replace = TRUE)
x[70:90] <- sample(110:115, 21, replace = TRUE)
x[25] <- 200
x[320] <- 170
df=data.frame(timestamp=1:n,value=x)

## Calculate anomalies
result <- IpSdEwma(
  data = df$value,
  n.train = 5,
  threshold = 0.01,
  l = 3
)
res <- cbind(df[6:n,], result$result)
rownames(res) <- 1:(n-5)

## Plot results
y.limits <- c(-150,250)
plot(x = res$timestamp, y = res$value, type = "l", ylim = y.limits,
     xlab = "timestamp", ylab = "value", main = "SD-EWMA ANOMALY DETECTOR")
points(x = res[res$is.anomaly == 1, "timestamp"],
       y = res[res$is.anomaly == 1, "value"], pch=4, col="red", lwd = 2)
par(new=TRUE)
plot(x = res$timestamp, y = res$ucl, type="l", col="red", xaxt="n",
     ylim = y.limits, xlab = "", ylab = "")
par(new=TRUE)
plot(x = res$timestamp, y = res$lcl, type="l", col="red", xaxt="n",
     ylim = y.limits, xlab = "", ylab = "")

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

## Calculate anomalies
for(i in 1:numIter) {
  # read new data
  newRow <- get_points(dsd_df, n = nread, outofpoints = "ignore")
  # calculate if it's an anomaly
  last.res <- IpSdEwma(
    data = newRow$value,
    n.train = 5,
    threshold = 0.01,
    l = 3,
    last.res = last.res$last.res
  )
  # prepare the result
  if(!is.null(last.res$result)){
    res <- rbind(res, cbind(newRow[(nread-nrow(last.res$result)+1):nread,],
                 last.res$result))
  }
}
print(res)

## Plot results
n.train <- 5
rownames(res) <- 1:(n-n.train)
y.limits <- c(-150,250)
plot(x = res$timestamp, y = res$value, type = "l", ylim = y.limits,
     xlab = "timestamp", ylab = "value", main = "SD-EWMA ANOMALY DETECTOR")
points(x = res[res$is.anomaly == 1, "timestamp"],
       y = res[res$is.anomaly == 1, "value"], pch=4, col="red", lwd = 2)
par(new=TRUE)
plot(x = res$timestamp, y = res$ucl, type="l", col="red", xaxt="n",
     ylim = y.limits, xlab = "", ylab = "")
par(new=TRUE)
plot(x = res$timestamp, y = res$lcl, type="l", col="red", xaxt="n",
     ylim = y.limits, xlab = "", ylab = "")


