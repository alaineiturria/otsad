library(otsad)
context("Optimized Incremental Processing Sd-Ewma")

test_that("OipSdEwma gives the correct result", {
  ## Generate data
  set.seed(100)
  n <- 500
  x <- sample(1:100, n, replace = TRUE)
  x[70:90] <- sample(110:115, 21, replace = TRUE)
  x[25] <- 200
  x[320] <- 170
  df <- data.frame(timestamp=1:n,value=x)

  ## Initialize parameters for the loop
  last.res <- NULL
  res <- NULL
  nread <- 250
  numIter <- n%/%nread
  iterador <- 0

  ## Calculate anomalies
  for(i in 1:numIter) {
    # read new data
    newRow <- df[(iterador+1):(iterador+nread),]
    # calculate if it's an anomaly
    last.res <- OipSdEwma(
      data = newRow$value,
      n.train = 5,
      threshold = 0.01,
      l = 3,
      last.res = last.res$last.res
    )
    # prepare the result
    if(!is.null(last.res$result)){
      res <- rbind(res, cbind(newRow[(nread-nrow(last.res$result)+1):nread,], last.res$result))
    }
    iterador <- iterador + nread
  }

  ## read correct results
  correct.results <- rep(0, 495)
  correct.results[c(20,87,315)] <- 1

  expect_equal(as.numeric(res$is.anomaly), correct.results)

})
