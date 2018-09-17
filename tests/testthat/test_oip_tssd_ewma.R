library(otsad)
context("Optimized Incremental Processing TsSd-Ewma")

test_that("OipTsSdEwma gives the correct result", {

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
  m <- 20

  ## Calculate anomalies
  for(i in 1:numIter) {
    # read new data
    newRow <- df[(iterador+1):(iterador+nread),]
    # calculate if it's an anomaly
    last.res <- OipTsSdEwma(
      data = newRow$value,
      n.train = 5,
      threshold = 0.01,
      l = 3,
      m = m,
      to.next.iteration = last.res$to.next.iteration
    )
    # prepare result
    if(!is.null(last.res$last.data.checked)){
      res <- rbind(res, cbind(last.timestamp, last.res$last.data.checked))
    }
    if(!is.null(last.res$checked.results)){
      init <- nread - (nrow(last.res$checked.results) +
          nrow(last.res$to.next.iteration$to.check)) + 1
      end <- init + nrow(last.res$checked.results) - 1
      res <- rbind(res, cbind(newRow[init:end,], last.res$checked.results))
    }
    if(i == numIter){
      res <- rbind(res,
        cbind(timestamp = newRow[
          (nread - nrow(last.res$to.next.iteration$to.check)
            + 1):nread, "timestamp"],
          last.res$to.next.iteration$to.check))
    }
    last.timestamp <- newRow[(nread-m+1):nread,]
    iterador <- iterador + nread
  }

  ## read correct results
  correct.results <- rep(0, 500)
  correct.results[92] <- 1

  expect_equal(as.numeric(res$is.anomaly), correct.results)

})
