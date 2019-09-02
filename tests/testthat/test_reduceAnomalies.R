library(otsad)
context("Reduce Anomalies")

test_that("ReduceAnomalies gives the correct result", {

  data <- c(0,0,0,1,0,1,1,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,1)
  correct.results <- c(0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1)

  res <- ReduceAnomalies(data[1:7], windowLength = 4, incremental = TRUE)
  data[1:7] <- res$result
  res <- ReduceAnomalies(data[8:10], windowLength = 4, incremental = TRUE, last.res = res$last.res)
  data[8:10] <- res$result
  res <- ReduceAnomalies(data[11:14], windowLength = 4, incremental = TRUE, last.res = res$last.res)
  data[11:14] <- res$result
  res <- ReduceAnomalies(data[15:16], windowLength = 4, incremental = TRUE, last.res = res$last.res)
  data[15:16] <- res$result
  res <- ReduceAnomalies(data[17:23], windowLength = 4, incremental = TRUE, last.res = res$last.res)
  data[17:23] <- res$result

  expect_equal(data, correct.results)

})
