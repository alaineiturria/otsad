library(otsad)
context("Clasic Processing Sd-Ewma")

test_that("CpSdEwma gives the correct result", {
  ## Generate data
  set.seed(100)
  n <- 500
  x <- sample(1:100, n, replace = TRUE)
  x[70:90] <- sample(110:115, 21, replace = TRUE)
  x[25] <- 200
  x[320] <- 170
  df <- data.frame(timestamp=1:n,value=x)

  ## Calculate anomalies
  result <- CpSdEwma(
    data = df$value,
    n.train = 5,
    threshold = 0.01,
    l = 3
  )

  ## read correct results
  correct.results <- rep(0, 500)
  correct.results[c(25, 92, 320)] <- 1

  expect_equal(as.numeric(result$is.anomaly), correct.results)

})




