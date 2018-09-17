library(otsad)
context("Optimized Clasic Processing Tssd-Ewma")

test_that("OcpTsSdEwma gives the correct result", {
  ## Generate data
  set.seed(100)
  n <- 500
  x <- sample(1:100, n, replace = TRUE)
  x[70:90] <- sample(110:115, 21, replace = TRUE)
  x[25] <- 200
  x[320] <- 170
  df <- data.frame(timestamp=1:n,value=x)

  ## Calculate anomalies
  result <- OcpTsSdEwma(
    data = df$value,
    n.train = 5,
    threshold = 0.01,
    l = 3,
    m = 20
  )

  ## read correct results
  correct.results <- rep(0, 500)
  correct.results[92] <- 1

  expect_equal(as.numeric(result$is.anomaly), correct.results)

})
