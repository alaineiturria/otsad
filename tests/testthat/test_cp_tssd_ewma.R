library(otsad)
context("Clasic Processing Tssd-Ewma")

test_that("CpTsSdEwma gives the correct result", {
  ## Generate data
  set.seed(100)
  n <- 500
  x <- sample(1:100, n, replace = TRUE)
  x[70:90] <- sample(110:115, 21, replace = TRUE)
  x[25] <- 200
  x[320] <- 170
  df <- data.frame(timestamp=1:n,value=x)

  ## Calculate anomalies
  result <- CpTsSdEwma(
    train.data = df[1:5,"value"],
    test.data = df[6:n,"value"],
    threshold = 0.01,
    l = 3,
    m = 20
  )

  ## read correct results
  correct.results <- rep(0, 495)
  correct.results[87] <- 1

  expect_equal(as.numeric(result$is.anomaly), correct.results)

})
