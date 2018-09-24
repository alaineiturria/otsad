library(otsad)
context("Optimized Clasic Processing Pewma")

test_that("OcpPewma gives the correct result", {
  ## Generate data
  set.seed(100)
  n <- 500
  x <- sample(1:100, n, replace = TRUE)
  x[70:90] <- sample(110:115, 21, replace = TRUE)
  x[25] <- 200
  x[320] <- 170
  df <- data.frame(timestamp=1:n,value=x)

  ## Calculate anomalies
  result <- OcpPewma(
    data = df$value,
    n.train = 5,
    alpha0 = 0.95,
    beta = 0.6,
    l = 3
  )

  ## read correct results
  correct.results <- rep(0, 500)
  correct.results[c(25, 70, 92, 320)] <- 1

  expect_equal(as.numeric(result$is.anomaly), correct.results)

})




