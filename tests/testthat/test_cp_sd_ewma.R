test_that("CpSdEwma gives the correct result",{
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
    train.data = df[1:5,"value"],
    test.data = df[6:n,"value"],
    threshold = 0.01,
    l = 3
  )
  res <- cbind(df[6:n,], result)
  rownames(res) <- 1:(n-5)

  ## read correct results
  # correct.results <- read.csv("tests/correct_result.csv", stringsAsFactors = FALSE)
  correct.results <- 1:500

  expect_equal(res[,c("value","is.anomaly")], correct.results[,c("value","is.anomaly")])
})




