library(otsad)
context("Get windows limits")

test_that("GetWindowsLimits gives the correct result", {

  is.real.anomaly <- c(0,0,1,0,0,0,0,1,0,1,0,0,0,0,0)
  correct.start <- c(0,0,1,0,0,0,0,6,0,0,0,0,0,0,0)
  correct.end <- c(0,0,5,0,0,0,0,12,0,0,0,0,0,0,0)
  df <- data.frame(timestamp = 1:15, value = 1:15, is.real.anomaly, stringsAsFactors = FALSE)
  data <- GetWindowsLimits(df, 4)

  correct.strart0 <- rep(0,15)
  correct.end0 <- rep(0,15)
  df <- data.frame(timestamp = 1:15, value = 1:15, is.real.anomaly = rep(0,15), stringsAsFactors = FALSE)
  data1 <- GetWindowsLimits(df)

  expect_equal(data$start.limit, correct.start)
  expect_equal(data$start.limit, correct.start)
  expect_equal(data1$start.limit, correct.strart0)
  expect_equal(data1$start.limit, correct.end0)

})
