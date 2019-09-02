library(otsad)
context("Get Null And Perfect Scores")

test_that("GetNullAndPerfectScores gives the correct result", {

  is.real.anomaly <- rep(0,180)
  is.real.anomaly[c(1,50,60,70)] <- 1
  df <- data.frame(timestamp = 1:180, value = 1:180, is.real.anomaly)
  res1 <- GetNullAndPerfectScores(df)

  df2 <- data.frame(timestamp = 1, value = 1:10, is.real.anomaly = rep(0,10))
  res2 <- GetNullAndPerfectScores(df2)

  expect_equal(res1$null.score, c(-4,-4,-8))
  expect_equal(res1$perfect.score, c(4, 4, 4))
  expect_equal(res2$null.score, c(0, 0, 0))
  expect_equal(res2$perfect.score, c(0, 0, 0))

})
