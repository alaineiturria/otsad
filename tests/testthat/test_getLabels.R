library(otsad)
context("Get windows limits")

test_that("GetLabels gives the correct result", {

  is.real.anomaly <- c(0,0,1,0,0,0,0,0,1,0,0,0,0,0,0)
  is.anomaly <- c(0,0,1,1,0,1,0,0,0,0,0,0,0,0,0)
  start.limit <- c(0,0,1,0,0,0,0,0,7,0,0,0,0,0,0)
  end.limit <- c(0,0,5,0,0,0,0,0,11,0,0,0,0,0,0)

  df <- data.frame(timestamp = 1:15, value = 1:15, is.real.anomaly, is.anomaly, start.limit, end.limit)
  res <- GetLabels(df)

  correct.res <- rep("tn", 15)
  correct.res[c(3,4)] <- "tp"
  correct.res[6] <- "fp"
  correct.res[9] <- "fn"

  expect_equal(res$label, correct.res)

})
