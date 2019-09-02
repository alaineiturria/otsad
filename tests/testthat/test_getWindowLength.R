library(otsad)
context("Get Window Length")

test_that("GetWindowLength gives the correct result", {
  expect_equal(GetWindowLength(data.length = 180, num.real.anomaly = 3), 6)
})
