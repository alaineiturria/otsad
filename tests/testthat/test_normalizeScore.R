library(otsad)
context("Normalize Score using Max and Min normalization")

test_that("NormalizeScore gives the correct result", {

  expect_equal(NormalizeScore(9, 2, 1), 800)
  expect_equal(NormalizeScore(9, 1, 0), 900)
  expect_equal(NormalizeScore(9, 0, 0), 9)

})
