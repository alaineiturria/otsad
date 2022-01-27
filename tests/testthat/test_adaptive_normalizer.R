library(otsad)
context("One-pass Adaptive Normalizalizer")

test_that("AdaptiveNormalizer gives the correct result", {

  normalizer <- AdaptiveNormalizer$new(wl = 3)

  res1 <- c(-0.963, -0.963,  0.019, -0.308,  1.000, -1.000)
  res2 <- c(-0.927, -0.927,  0.358, -1.000, -1.000,  1.000)
  res3 <- c(-0.981, -0.981, -1.000,  1.000, -0.486, -0.486)

  expect_equal(normalizer$normalize(10), 0)
  expect_equal(normalizer$normalize(15), 0)
  expect_equal(normalizer$normalize(20), 0)
  expect_equal(round(as.vector(normalizer$normalize(10)),3), res1)
  expect_equal(round(as.vector(normalizer$normalize(30)),3), res2)
  expect_equal(round(as.vector(normalizer$normalize(15)),3), res3)
  expect_equal(normalizer$denormalize(-0.4858841), 15)

})
