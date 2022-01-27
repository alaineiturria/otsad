library(otsad)
context("One-pass Adaptive Min-Max Normalizer")

test_that("AdaptiveNormalizer2 gives the correct result", {

  normalizer <- AdaptiveNormalizer2$new(wl = 3)

  expect_equal(normalizer$normalize(10), 0)
  expect_equal(normalizer$normalize(15), 0)
  expect_equal(normalizer$normalize(20), c(0, 0, 0))
  expect_equal(normalizer$normalize(10), c(0.5, 1, 0))
  expect_equal(normalizer$normalize(30), c(0.5, 0, 1))
  expect_equal(normalizer$normalize(15), c(0, 1, 0.25))
  expect_equal(normalizer$denormalize(0.25), 15)

})
