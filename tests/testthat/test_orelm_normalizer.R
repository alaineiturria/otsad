library(otsad)
context("Orelm Normalizer")

test_that("OrelmNormalizer gives the correct result", {

  normalizer <- OrelmNormalizer$new(wl = 3, method = "DN")

  expect_equal(as.vector(normalizer$normalize(10)), c(10, NA, 10, 10))
  expect_equal(as.vector(round(normalizer$normalize(15),3)), c(10, 0.707))
  expect_equal(as.vector(round(normalizer$normalize(20),3)),  c(10, 0.707, 0.707, 1))
  expect_equal(as.vector(round(normalizer$normalize(10),3)), c(0.707, 1, 1, -0.783))
  expect_equal(as.vector(round(normalizer$normalize(30),3)), c(1.000, -0.783, -0.783, 1.554))
  expect_equal(as.vector(round(normalizer$normalize(15),3)), c(-0.783, 1.554, 1.554, -0.221))

})
