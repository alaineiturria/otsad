library(otsad)
context("Window Normalizer")

test_that("WindowNormalizer gives the correct result", {

  normalizer <- WindowNormalizer$new(wl = 3)

  expect_equal(normalizer$normalize(10), 10)
  expect_equal(round(normalizer$normalize(15),3), c(-0.707, 0.707))
  expect_equal(round(normalizer$normalize(20),3), c(-1, 0, 1))
  expect_equal(round(normalizer$normalize(10),3), c(0, 1, -1))
  expect_equal(round(normalizer$normalize(30),3), c(0, -1, 1))
  expect_equal(round(normalizer$normalize(15),3), c(-0.801, 1.121, -0.320))
  expect_equal(normalizer$denormalize(-0.3202563), 15)

})
