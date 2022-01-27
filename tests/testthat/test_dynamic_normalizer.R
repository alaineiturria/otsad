library(otsad)
context("Dinamyc Normalizer")

test_that("DynamicNormalizer gives the correct result", {

  normalizer <- DynamicNormalizer$new()

  expect_equal(normalizer$normalize(10), 10)
  expect_equal(round(normalizer$normalize(15),3), 0.707)
  expect_equal(round(normalizer$normalize(20),3), 1)
  expect_equal(round(normalizer$normalize(10),3), -0.783)
  expect_equal(round(normalizer$normalize(30),3), 1.554)
  expect_equal(round(normalizer$normalize(15),3), -0.221)
  expect_equal(normalizer$denormalize(-0.2214036), 15)

})
