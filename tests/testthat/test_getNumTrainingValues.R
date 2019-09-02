library(otsad)
context("Get Number of Training Values")

test_that("GetNumTrainingValues gives the correct result", {

  expect_equal(GetNumTrainingValues(750), 112)
  expect_equal(GetNumTrainingValues(8000), 750)

})
