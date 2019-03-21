library(otsad)
context("Contextual Anomaly Detector")

# helper function to skip tests if we don't have the 'bencode' module
skip_if_no_libraries <- function() {
  reticulate::use_virtualenv("r-reticulate")
  have_bencode <- reticulate::py_module_available("bencode")
  
  if (!have_bencode){
    skip("bencode not available for testing")
  }
}

test_that("CAD with skip", {
  skip_if_no_libraries()
  
  set.seed(123)
  data <- c(rnorm(10), rnorm(10, mean = 100)) 
  
  expect_equal(unname(ContextualAnomalyDetector(data, rest.period = 2)$result[11,"anomaly.score"]) > 0.75, TRUE)
})

