library(otsad)
context("Contextual Anomaly Detector")


test_that("Comparison with Python version",{
  set.seed(123)
  data <- c(rnorm(10), rnorm(10, mean = 100)) 
      
expect_equal(unname(ContextualAnomalyDetector(data, rest.period = 2)$result[11,"anomaly.score"]) > 0.75, TRUE)
})
