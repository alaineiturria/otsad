library(otsad)
context("Contextual Anomaly Detector")


test_that("Comparison with Python version",{
  library(reticulate)
  
  np <- import("numpy", convert = TRUE)
  np$random$seed(as.integer(100))
  data <- np$random$normal(0,1,as.integer(10))
  data2 <- np$random$normal(100,1,as.integer(10))
  data <- c(data, data2) 
  resultados.python <- read.csv("~/Documentos/Experimentos/Alaine/CAD/resultado_python.csv", header = F)[ ,1]
  limit.ok <- 200
      
  expect_equal(resultados.python[1:limit.ok], 
               ContextualAnomalyDetector(data[1:limit.ok], rest.period = 2))
})
