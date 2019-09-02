library(otsad)
context("Get detector score")

test_that("GetDetectorScore gives the correct result", {
  n <- 180
  x <-  c(59,4,36,39,21,14,39,27,71,41,27,41,20,83,53,40,58,98,66,34,34,95,39,57,200,14,24,72,30,52,
          28,37,44,81,53,70,85,85,40,16,64,29,93,16,97,1,71,64,78,90,52,75,93,10,50,20,100,4,22,90,
          12,37,17,17,98,82,96,40,86,115,112,115,113,115,114,111,110,112,115,111,113,115,115,112,
          115,113,114,115,113,111,91,74,92,39,86,6,26,37,37,29,39,35,28,81,100,76,61,60,59,24,46,
          100,9,6,50,6,96,64,27,100,36,38,34,11,58,34,1,100,37,23,75,31,41,40,45,78,21,75,59,33,38,
          3,49,98,1,11,44,32,98,170,74,67,73,12,22,63,96,82,3,69,84,81,85,19,10,45,61,87,86,70,65,
          66,89,86,85,33,55,74,81,21)
  df <- data.frame(timestamp=1:n,value=x)
  df$is.real.anomaly <- 0
  df[c(25,80,150), "is.real.anomaly"] <- 1
  df$is.anomaly <- 0
  df[c(18,25,96,150), "is.anomaly"] <- 1

  # Get detector score
  scores <- GetDetectorScore(df, print = FALSE, title = "")


  expect_equal(round(scores$standard, 2), round(0.5869384, 2))
  expect_equal(round(scores$low_FP_rate, 2), round(0.3669427, 2))
  expect_equal(round(scores$low_FN_rate, 2), round(-0.4130616, 2))
  expect_equal(scores$tp, 2)
  expect_equal(scores$tn, 175)
  expect_equal(scores$fp, 2)
  expect_equal(scores$fn, 1)


})
