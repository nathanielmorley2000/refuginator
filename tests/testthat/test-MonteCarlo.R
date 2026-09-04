test_that("Temporal Monte Carlo works", {
  
  # CREATE COMPARISON DATA
  # Import presence data
  pathpresence <-  test_path("testdata", "presenceData.csv")
  datapresence <- read.csv(pathpresence, header = TRUE, sep = ",", quote = '"', check.names = FALSE)
  
  nit = 10000
  score <- monteCarlo(9, 5, 6, nit, datapresence) 
  result = score/nit
  
  # TEST
  expect_equal(result, 0.01, tolerance = 0.01)
})
