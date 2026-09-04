test_that("findMapData() correctly transforms data", {
  
  # LOAD COMPARISON DATA
  # With site/dataset ID
  pathwith <- test_path("testdata", "inputData_siteid_datasetid.csv")
  datawith <- read.csv(pathwith, header = TRUE, sep = ",", quote = '"', check.names = FALSE)
  
  # Without site/dataset ID
  pathwithout <- test_path("testdata", "inputData_nositeid_nodatasetid.csv")
  datawithout <- read.csv(pathwithout, header = TRUE, sep = ",", quote = '"', check.names = FALSE)

  # Expected result
  pathexpected <- test_path("testdata", "mapData.csv")
  dataexpected <- read.csv(pathexpected)
  dataexpected$time <- as.factor(dataexpected$time) # convert to factor to match the data the function should produce
  
  
  # TEST
  # Test with site/dataset ID is correct
  expect_equal(as.data.frame(findMapData(datawith)), as.data.frame(dataexpected))
  
  # Test without site/dataset ID is correct
  expect_equal(as.data.frame(findMapData(datawithout)), dataexpected)
  
  # Test with and without producing same result
  expect_equal(findMapData(datawith), findMapData(datawithout))
})
