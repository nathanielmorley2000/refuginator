test_that("summarizeData() correctly summarizes data", {
  
  # LOAD COMPARISON DATA
  # Without site/dataset ID (accurate representation of data at this point of pipeline)
  pathwithout <- test_path("testdata", "inputData_nositeid_nodatasetid.csv")
  datawithout <- read.csv(pathwithout, header = TRUE, sep = ",", quote = '"', check.names = FALSE)
  
  # With site/dataset ID (simulates extra columns - removed earlier in code)
  pathwith <- test_path("testdata", "inputData_siteid_datasetid.csv")
  datawith <- read.csv(pathwith, header = TRUE, sep = ",", quote = '"', check.names = FALSE)
  
  # Expected result
  pathexpected <- test_path("testdata", "presenceData.csv")
  dataexpected <- read.csv(pathexpected, header = TRUE, sep = ",", quote = '"', check.names = FALSE)
  
  # TEST
  # Simplified dataset produces correct output
  expect_equal(as.data.frame(summarizeData(datawithout)), dataexpected)
  
  # Produces error when extra columns in
  expect_error(summarizeData(datawith))
})



test_that("transformData() correctly transforms data", {
  
  # LOAD COMPARISON DATA
  # With site/dataset ID 
  pathwith <- test_path("testdata", "inputData_siteid_datasetid.csv")
  datawith <- read.csv(pathwith, header = TRUE, sep = ",", quote = '"', check.names = FALSE)
  
  # Without site/dataset ID
  pathwithout <- test_path("testdata", "inputData_nositeid_nodatasetid.csv")
  datawithout <- read.csv(pathwithout, header = TRUE, sep = ",", quote = '"', check.names = FALSE)
  
  # Expected result
  pathexpected <- test_path("testdata", "transformedData.csv")
  dataexpected <- read.csv(pathexpected, header = TRUE, sep = ",", quote = '"', check.names = FALSE)
  
  # Dataset with unnecessary column
  dataextra <- datawith
  dataextra["unnecessary_column"] <- 1:36
  
  # Dataset missing necessary column
  datamissing <- subset(datawith, select = -sitename)
    
  
  
  # TEST
  # Test with site/dataset ID is correct
  expect_equal(as.data.frame(transformData(datawith)), as.data.frame(dataexpected))
  
  # Test without site/dataset ID is correct
  expect_equal(as.data.frame(transformData(datawithout)), dataexpected)
  
  # Test with and without producing same result
  expect_equal(transformData(datawith), transformData(datawithout))
  
  # Expect error if unnecessary column included
  expect_error(transformData(dataextra))
  
  # Expect error if necessary column omitted
  expect_error(transformData(datamissing))
})


# Produces error when extra columns in
expect_error(summarizeData(datawith))

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



test_that("find_bbox() finds correct bounding box coordinates", {
  
  # CREATE COMPARISON DATA
  # Import map data
  pathmap <- test_path("testdata", "mapData.csv")
  datamap <- read.csv(pathmap)
  
  # Expected Result
  expected_result <- sf::st_bbox(c(xmin = -168.49, ymin = 64.47, xmax = -143.10, ymax = 69.21), 
                             crs = 4326)
   
  
  # TEST
  expect_equal(find_bbox(datamap), expected_result, tolerance = 0.01)
})
