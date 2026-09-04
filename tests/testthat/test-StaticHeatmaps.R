test_that("staticHeatmaps() produces correct animation classes", {
  
  # CREATE COMPARISON DATA
  # Import map data
  pathmap <- test_path("testdata", "mapData.csv")
  datamap <- read.csv(pathmap)
  datamap$time <- as.factor(datamap$time) # convert to factor to match the data the function should produce
  
  # bbox data
  databbox <- sf::st_bbox(c(xmin = -168.49, ymin = 64.47, xmax = -143.10, ymax = 69.21), 
                             crs = 4326)
  
  # bbox data for sfc
  databbox_sfc <- sf::st_as_sfc(databbox)
  
  # Time bins
  timebins <- sort(unique(datamap$time))
  
  # Create animation
  anim <- staticHeatmap(datamap, databbox, databbox_sfc, countries, timebins)
  
  
  
  # TEST
  # Assert it inherits the correct classes without actually rendering it
  expect_s3_class(anim, "gg")
  expect_s3_class(anim, "ggplot")
  expect_s3_class(anim, "gganim")
})
