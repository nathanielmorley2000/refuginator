test_that("Data can be retrieved from Neotoma API", {
  
  # This test uses an API call - gracefully fail test if internet is unavailable
  skip_if_offline()
  
  # CREATE COMPARISON DATA
  columns <- c("sitename", "siteid", "datasetid", "lat", "long", 1500, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500, 7000, 7500, 8000, 
    8500, 9000, 9500, 11000, 12000, 13500, 14000, 14500, 15000)
  
  # bbox data
  bbox_coords = matrix(c(-157, 66.5,  # lower-left
                         -156, 66.5,  # lower-right
                         -156, 67,  # upper-right
                         -157, 67,  # upper-left
                         -157, 66.5), # lower-left to close the polygon
                       ncol = 2, byrow = TRUE)
  bbox_polygon = sf::st_polygon(list(bbox_coords))
  
  # Make Neotoma API call to retrieve site metadata
  al_sites = neotoma2::get_sites(loc = bbox_polygon, all_data = TRUE)
  al_datasets = neotoma2::get_datasets(al_sites, all_data = TRUE)
  al_pollen = al_datasets %>%
    neotoma2::filter(datasettype == "pollen" & !is.na(age_range_young))
  
  # Retrieve site data
  APIResult <- findNeotoma(al_pollen, "Picea", "Picea.*", 500, 0, 20000, "Minimum") 
  colnames(APIResult) <- columns
  
  # Retrieve expected data
  pathexpected <- test_path("testdata", "NeotomaResults.csv")
  dataexpected <- read.csv(pathexpected) 
  colnames(dataexpected) <- columns
  
  
  # TEST
  # Check actual result matches expected result
  expect_equal(as.data.frame(APIResult), dataexpected)
})
