# Data Organization ------------------------------------------------------------

# Custom function that summarizes number of localities with data and those with pollen for each time bin
summarizeData <- function(data, control) {
  
  # Transform data
  data = data %>%
    dplyr::select(!c("lat", "long")) %>%
    tidyr::pivot_longer(cols = -sitename, names_to = "time", values_to = "abundance")  # Replace COLUMN_NAME with actual column name
  
  # Capture warnings during the mutate() step
  tryCatch({
    withCallingHandlers({
      
      # Handle rows where COLUMN_NAME could not be converted to numeric
      data = data %>%
        dplyr::group_by(time) %>%
        dplyr::summarize(
          localities_with_data = sum(!is.na(abundance)),
          localities_with_pollen = sum(abundance > 0, na.rm = TRUE)
        ) %>%
        dplyr::mutate(
          time = as.numeric(time),
          localities_with_data = as.numeric(localities_with_data),
          localities_with_pollen = as.numeric(localities_with_pollen)
        ) %>%
        dplyr::arrange(time)
      return(data)
    
      # Throw error message if extra columns are messing up calculations
      }, warning = function(w) {
        shiny::showModal(modalDialog(
          title = "Error: Extra Columns Present",
                                     "It seems that extra columns are present in your uploaded data. Please ensure only the required columns are included.",
                                     easyClose = TRUE,
                                     footer = NULL))
      
      # Add JavaScript to refresh the page after closing the error message
      shinyjs::runjs("$('#shiny-modal').on('hidden.bs.modal', function() { location.reload(); });")
      
      # Suppress the warning and stop further execution in this context
      invokeRestart("muffleWarning")
      control = FALSE})
    })
  }


# Transform input data so regional presence plot can be drawn
transformData <- function(data) {
  
  # Remove siteid and datasetid columns, if present
  unwanted_columns = c("siteid", "datasetid")
  existing_columns = colnames(data)
  columns_to_remove = dplyr::intersect(existing_columns, unwanted_columns)
  data = data %>%
    dplyr::select(-all_of(columns_to_remove))
  
  # Check if the first three columns are correct
  expected_initial_columns <- c("sitename", "lat", "long")
  uploaded_columns <- colnames(data)
  if (!all(expected_initial_columns == uploaded_columns[1:3])) {
    shiny::showModal(modalDialog(
      title = "Error: Invalid Columns",
      "If using custom data, the first three columns must be 'sitename', 'lat', and 'long'.",
      easyClose = TRUE,
      footer = NULL
    ))
    
    # Add JavaScript to refresh the page after closing the error message
    shinyjs::runjs("$('#shiny-modal').on('hidden.bs.modal', function() { location.reload(); });")
    
  } else {
    
    # Set control to "TRUE"
    control = TRUE
    
    # Custom function from DataOrganization.R file that summarizes number of localities with data and those with pollen for each time bin
    data = summarizeData(data, control)
    
    if (control == TRUE) {
      
      # Pivot to a long table that can be used for graphing
      data = data %>%
        tidyr::pivot_longer(cols = c(localities_with_data, localities_with_pollen),
                            names_to = "metric", values_to = "value")
      return(data)
    
      # Fail condition if unsuccessful
      } else {
        data = NULL
      }
    }
  }



# Geospatial Data Visualisation  -----------------------------------------------

# Load country borders
countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


# Custom function to transform data into a format that can be used for animations
findMapData <- function(mapdata) {
  
  # Remove siteid and datasetid columns, if present
  unwanted_columns = c("siteid", "datasetid")
  existing_columns = colnames(mapdata)
  columns_to_remove = dplyr::intersect(existing_columns, unwanted_columns)
  mapdata <- mapdata %>%
    dplyr::select(-all_of(columns_to_remove))
  
  # Validate required columns
  required_cols <- c("sitename", "lat", "long")
  
  # Identify time columns (all except the required ones)
  time_cols <- setdiff(names(mapdata), required_cols)
  if(length(time_cols) == 0) {
    showNotification("No numerical columns found for time categories. Please include at least one time-based numerical column.", type = "error")
    return(NULL)
  }
  
  # Transform data into a format that can be used for the heatmap
  mapdata <- mapdata %>%
    tidyr::pivot_longer(cols = all_of(time_cols),
                        names_to = "time", values_to =
                          "value") %>%
    tidyr::drop_na(value) %>%
    dplyr::mutate(time = as.numeric(as.character(time))) %>%
    dplyr::mutate(time = factor(time, levels = sort(unique(time)))) %>%
    dplyr::mutate(value = as.numeric(value))
  
  
  # Set data_loaded to TRUE after successful processing
  data_loaded(TRUE)
  return(mapdata)
  }


# Find dimensions of bounding box around study area
find_bbox <- function(map_data) {
  
  # Draw original bounding box based on extremal sites
  coordinates = sf::st_as_sf(map_data, coords = c("long", "lat"), crs = 4326)
  bbox = sf::st_bbox(coordinates)
  
  # Set margin to 0.1 and find original height and width
  margin = 0.1
  width = bbox$xmax - bbox$xmin
  height = bbox$ymax - bbox$ymin
  
  # Adjust coordinates to accommodate margin
  xmin = bbox$xmin - width * margin
  xmax = bbox$xmax + width * margin
  ymin = bbox$ymin - height * margin
  ymax = bbox$ymax + height * margin
  
  # Create new bounding box and convert so it can be recognized by ggplot2
  bbox_coords = matrix(c(xmin, ymin,  # lower-left
                         xmax, ymin,  # lower-right
                         xmax, ymax,  # upper-right
                         xmin, ymax,  # upper-left
                         xmin, ymin), # lower-left, close the polygon
                       ncol = 2, byrow = TRUE)
  bbox_polygon = sf::st_polygon(list(bbox_coords))
  bbox_sf = sf::st_sfc(bbox_polygon, crs = 4326)
  expanded_bbox <- sf::st_bbox(bbox_sf)
  
  # Finish function and return adjusted bounding box values
  return(expanded_bbox)
}
