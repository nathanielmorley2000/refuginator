# define pipe operator for code
"%>%" <- dplyr::"%>%"

findNeotoma <- function(al_pollen, taxon, taxonReplace, timeBin, yearMin, yearMax, samplingProtocol) {
  # download dataset -- may take some time
  al_dl = al_pollen %>% neotoma2::get_downloads(all_data = TRUE)
  allSamp = neotoma2::samples(al_dl)

  # harmonize taxa based on user input
  allSamp = allSamp %>%
    dplyr::filter(ecologicalgroup %in% c("TRSH")) %>%
    dplyr::mutate(variablename = replace(variablename,
                                  stringr::str_detect(variablename, taxonReplace),
                                  taxon))

  # Create a function to check and add the specific taxon if not present
  ensure_taxon_present <- function(df, taxon) {
    if (!(taxon %in% df$variablename)) {
      df <- dplyr::bind_rows(df, data.frame(sitename = df$sitename[1], lat = df$lat[1], long = df$long[1], siteid = df$siteid[1], datasetid = df$datasetid[1], age = df$age[1], variablename = taxon, value = 0))
    }
    return(df)
  }

  # Apply the function to each group
  allSamp0 = allSamp %>%
    dplyr::group_by(sitename, lat, long, siteid, datasetid, age, variablename) %>%
    dplyr::summarize(value = sum(value), .groups = "keep") %>%
    dplyr::group_by(sitename, lat, long, siteid, datasetid, age) %>%
    dplyr::do(ensure_taxon_present(., taxon)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(variablename == taxon) %>%
    dplyr::elect(sitename, lat, long, siteid, datasetid, value, age)

  # create 500-yr time bins as a separate column
  timeCorrected = allSamp0 %>%
    dplyr::filter(age >= 0) %>%
    dplyr::mutate(Year_Bin = floor(age / timeBin) * timeBin)

  # selects sample with smallest value (for specified taxon) in 500-year time bin
  if (samplingProtocol == "Minimum") {
    data_filtered = timeCorrected %>%
      dplyr::group_by(sitename, Year_Bin) %>%
      dplyr::slice_min(order_by = value, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Year_Bin >= yearMin) %>%
      dplyr::filter(Year_Bin <= yearMax)

  # selects sample with largest value (for specified taxon) in 500-year time bin
  } else if (samplingProtocol == "Maximum") {
    data_filtered = timeCorrected %>%
      dplyr::group_by(sitename, Year_Bin) %>%
      dplyr::slice_max(order_by = value, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Year_Bin >= yearMin) %>%
      dplyr::filter(Year_Bin <= yearMax)
  }

  # creates pivot table with correctly ordered time bins
  ordered_years = sort(unique(data_filtered$Year_Bin))
  pivot_table = data_filtered %>%
    dplyr::select(sitename, siteid, datasetid, lat, long, Year_Bin, value) %>%
    tidyr::pivot_wider(names_from = Year_Bin, values_from = value, values_fill = list(Taxon_Abundance = NA)) %>%
    dplyr::select(sitename, siteid, datasetid, lat, long, all_of(as.character(ordered_years)))

  return(pivot_table)
}

summarizeData <- function(data, control) {
  # Perform your data transformation here using dplyr
  data = data %>%
    dplyr::select(!c("lat", "long")) %>%
    tidyr::pivot_longer(cols = -sitename, names_to = "time", values_to = "abundance")  # Replace COLUMN_NAME with actual column name

  tryCatch({
    # Capture warnings during the mutate() step
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
    }, warning = function(w) {

      # throw error message if extra columns are messing up calculations
      shiny::showModal(modalDialog(
          title = "Error: Extra Columns Present",
          "It seems that extra columns are present in your uploaded data. Please ensure only the required columns are included.",
          easyClose = TRUE,
          footer = NULL
        ))

      # add JavaScript to refresh the page after closing the error message
      shinyjs::runjs("$('#shiny-modal').on('hidden.bs.modal', function() { location.reload(); });")

      # Suppress the warning and stop further execution in this context
      invokeRestart("muffleWarning")

      control = FALSE
    })
  })
}

# significance of going from >= 11 down to <= 5 for 7 time bins, then back up to >= 11
monteCarlo <- function(entry, decline, duration, nit, summary) {
  it = 1
  score = 0
  count = 0
  for(it in 1:nit){
    vec = floor(runif(nrow(summary), min = 0, max = summary$localities_with_data + 1)) # randomly generate integer for each time bin between 0 and the number of available localities
    indices_greater_than_entry = which(vec >= entry)
    if(length(indices_greater_than_entry) > 1){
      for(i in 1:length(indices_greater_than_entry)){
        count = 0
        if(i + 1 <= length(indices_greater_than_entry)){
          index_lower = indices_greater_than_entry[i]
          index_higher = indices_greater_than_entry[i + 1]
          for(j in index_lower:index_higher){
            if(vec[j] <= decline){
              count <- count + 1
            }
          }
          if(count >= duration) {
            score = score + 1
            break
          }
        }
      }
    }
  }
  return(score)
}

find_bbox <- function(map_data) {
  # find original bounding box
  coordinates = sf::st_as_sf(map_data, coords = c("long", "lat"), crs = 4326)
  bbox = sf::st_bbox(coordinates)

  # set margin to 0.1 and find original height and width
  margin = 0.1
  width = bbox$xmax - bbox$xmin
  height = bbox$ymax - bbox$ymin

  # adjust coordinates to accommodate margin
  xmin = bbox$xmin - width * margin
  xmax = bbox$xmax + width * margin
  ymin = bbox$ymin - height * margin
  ymax = bbox$ymax + height * margin

  # create new bounding box and convert so it can be recognized by ggplot2
  bbox_coords = matrix(c(xmin, ymin,  # lower-left
                         xmax, ymin,  # lower-right
                         xmax, ymax,  # upper-right
                         xmin, ymax,  # upper-left
                         xmin, ymin), # close the polygon
                       ncol = 2, byrow = TRUE)
  bbox_polygon = sf::st_polygon(list(bbox_coords))
  bbox_sf = sf::st_sfc(bbox_polygon, crs = 4326)
  expanded_bbox <- sf::st_bbox(bbox_sf)

  # finish function and return adjusted bounding box values
  return(expanded_bbox)}
