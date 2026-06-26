
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
    dplyr::select(sitename, lat, long, siteid, datasetid, value, age)

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


