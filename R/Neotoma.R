findNeotoma <- function(al_pollen, taxon, taxonReplace, timeBin, yearMin, yearMax, samplingProtocol) {
  
  # Download dataset -- may take some time
  al_dl = al_pollen %>% neotoma2::get_downloads(all_data = TRUE)
  allSamp = neotoma2::samples(al_dl)

  # Harmonize taxa based on user input
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

  # Create time bins as a separate column
  timeCorrected = allSamp0 %>%
    dplyr::filter(age >= 0) %>%
    dplyr::mutate(Year_Bin = floor(age / timeBin) * timeBin)

  # Selects sample with smallest value (for specified taxon) in time bin
  if (samplingProtocol == "Minimum") {
    data_filtered = timeCorrected %>%
      dplyr::group_by(sitename, Year_Bin) %>%
      dplyr::slice_min(order_by = value, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Year_Bin >= yearMin) %>%
      dplyr::filter(Year_Bin <= yearMax)

  # Selects sample with largest value (for specified taxon) in time bin
  } else if (samplingProtocol == "Maximum") {
    data_filtered = timeCorrected %>%
      dplyr::group_by(sitename, Year_Bin) %>%
      dplyr::slice_max(order_by = value, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Year_Bin >= yearMin) %>%
      dplyr::filter(Year_Bin <= yearMax)
  }

  # Creates pivot table with correctly ordered time bins
  ordered_years = sort(unique(data_filtered$Year_Bin))
  pivot_table = data_filtered %>%
    dplyr::select(sitename, siteid, datasetid, lat, long, Year_Bin, value) %>%
    tidyr::pivot_wider(names_from = Year_Bin, values_from = value, values_fill = list(Taxon_Abundance = NA)) %>%
    dplyr::select(sitename, siteid, datasetid, lat, long, all_of(as.character(ordered_years)))

  return(pivot_table)
}
