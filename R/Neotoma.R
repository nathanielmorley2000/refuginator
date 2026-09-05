findNeotoma <- function(al_pollen, taxon, taxonReplace, timeBin, yearMin, yearMax, samplingProtocol) {
  
  # Download dataset -- may take some time
  al_dl = al_pollen %>% neotoma2::get_downloads(all_data = TRUE)
  allSamp = neotoma2::samples(al_dl)

  # Harmonize taxa based on user input
  allSamp = allSamp %>%
    dplyr::filter(.data$ecologicalgroup %in% c("TRSH")) %>%
    dplyr::mutate(variablename = replace(.data$variablename,
                                  stringr::str_detect(.data$variablename, taxonReplace),
                                  taxon))

  # Create a function to check and add the specific taxon if not present
  ensure_taxon_present <- function(df, taxon) {
    if (!(taxon %in% df$variablename)) {
      df <- dplyr::bind_rows(
        df,
        data.frame(
          variablename = taxon,
          value = 0
        )
      )
    }
    
    df
  }
  
  # Apply the function to each group
  allSamp0 <- allSamp %>%
    dplyr::group_by(
      .data$sitename,
      .data$lat,
      .data$long,
      .data$siteid,
      .data$datasetid,
      .data$age,
      .data$variablename
    ) %>%
    dplyr::summarize(
      value = sum(.data$value),
      .groups = "keep"
    ) %>%
    dplyr::group_by(
      .data$sitename,
      .data$lat,
      .data$long,
      .data$siteid,
      .data$datasetid,
      .data$age
    ) %>%
    dplyr::group_modify(
      ~ ensure_taxon_present(.x, taxon)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$variablename == taxon) %>%
    dplyr::select(
      .data$sitename,
      .data$lat,
      .data$long,
      .data$siteid,
      .data$datasetid,
      .data$value,
      .data$age
    )
  
  # Create time bins as a separate column
  timeCorrected = allSamp0 %>%
    dplyr::filter(.data$age >= 0) %>%
    dplyr::mutate(Year_Bin = floor(.data$age / timeBin) * timeBin)

  # Selects sample with smallest value (for specified taxon) in time bin
  if (samplingProtocol == "Minimum") {
    data_filtered = timeCorrected %>%
      dplyr::group_by(.data$sitename, .data$Year_Bin) %>%
      dplyr::slice_min(order_by = .data$value, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$Year_Bin >= yearMin) %>%
      dplyr::filter(.data$Year_Bin <= yearMax)

  # Selects sample with largest value (for specified taxon) in time bin
  } else if (samplingProtocol == "Maximum") {
    data_filtered = timeCorrected %>%
      dplyr::group_by(.data$sitename, .data$Year_Bin) %>%
      dplyr::slice_max(order_by = .data$value, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$Year_Bin >= yearMin) %>%
      dplyr::filter(.data$Year_Bin <= yearMax)
  }

  # Creates pivot table with correctly ordered time bins
  ordered_years = sort(unique(data_filtered$Year_Bin))
  pivot_table = data_filtered %>%
    dplyr::select(.data$sitename, .data$siteid, .data$datasetid, .data$lat, .data$long, .data$Year_Bin, .data$value) %>%
    tidyr::pivot_wider(names_from = .data$Year_Bin, values_from = .data$value, values_fill = list(Taxon_Abundance = NA)) %>%
    dplyr::select(.data$sitename, .data$siteid, .data$datasetid, .data$lat, .data$long, tidyr::all_of(as.character(ordered_years)))

  return(pivot_table)
}
