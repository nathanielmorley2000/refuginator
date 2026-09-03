staticHeatmap <- function(map_data, expanded_bbox, expanded_bbox_sfc, countries, timebins) {
  
  # Organize map_data so lowest values plotted under highest values
  map_data <- map_data %>%
    dplyr::arrange(value)
  
  # Create base map
  base_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = countries, fill = "lightgrey", color = "black") +  # Background countries
    ggplot2::geom_sf(data = expanded_bbox_sfc, fill = NA, color = "black", lwd = 2) +  # Bounding box
    ggplot2::coord_sf(xlim = c(expanded_bbox$xmin, expanded_bbox$xmax),
                      ylim = c(expanded_bbox$ymin, expanded_bbox$ymax),
                      expand = FALSE) +  # Zoom into bounding box
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude")+
    ggplot2::theme_minimal(base_size = 20)
  
  # Plot data onto base map with black points being n=0 and coloured points reflecting number of grains >1
  map_with_data <- base_map +
    ggplot2::geom_point(data = map_data, ggplot2::aes(x = long, y = lat, fill = value, group = time), shape = 21, size = 10) +
    ggplot2::scale_size_continuous(guide = 'none') +
    
    # Set up a dual color scale: 0 values as white, others with viridis gradient
    ggplot2::scale_fill_gradientn(
      colors = c("white", viridis::viridis(256)),  # White for 0, viridis for others
      values = scales::rescale(c(0, 1)),  # Ensure 0 is mapped to white
      limits = c(0, max(map_data$value, na.rm = TRUE)),  # Set limits starting from 0
      na.value = NA  # Ensure NA values are not plotted
    ) +
    ggplot2::labs(color = "Abundance")
  
  # Animate the map through time
  map_with_animation <- map_with_data +
    gganimate::transition_time(-time) +
    ggplot2::ggtitle('Year: {frame_time}',
                     subtitle = 'Frame {frame} of {nframes}')

  return(map_with_animation)
}