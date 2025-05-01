safely_patch_options <- function() {
  original_options <- base::options

  unlockBinding("options", baseenv())
  assign("options", function(..., .env = parent.frame()) {
   args <- list(...)
    if ("scipen" %in% names(args) && args$scipen > 9999) {
      args$scipen <- 9999
    }
    do.call(original_options, args, envir = .env)
  }, envir = baseenv())
  lockBinding("options", baseenv())
}



# Apply the patch BEFORE loading packages
safely_patch_options()

# load libraries
#library("dplyr")
#library("tidyr")
#library("ggplot2")
#library("gganimate")
#library("gifski")
#library("sf")
#library("rnaturalearth")
#library("rnaturalearthdata")
#library("shiny")
#library("shinyjs")
#library("shinycssloaders")
#library("plotly")
#library("leaflet")
#library("neotoma2")
#library("viridis")

# run application
#runApp("~/GitHub/refuginator/inst/ShinyApp/")
