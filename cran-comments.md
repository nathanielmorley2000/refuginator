## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* checking dependencies in R code ... NOTE
  Namespaces in Imports field not imported from:
    'dplyr' 'gganimate' 'ggplot2' 'gifski' 'leaflet' 'neotoma2' 'plotly'
    'rnaturalearth' 'rnaturalearthdata' 'sf' 'shinycssloaders' 'shinyjs'
    'tidyr' 'viridis'
    All declared Imports should be used.
    
    This package contains one exported function in the "R/" subdirectory, which 
    launches a shiny application. Most of these imports are used in the program files
    for the actual shiny application, located in the "inst/ShinyApp/" subdirectory, 
    and are missed by the R CMD check.



## revdepcheck results

This is a new release, so there are no packages with reverse dependencies.
