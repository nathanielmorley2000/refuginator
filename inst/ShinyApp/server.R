server <- function(input, output, session) {

# NEOTOMA DATABASE -------------------------------------------------------------
  
  # Facilitate data harmonization in custom findNeotoma() function
  taxonReplace <- reactive({
    taxon = input$taxon
    modified_taxon_name = paste0(taxon, ".*")
    return(modified_taxon_name)
  })
  
  
  # Find and display sites using Neotoma search feature
  sites <- observeEvent(input$neotomaSearch, {
    
    # Check to make sure all fields are filled out
    if (is.na(input$xmin) ||
        is.na(input$xmax) ||
        is.na(input$ymin) ||
        is.na(input$ymax) ||
        input$taxon == "" ||
        is.na(input$timeBin) ||
        is.na(input$yearMax) ||
        is.na(input$yearMin)) {

      # Show a modal dialog if any input is missing
      shiny::showModal(modalDialog(
        title = "Input Error",
        "Please fill out all fields before proceeding.",
        easyClose = TRUE,
        footer = NULL
      ))

      # Add JavaScript to refresh the page after closing the error message
      shinyjs::runjs("$('#shiny-modal').on('hidden.bs.modal', function() { location.reload(); });")

      # Otherwise perform Neotoma search
      } else {
      
        # Define user inputs
        xmin = input$xmin
        xmax = input$xmax
        ymin = input$ymin
        ymax = input$ymax
  
        # Create bounding box from user inputs to make Neotoma API call
        bbox_coords = matrix(c(xmin, ymin,  # lower-left
                               xmax, ymin,  # lower-right
                               xmax, ymax,  # upper-right
                               xmin, ymax,  # upper-left
                               xmin, ymin), # lower-left to close the polygon
                             ncol = 2, byrow = TRUE)
        bbox_polygon = sf::st_polygon(list(bbox_coords))
  
        # Show error message if API call is unsuccessful
        tryCatch({
  
          # Make Neotoma API call to retrieve site metadata
          al_sites = neotoma2::get_sites(loc = bbox_polygon, all_data = TRUE)
          sites_summary = neotoma2::summary(al_sites)
  
          # Get datasets and filter to only include pollen data
          al_datasets = neotoma2::get_datasets(al_sites, all_data = TRUE)
          al_pollen = al_datasets %>%
            neotoma2::filter(datasettype == "pollen" & !is.na(age_range_young))
  
          # If no sites are returned, show a modal with a specific message
          if (is.null(al_pollen) || length(al_pollen) == 0) {
            shiny::showModal(modalDialog(
              title = "No Sites Found",
              "No sites were found for the given coordinates. Try different coordinates.",
              easyClose = TRUE,
              footer = NULL))
  
            # Add JavaScript to refresh the page after closing the error message
            shinyjs::runjs("$('#shiny-modal').on('hidden.bs.modal', function() { location.reload(); });")
  
            # Preview selected sites on the dashboard and give the option of changing before downloading data
            } else {
              output$sitePreview <- leaflet::renderLeaflet({
                neotoma2::plotLeaflet(al_pollen) %>%
                  leaflet::addPolygons(map = .,
                                       data = bbox_polygon,
                                       color = "green") })
              }
          
          # If there is an error (e.g., connection fails), show a modal with the error message
          }, error = function(e) {
            shiny::showModal(modalDialog(
              title = "API Connection Error",
              paste("Failed to connect to the Neotoma API. Check your internet connection or try again later."),
              easyClose = TRUE,
              footer = NULL))
    
            # Add JavaScript to refresh the page after closing the error message
            shinyjs::runjs("$('#shiny-modal').on('hidden.bs.modal', function() { location.reload(); });")
        })
  
        # Download organized file
        neotomaData <- observeEvent(input$proceed, {
          
          # Define user input
          taxon = input$taxon
          taxonReplace = taxonReplace()
          timeBin = input$timeBin
          yearMin = input$yearMin
          yearMax = input$yearMax
          samplingProtocol = input$samplingProtocol
  
          # Custom function from Neotoma.R file that downloads data and arranges it according to minimum or maximum sample approach
          neotomaData <- findNeotoma(al_pollen, taxon, taxonReplace, timeBin, yearMin, yearMax, samplingProtocol)
  
          # Displays output on dashboard
          output$neotomaTable <- shiny::renderTable({
            neotomaData[1:10, 1:12]
          })
  
          # Allows user to download data to their file directory
          output$downloadNeotoma <- shiny::downloadHandler(
            filename = function() {
              paste("NeotomaData-", Sys.Date(), ".csv", sep = "")
            },
            content = function(file) {
              write.csv(neotomaData, file, row.names = FALSE)
            }
          )
        })
    }
  })



# UPLOAD DATA ------------------------------------------------------------------
  
## Mechanics -------------------------------------------------------------------  
  
  # Server-side indicator for whether a file has been uploaded
  output$fileUploaded <- reactive({
    return(!is.null(rawData()))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  
  # Reactive value to track if data is loaded
  data_loaded <- reactiveVal(FALSE)

  
  # Reactive expression to read the uploaded file
  rawData <- reactive({
    req(input$file1)
    inFile <- input$file1
    rawdata <- read.csv(inFile$datapath, header = TRUE, sep = ",", quote = '"', check.names = FALSE)
    return(rawdata)
  })

  
  # Dynamically show the "Analyze" button once a file is uploaded
  output$analyze_btn_ui <- renderUI({
    if (!is.null(input$file1)) {
      actionButton("analyze_btn", "Analyze")
    }
  })
  

  
## Insert REGIONAL ANALYSIS tab ------------------------------------------------
  
  # Default to being absent
  tab_inserted <- reactiveVal(FALSE)
  
  
  # Dynamically insert REGIONAL ANALYSIS tab when "Analyze" button pressed
  observeEvent(input$analyze_btn, {
    if (!tab_inserted()) {
      
      # Use UI template in InsertRegionalAnalysis.R
      insertRegionalAnalysis()
      tab_inserted(TRUE)
      }
    
    # Automatically switch to the newly inserted "Analysis" tab
    updateTabsetPanel(session, "main_tabs", selected = paste("Regional Analysis"))
    })



# DATA ORGANIZATION ------------------------------------------------------------
  
  # Reactive expression to transform the data
  transformedData <- reactive({
    data = rawData()
    
    # Custom function from DataOrganization.R that arranges data for plotting
    transformData(data)
  })
  
  
  # Reactive expression to transform the data
  mapData <- reactive({
    mapdata <- rawData()
    req(mapdata)
    
    # Custom function from DataOrganization.R to transform data into a format that can be used for animations
    findMapData(mapdata)
  })
  
  
  # Automatically draw bounding box with 10% margin around the datapoints
  expandedBBox <- reactive({
    map_data <- mapData()
    
    # Custom function in DataOrganization.R that automatically finds bounding box from coordinates on datasheet
    expanded_bbox <- find_bbox(map_data)
    return(expanded_bbox)
  })
  
  
  # Further modify the bounding box so it can be placed on map
  expandedBBoxsfc <- reactive({
    expanded_bbox_sfc <- sf::st_as_sfc(expandedBBox())
    return(expanded_bbox_sfc)
  })
  
  

# REGIONAL ANALYSIS ------------------------------------------------------------
  
## Regional Presence Plot ------------------------------------------------------
  
  # Render interactive pplot with plotly package
  output$dataPlot <- plotly::renderPlotly({
    req(transformedData())
    summary_long <- transformedData()

    # Create static plot with ggplot2
    p <- ggplot2:: ggplot(summary_long, ggplot2::aes(x = value, y = time, color = metric)) +
      ggplot2::geom_path(linewidth = 1) +
      ggplot2::labs(title = "Localities Data Over Time",
           x = "Number of localities",
           y = "Time",
           color = "Metric") +
      ggplot2::scale_y_reverse() +
      ggplot2::theme_classic()

    # Convert using plotly to make interactive
    plotly::ggplotly(p) %>%
      plotly::layout(hovermode = "x")
  })

  

## Temporal Monte Carlo --------------------------------------------------------
  
  # Calculate spatial Monte Carlo test upon pressing button
  observeEvent(input$calcButton, {
    
    # Check all fields are filled out
    if (is.na(input$entry) ||
        is.na(input$decline) ||
        is.na(input$duration) ||
        is.na(input$nit)) {

      # If not, show a modal dialog if any input is missing
      shiny::showModal(modalDialog(
        title = "Input Error",
        "Please fill out all fields before proceeding.",
        easyClose = TRUE,
        footer = NULL))

      # Perform Monte Carlo test if all elements present
      } else {
      
        # Define user inputs
        entry = input$entry
        decline = input$decline
        duration = input$duration
        nit = input$nit
        rawData = rawData()
  
        # Remove siteid and datasetid columns, if present
        unwanted_columns = c("siteid", "datasetid")
        existing_columns = colnames(rawData)
        columns_to_remove = dplyr::intersect(existing_columns, unwanted_columns)
        rawData <- rawData %>%
          dplyr::select(-all_of(columns_to_remove))
  
        # Custom function from DataOrganization.R file that summarizes number of localities with data and those with pollen for each time bin
        summary = summarizeData(rawData)
  
        # custom function from MonteCarlo.R file that runs Monte Carlo simulation to resample curve and keeps track of "successful" iterations
        score <- monteCarlo(entry, decline, duration, nit, summary)
  
        # Calculate realized p-value and display on ui
        result = score/nit
        output$calcResult <- renderText({
          paste("The result of the calculation is:", result)
        })
        } 
    })



## Display static animations ---------------------------------------------------

### UI Output ------------------------------------------------------------------
  
  # Render plot UI with conditional spinner
  output$plot_ui <- renderUI({
    
    # Data is loading; show spinner
    if (!data_loaded()) {
      withSpinner(plotOutput("animated_plot", height = "700px"))
    
      # Otherwise show plot
      } else {
      plotOutput("animated_plot", height = "700px")
    }
  }) 
  

  
### Plot Map -------------------------------------------------------------------
  
  # Render the animation and provide a download option
  output$animated_plot <- renderPlot({
    
    # Define necessary inputs
    req(expandedBBoxsfc())
    map_data <- mapData()
    expanded_bbox <- expandedBBox()
    expanded_bbox_sfc <- expandedBBoxsfc()
    
    # Sort time bins
    map_data <- map_data %>%
      dplyr::mutate(time = as.numeric(as.character(time)))
    timebins <- sort(unique(map_data$time))
    
    # Create static heatmap animation using StaticHeatmaps.R
    map_with_animation <- staticHeatmap(map_data, expanded_bbox, expanded_bbox_sfc, countries, timebins)
    return(map_with_animation)
  })



## Download Animation ----------------------------------------------------------
  
  # Begin downloading when button pressed
  output$downloadAnimation <- downloadHandler(
    filename = function() {
      
      # Default filename
      paste("heatmap_animation.gif")
    },
    content = function(file) {
      
      # Download notification
      download_notification <- showNotification("Your animation is downloading. This may take a minute.", 
                                                type = "message", duration = NULL,
                                                closeButton = FALSE)
      
      # Remove notification upon download
      on.exit({
        removeNotification(download_notification)
      })

      # Define necessary inputs
      req(expandedBBoxsfc())
      map_data <- mapData()
      expanded_bbox <- expandedBBox()
      expanded_bbox_sfc <- expandedBBoxsfc()
      
      # Sort time bins
      map_data <- map_data %>%
        dplyr::mutate(time = as.numeric(as.character(time)))
      timebins <- sort(unique(map_data$time))

      # Create static heatmap animation using StaticHeatmaps.R
      map_with_animation <- staticHeatmap(map_data, expanded_bbox, expanded_bbox_sfc, countries, timebins)
      
      # Save animation as GIF
      num_years <- length(timebins)
      gganimate::anim_save(file, animation = gganimate::animate(map_with_animation,
                                                                nframes = num_years,
                                                                fps = 1.5,
                                                                width = 1600,
                                                                height = 1200,
                                                                res = 150))
    })

  }