# define server logic for Refuginator 3,000
server <- function(input, output, session) {

  # define pipe operator for code
  "%>%" <- dplyr::"%>%"

# NEOTOMA DATABASE -------------------------------------------------------------
  
  sites <- observeEvent(input$neotomaSearch, {
    # check to make sure all fields are filled out
    if (is.na(input$xmin) ||
        is.na(input$xmax) ||
        is.na(input$ymin) ||
        is.na(input$ymax) ||
        input$taxon == "" ||
        is.na(input$timeBin) ||
        is.na(input$yearMax) ||
        is.na(input$yearMin)) {

      # show a modal dialog if any input is missing
      shiny::showModal(modalDialog(
        title = "Input Error",
        "Please fill out all fields before proceeding.",
        easyClose = TRUE,
        footer = NULL
      ))

      # add JavaScript to refresh the page after closing the error message
      shinyjs::runjs("$('#shiny-modal').on('hidden.bs.modal', function() { location.reload(); });")

    } else {
      # define user inputs
      xmin = input$xmin
      xmax = input$xmax
      ymin = input$ymin
      ymax = input$ymax

      # create bounding box from user inputs to make Neotoma API call
      bbox_coords = matrix(c(xmin, ymin,  # lower-left
                             xmax, ymin,  # lower-right
                             xmax, ymax,  # upper-right
                             xmin, ymax,  # upper-left
                             xmin, ymin), # close the polygon
                           ncol = 2, byrow = TRUE)
      bbox_polygon = sf::st_polygon(list(bbox_coords))


      # allows error messages to be displayed if API call is unsuccessful
      tryCatch({

        # make Neotoma API call to retrieve site metadata
        al_sites = neotoma2::get_sites(loc = bbox_polygon, all_data = TRUE)
        sites_summary = neotoma2::summary(al_sites)


        # get datasets and filter to only include pollen data
        al_datasets = neotoma2::get_datasets(al_sites, all_data = TRUE)
        al_pollen = al_datasets %>%
          neotoma2::filter(datasettype == "pollen" & !is.na(age_range_young))

        if (is.null(al_pollen) || length(al_pollen) == 0) {
          # If no sites are returned, show a modal with a specific message
          shiny::showModal(modalDialog(
            title = "No Sites Found",
            "No sites were found for the given coordinates. Try different coordinates.",
            easyClose = TRUE,
            footer = NULL
          ))

          # add JavaScript to refresh the page after closing the error message
          shinyjs::runjs("$('#shiny-modal').on('hidden.bs.modal', function() { location.reload(); });")

        } else {
          # preview selected sites on the dashboard and give the option of changing before downloading data
          output$sitePreview <- leaflet::renderLeaflet({
            neotoma2::plotLeaflet(al_pollen) %>%
              leaflet::addPolygons(map = .,
                                   data = bbox_polygon,
                                   color = "green") })
        }
      }, error = function(e) {
        # If there is an error (e.g., connection fails), show a modal with the error message
        shiny::showModal(modalDialog(
          title = "API Connection Error",
          paste("Failed to connect to the Neotoma API. Check your internet connection or try again later."),
          easyClose = TRUE,
          footer = NULL
        ))

        # add JavaScript to refresh the page after closing the error message
        shinyjs::runjs("$('#shiny-modal').on('hidden.bs.modal', function() { location.reload(); });")
      })

      neotomaData <- observeEvent(input$proceed, {
        # define user input
        taxon = input$taxon
        taxonReplace = taxonReplace()
        timeBin = input$timeBin
        yearMin = input$yearMin
        yearMax = input$yearMax
        samplingProtocol = input$samplingProtocol

        # custom function from functions.R file that downloads data and arranges it according to minimum sample approach
        neotomaData <- findNeotoma(al_pollen, taxon, taxonReplace, timeBin, yearMin, yearMax, samplingProtocol)

        # displays output on dashboard
        output$neotomaTable <- shiny::renderTable({
          neotomaData[1:10, 1:12]
        })

        # allows user to download data to their file directory
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

  # needed for data harmonization in custom findNeotoma() function
  taxonReplace <- reactive({
    taxon = input$taxon
    modified_taxon_name = paste0(taxon, ".*")
    return(modified_taxon_name)
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
  
  
  # Validate required columns
  required_cols <- c("sitename", "lat", "long")
  
  
  # reactive expression to transform the data
  mapData <- reactive({
    # define user input
    mapdata <- rawData()
    req(mapdata)
    
    # remove siteid and datasetid columns, if present
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
    
    # transform data into a method that can
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
  })
  

# REGIONAL ANALYSIS ------------------------------------------------------------
  
## Regional Presence Plot ------------------------------------------------------
  
  output$dataPlot <- plotly::renderPlotly({
    # define necessary data
    req(transformedData())
    summary_long <- transformedData()

    # create plot
    p <- ggplot2:: ggplot(summary_long, ggplot2::aes(x = value, y = time, color = metric)) +
      ggplot2::geom_path(linewidth = 1) +
      ggplot2::labs(title = "Localities Data Over Time",
           x = "Number of localities",
           y = "Time",
           color = "Metric") +
      ggplot2::scale_y_reverse() +
      ggplot2::theme_classic()

    # convert using plotly to make interactive
    plotly::ggplotly(p) %>%
      plotly::layout(hovermode = "x")
  })


## Spatial Monte Carlo ---------------------------------------------------------
  observeEvent(input$calcButton, {
    # check to make sure all fields are filled out
    if (is.na(input$entry) ||
        is.na(input$decline) ||
        is.na(input$duration) ||
        is.na(input$nit)) {

      # show a modal dialog if any input is missing
      shiny::showModal(modalDialog(
        title = "Input Error",
        "Please fill out all fields before proceeding.",
        easyClose = TRUE,
        footer = NULL
      ))

    } else {
      # define user inputs
      entry = input$entry
      decline = input$decline
      duration = input$duration
      nit = input$nit
      rawData = rawData()

      # remove siteid and datasetid columns, if present
      unwanted_columns = c("siteid", "datasetid")
      existing_columns = colnames(rawData)
      columns_to_remove = dplyr::intersect(existing_columns, unwanted_columns)
      rawData <- rawData %>%
        dplyr::select(-all_of(columns_to_remove))

      # custom function from functions.R file that summarizes number of localities with data and those with pollen for each time bin
      summary = summarizeData(rawData)

      # custom function from functions.R file that runs Monte Carlo simulation to resample curve and keeps track of "successful" iterations
      score <- monteCarlo(entry, decline, duration, nit, summary)

      # calculate realized p-value and display on ui
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
    if (!data_loaded()) {
      # Data is loading; show spinner
      withSpinner(plotOutput("animated_plot", height = "700px"))
    } else {
      plotOutput("animated_plot", height = "700px")
    }
  }) 
  

  
  
### Plot Map -------------------------------------------------------------------
  
  # Render the animation and provide a download option
  output$animated_plot <- renderPlot({
    df <- mapData()
    expanded_bbox <- expandedBBox()
    expanded_bbox_sfc <- expandedBBoxsfc()
    req(df)
    
    # Extract unique time points
    time_points <- rev(levels(df$time))
    current_time_index <- rv$current_time
    current_time <- time_points[current_time_index]
    # Filter data for the current time, but don't drop 0s or NAs here
    df_time <- df %>%
      dplyr::filter(time == current_time)
    
    # Set up the base map
    base_map <- ggplot() +
      geom_sf(data = countries, fill = "lightgrey", color = "black") +  # Background countries
      geom_sf(data = expanded_bbox_sfc, fill = NA, color = "black", lwd = 2) +  # Bounding box
      coord_sf(xlim = c(expanded_bbox$xmin, expanded_bbox$xmax),
               ylim = c(expanded_bbox$ymin, expanded_bbox$ymax),
               expand = FALSE) +  # Zoom into bounding box
      xlab("Longitude") +
      ylab("Latitude") +
      theme_minimal(base_size = 20)
    
    # Check if there are any non-NA values for this time slice
    if (nrow(df_time) > 0) {
      
      # Plot points onto base map
      map_with_data <- base_map +
        geom_point(data = df_time, aes(x = long, y = lat, color = value, group = time), size = 10) +
        scale_size_continuous(guide = 'none') +
        
        # Set up a dual color scale: 0 values as white, others with viridis gradient
        scale_color_gradientn(
          colors = c("white", viridis(256)),  # White for 0, viridis for others
          values = scales::rescale(c(0, 1)),  # Ensure 0 is mapped to white
          limits = c(0, max(df$value, na.rm = TRUE)),  # Set limits starting from 0
          na.value = NA  # Ensure NA values are not plotted
        ) +
        
        labs(title = paste("Geospatial Heat Map - Time:", current_time, "ya"),
             color = "Abundance")
      
    } else {
      # If there is no data (empty slice), just plot the base map and color legend
      map_with_data <- base_map +
        scale_color_gradientn(
          colors = viridis(256), 
          limits = c(0, 10),  # Arbitrary limits to ensure the color scale appears
          na.value = "white"
        ) +
        labs(title = paste("Geospatial Heat Map - Time:", current_time, "ya"),
             color = "Abundance")
    }
    return(map_with_data)
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