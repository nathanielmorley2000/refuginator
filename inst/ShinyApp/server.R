# define server logic for Refuginator 3,000
server <- function(input, output, session) {

  # define pipe operator for code
  "%>%" <- dplyr::"%>%"

  #################### Neotoma Database ####################
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
      bbox_coords = base::matrix(c(xmin, ymin,  # lower-left
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
          output$sitePreview <- renderLeaflet({
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
        output$neotomaTable <- renderTable({
          neotomaData[1:10, 1:12]
        })

        # allows user to download data to their file directory
        output$downloadNeotoma <- downloadHandler(
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
  ##########################################################


  #################### upload file ####################
  # Reactive value to track if data is loaded
  data_loaded <- reactiveVal(FALSE)

  # reactive expression to read the uploaded file
  rawData <- reactive({
    req(input$file1)
    inFile <- input$file1
    rawdata <- read.csv(inFile$datapath, header = TRUE, sep = ",", quote = '"', check.names = FALSE)

    return(rawdata)
  })


  # Reactive expression to transform the data
  transformedData <- reactive({
    # define user inputs
    data = rawData()

    # remove siteid and datasetid columns, if present
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

      # add JavaScript to refresh the page after closing the error message
      shinyjs::runjs("$('#shiny-modal').on('hidden.bs.modal', function() { location.reload(); });")

    } else {

      # set control to "TRUE"
      control = TRUE

      # custom function from functions.R file that summarizes number of localities with data and those with pollen for each time bin
        data = summarizeData(data, control)

        if (control == TRUE) {
        # pivot to a long table that can be used for graphing
        data = data %>%
          tidyr::pivot_longer(cols = c(localities_with_data, localities_with_pollen),
                       names_to = "metric", values_to = "value")
        return(data)
        } else {
          data = NULL
        }
    }
  })

  # code for displaying regional analysis tab when "Analyze" button clicked
  tab_inserted <- reactiveVal(FALSE)

  # dynamically show the "Analyze" button once a file is uploaded
  output$analyze_btn_ui <- renderUI({
    if (!is.null(input$file1)) {
      actionButton("analyze_btn", "Analyze")
    }
  })

  # Observe the Analyze button click
  observeEvent(input$analyze_btn, {
    if (!tab_inserted()) {
      # Insert the new "Analysis" tab dynamically
      insertTab(inputId = "main_tabs",
                #################### regional analysis ####################
                tabPanel("Regional Analysis",
                         sidebarLayout(
                           # Sidebar panel for Monte Carlo inputs ----
                           sidebarPanel(
                             h2("Monte Carlo Analysis"),
                             tags$div(style = "height: 10px;"),
                             numericInput("entry", "Entry Incidence", value = 9),
                             numericInput("decline", "Decline Incidence", value = 5),
                             numericInput("duration", "Duration of Decline (Number of Time Bins)", value = 1),
                             numericInput("nit", "Number of Iterations", value = 10000),
                             actionButton("calcButton", "Calculate"),
                             tags$hr(),
                             h4("Realized p-value:"),
                             verbatimTextOutput("calcResult")
                           ),
                           # Main panel for displaying outputs ----
                           mainPanel(
                              shinycssloaders::withSpinner(plotly::plotlyOutput("dataPlot")),
                              tags$hr(),
                              downloadButton("downloadAnimation", "Download Animation"),
                              #uiOutput("dataAnimation"),
                              uiOutput("plot_ui"),
                              actionButton("play", "Play Animation"),
                              actionButton("pause", "Pause Animation"),
                              uiOutput("time_slider_ui")

                           ))),
                ###########################################################
        target = "Upload Data",
        position = "after"
    )
    tab_inserted(TRUE)
  }
  # Automatically switch to the newly inserted "Analysis" tab
  updateTabsetPanel(session, "main_tabs", selected = "Regional Analysis")
})

  #####################################################


  #################### regional analysis ####################
  # indicator for whether a file has been uploaded
  output$fileUploaded <- reactive({
    return(!is.null(rawData()))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)


  ##### regional incidence plot #####
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
  ###################################


  ##### Monte Carlo analysis #####
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
  ################################


  ##### site-specific animations #####
  # Load country boundaries
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

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

  # Render the time slider UI dynamically based on the data
  output$time_slider_ui <- renderUI({
    df <- mapData()
    req(df)

    # Extract unique time points
    time_points <- levels(df$time)

    # Slider for selecting time
    sliderInput(
      inputId = "time_slider",
      label = "Select Time:",
      min = 1,
      max = length(time_points),
      value = 1,
      step = 1,
      ticks = FALSE,
      animate = FALSE,
      width = "100%",
      sep = ""
    )
  })

  # Reactive value to track current time bin index and observer
  rv <- reactiveValues(
    current_time = 1,
    is_playing = FALSE,
    timer = NULL,      # reactiveTimer
    timer_obs = NULL   # Observer
  )

  # Render plot UI with conditional spinner
  output$plot_ui <- renderUI({
    if (!data_loaded()) {
      # Data is loading; show spinner
      shinycssloaders::withSpinner(plotOutput("animated_plot", height = "700px"))
    } else {
      plotOutput("animated_plot", height = "700px")
    }
  })

  # Observe Play button
  observeEvent(input$play, {
    if (!rv$is_playing) {
      rv$is_playing <- TRUE
      rv$timer <- reactiveTimer(2000, session)  # 2-second interval

      # Create and assign the observer to rv$timer_obs
      rv$timer_obs <- observe({
        rv$timer()  # Trigger every 1.5 seconds
        isolate({
          if (!rv$is_playing) return()  # Double-check state

          df <- mapData()
          req(df)
          time_levels <- levels(df$time)
          current_index <- rv$current_time

          # Increment the time index
          new_index <- (current_index %% length(time_levels)) + 1
          rv$current_time <- new_index
          updateSliderInput(session, "time_slider", value = new_index)
        })
      })
    }
  })

  # Observe Pause button
  observeEvent(input$pause, {
    if (rv$is_playing) {
      rv$is_playing <- FALSE
      # Destroy the observer to stop the timer
      if (!is.null(rv$timer_obs)) {
        rv$timer_obs$destroy()
        rv$timer_obs <- NULL
      }
      rv$timer <- NULL  # Clear the timer
    }
  })

  # Update current_time based on slider
  observeEvent(input$time_slider, {
    rv$current_time <- input$time_slider
  })

  # automatically draw bounding box with 10% margin around the datapoints
  expandedBBox <- reactive({
    # define the data
    map_data <- mapData()

    # custom function in functions.R that automatically finds bounding box from coordinates on datasheet
    expanded_bbox <- find_bbox(map_data)
    return(expanded_bbox)
  })

  # further modify the bounding box so it can be placed on map
  expandedBBoxsfc <- reactive({
    expanded_bbox_sfc <- sf::st_as_sfc(expandedBBox())
    return(expanded_bbox_sfc)
  })

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
    base_map <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = countries, fill = "lightgrey", color = "black") +  # Background countries
      ggplot2::geom_sf(data = expanded_bbox_sfc, fill = NA, color = "black", lwd = 2) +  # Bounding box
      ggplot2::coord_sf(xlim = c(expanded_bbox$xmin, expanded_bbox$xmax),
               ylim = c(expanded_bbox$ymin, expanded_bbox$ymax),
               expand = FALSE) +  # Zoom into bounding box
      ggplot2::xlab("Longitude") +
      ggplot2::ylab("Latitude") +
      ggplot2::theme_minimal(base_size = 20)

    # Check if there are any non-NA values for this time slice
    if (nrow(df_time) > 0) {

      # Plot points onto base map
      map_with_data <- base_map +
        ggplot2::geom_point(data = df_time, ggplot2::aes(x = long, y = lat, color = value, group = time), size = 10) +
        ggplot2::scale_size_continuous(guide = 'none') +

        # Set up a dual color scale: 0 values as white, others with viridis gradient
        ggplot2::scale_color_gradientn(
          colors = c("white", viridis::viridis(256)),  # White for 0, viridis for others
          values = scales::rescale(c(0, 1)),  # Ensure 0 is mapped to white
          limits = c(0, max(df$value, na.rm = TRUE)),  # Set limits starting from 0
          na.value = NA  # Ensure NA values are not plotted
        ) +

        ggplot2::labs(title = paste("Geospatial Heat Map - Time:", current_time, "ya"),
             color = "Abundance")

    } else {
      # If there is no data (empty slice), just plot the base map and color legend
      map_with_data <- base_map +
        ggplot2::scale_color_gradientn(
          colors = viridis::viridis(256),
          limits = c(0, 10),  # Arbitrary limits to ensure the color scale appears
          na.value = "white"
        ) +
        ggplot2::labs(title = paste("Geospatial Heat Map - Time:", current_time, "ya"),
             color = "Abundance")
    }
    return(map_with_data)
  })


  # allows user to download animation to their file directory
  output$downloadAnimation <- downloadHandler(
    filename = function() {
      paste("heatmap_animation.gif")
    },
    content = function(file) {

      # define necessary inputs
      req(expandedBBoxsfc())
      map_data <- mapData()
      expanded_bbox <- expandedBBox()
      expanded_bbox_sfc <- expandedBBoxsfc()

      map_data <- map_data %>%
        dplyr::mutate(time = as.numeric(as.character(time)))

      timebins <- sort(unique(map_data$time))

      # create base map
      base_map <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = countries, fill = "lightgrey", color = "black") +  # Background countries
        ggplot2::geom_sf(data = expanded_bbox_sfc, fill = NA, color = "black", lwd = 2) +  # Bounding box
        ggplot2::coord_sf(xlim = c(expanded_bbox$xmin, expanded_bbox$xmax),
                 ylim = c(expanded_bbox$ymin, expanded_bbox$ymax),
                 expand = FALSE) +  # Zoom into bounding box
        ggplot2::xlab("Longitude") +
        ggplot2::ylab("Latitude")+
        ggplot2::theme_minimal(base_size = 20)

      # plot data onto base map with black points being n=0 and coloured points reflecting number of grains >1
      map_with_data <- base_map +
        ggplot2::geom_point(data = map_data, ggplot2::aes(x = long, y = lat, color = value, group = time), size = 10) +
        ggplot2::scale_size_continuous(guide = 'none') +

        # Set up a dual color scale: 0 values as white, others with viridis gradient
        ggplot2::scale_color_gradientn(
          colors = c("white", viridis::viridis(256)),  # White for 0, viridis for others
          values = scales::rescale(c(0, 1)),  # Ensure 0 is mapped to white
          limits = c(0, max(map_data$value, na.rm = TRUE)),  # Set limits starting from 0
          na.value = NA  # Ensure NA values are not plotted
        ) +

        ggplot2::labs(color = "Abundance")

      # animate the map through time
      map_with_animation <- map_with_data +
        gganimate::transition_time(-time) +
        ggplot2::ggtitle('Year: {frame_time}',
                subtitle = 'Frame {frame} of {nframes}')
      num_years <- length(timebins)

      # save map to www/ folder
      gganimate::anim_save(file, animation = gganimate::animate(map_with_animation,
                                          nframes = num_years,
                                          fps = 1.5,
                                          width = 1600,
                                          height = 1200,
                                          res = 150))

    }
  )
  ####################################
}
