# DEFINE USER INTERFACE --------------------------------------------------------
ui <- shiny::fluidPage(

  # Initialize js integration
  shinyjs::useShinyjs(),

  
  # App title for webpage head
  shiny::tags$head(shiny::HTML("<title>Identify Geohistorical Refugia with Refuginator</title>")),

  
  # App title bar
  shiny::titlePanel(shiny::h1("Refuginator",
  style={'background-color: #000000;
  margin-top: -20px;
  margin-left: -15px;
  margin-right: -15px;
  padding-left: 20px;
	color: #ffffff;'})),

  
  # Custom CSS using inline style to increase margins
  shiny::tags$style(shiny::HTML("
    #upload-data-tab {
      margin-left: 20px;
      margin-right: 20px;
      margin-bottom: 20px;
    }
  ")),

  
  # Create multiple tabs with different inputs and outputs
  shiny::tabsetPanel(
    id = "main_tabs",

    ## Upload File page --------------------------------------------------------
    shiny::tabPanel("Upload Data",

             # Assign ID for styling
             shiny::div(id = "upload-data-tab",

                 # Show usage policy
                # shiny::includeHTML("html/UsagePolicy.html"),

                 # Checkbox for agreeing to terms
                 shiny::checkboxInput("agree", "I agree to the Usage Policy", value = FALSE),
                 shiny::tags$hr(),

                 # If checkbox clicked, show file upload
                 shiny::conditionalPanel(
                   condition = "input.agree == true",
                   shiny::fileInput("file1", "Choose CSV File",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   shiny::uiOutput("analyze_btn_ui")
                   )             
                 )    
             ),

    
    # "Regional Analysis" tab will be inserted here

    
    ## Neotoma Database page ---------------------------------------------------
    shiny::tabPanel("Neotoma Pollen Database",
                    shiny::sidebarLayout(

                      # Sidebar panel for search terms
                      shiny::sidebarPanel(
                        shiny::h2("Search Neotoma"),
                        shiny::tags$div(style = "height: 10px;"),
                        shiny::h4("Coordinates:"),
                        shiny::numericInput("xmin", "Western Longitude", value = -168.92),
                        shiny::numericInput("xmax", "Eastern Longitude", value = -144.71),
                        shiny::numericInput("ymin", "Southern Latitude", value = 64.69),
                        shiny::numericInput("ymax", "Northern Latitude", value = 68.87),
                        shiny::tags$hr(),
                        shiny::h4("Taxon of Interest:"),
                        shiny::textInput("taxon", label = "Scientific Name (e.g., Picea)", value = "Picea"),
                        shiny::tags$hr(),
                        shiny::h4("Time Parameters:"),
                        shiny::numericInput("yearMax", "Beginning of Interval (ya)", value = 20000),
                        shiny::numericInput("yearMin", "End of Interval (ya)", value = 0),
                        shiny::numericInput("timeBin", "Time Bin", value = 500),
                        shiny::selectInput("samplingProtocol",
                                    "Sampling Protocol:",
                                    choices = c("Minimum", "Maximum")),
                        shiny::actionButton("neotomaSearch", "Search")
               ),

               # Main panel for displaying outputs
               shiny::mainPanel(
                 
                 # Sites preview
                 shiny::conditionalPanel(
                   condition = "input.neotomaSearch == false",
                   shiny::h2("Input Search Parameters")
                 ),
                 shiny::conditionalPanel(
                   condition = "input.neotomaSearch == true",
                   shiny::h2("Sites Preview:"),
                   shinycssloaders::withSpinner(leaflet::leafletOutput("sitePreview"), type = 6),
                   shiny::actionButton("proceed", "Proceed with Selection")
                 ),
                 shiny::tags$hr(),
                 
                 
                 # Transformed dataset preview
                 shiny::conditionalPanel(
                   condition = "input.proceed == true",
                   shiny::h2("Data Preview:"),
                   shinycssloaders::withSpinner(shiny::tableOutput("neotomaTable"), type = 6),
                   shiny::downloadButton("downloadNeotoma", "Download Data")
                   )
                 )
               )
             )
    )
  )
