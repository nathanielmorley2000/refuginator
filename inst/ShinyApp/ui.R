# load custom functions from functions.R file
source("R/functions.R")

# limit scipen to 9999
safely_patch_options()

# define user interface for Refuginator
ui <- fluidPage(

  # initialize js integration
  shinyjs::useShinyjs(),

  # app title for head ---
  tags$head(HTML("<title>Identify Geohistorical Refugia with Refuginator</title>")),

  # app title
  titlePanel(h1("Refuginator",
  style={'background-color: #000000;
  margin-top: -20px;
  margin-left: -15px;
  margin-right: -15px;
  padding-left: 20px;
	color: #ffffff;'})),

  # custom CSS using inline style to increase margins
  tags$style(HTML("
    #upload-data-tab {
      margin-left: 20px;
      margin-right: 20px;
      margin-bottom: 20px;
    }
  ")),


  # create multiple tabs with different inputs and outputs
  tabsetPanel(
    id = "main_tabs",

    #################### upload file ####################
    tabPanel("Upload Data",

             # assign ID for styling
             div(id = "upload-data-tab",

                 # show usage policy
                 includeHTML("html/UsagePolicy.html"),

                 # checkbox for agreeing to terms
                 checkboxInput("agree", "I agree to the Usage Policy", value = FALSE),
                 tags$hr(),

                 # if checkbox clicked, show file upload
                 conditionalPanel(
                   condition = "input.agree == true",
                   fileInput("file1", "Choose CSV File",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   uiOutput("analyze_btn_ui")
                  )
             )
    ),
    #####################################################

    # "Regional Analysis" tab will be inserted here

    #################### Neotoma Database ####################
    tabPanel("Neotoma Pollen Database",
             sidebarLayout(

               # sidebar panel for search terms
               sidebarPanel(
                 h2("Search Neotoma"),
                 tags$div(style = "height: 10px;"),
                 h4("Coordinates:"),
                 numericInput("xmin", "Western Longitude", value = -168.92),
                 numericInput("xmax", "Eastern Longitude", value = -144.71),
                 numericInput("ymin", "Southern Latitude", value = 64.69),
                 numericInput("ymax", "Northern Latitude", value = 68.87),
                 tags$hr(),
                 h4("Taxon of Interest:"),
                 textInput("taxon", label = "Scientific Name (e.g., Picea)", value = "Picea"),
                 tags$hr(),
                 h4("Time Parameters:"),
                 numericInput("yearMax", "Beginning of Interval (ya)", value = 20000),
                 numericInput("yearMin", "End of Interval (ya)", value = 0),
                 numericInput("timeBin", "Time Bin", value = 500),
                 selectInput("samplingProtocol",
                             "Sampling Protocol:",
                             choices = c("Minimum", "Maximum")),
                 actionButton("neotomaSearch", "Search")
               ),

               # main panel for displaying outputs
               mainPanel(
                 # sites preview
                 conditionalPanel(
                   condition = "input.neotomaSearch == false",
                   h2("Input Search Parameters")
                 ),
                 conditionalPanel(
                   condition = "input.neotomaSearch == true",
                   h2("Sites Preview:"),
                   shinycssloaders::withSpinner(leaflet::leafletOutput("sitePreview"), type = 6),
                   actionButton("proceed", "Proceed with Selection")
                 ),
                 tags$hr(),
                 # transformed dataset preview
                 conditionalPanel(
                   condition = "input.proceed == true",
                   h2("Data Preview:"),
                   shinycssloaders::withSpinner(tableOutput("neotomaTable"), type = 6),
                   downloadButton("downloadNeotoma", "Download Data")
                 )
               )
             ))
    ##########################################################
  )
)
