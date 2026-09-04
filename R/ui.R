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
                shiny::tags$h1("Usage Policy"),
                shiny::tags$p(shiny::tags$h3("Refuginator: An Interactive Tool for Identifying Refugia"), "Copyright (C) 2026  Nathaniel E.D. Morley"),
                shiny::tags$p("This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful,
                              but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.  If not, see", shiny::tags$a("http://www.gnu.org/licenses/.", href = "http://www.gnu.org/licenses/")),
                shiny::tags$p("Source code is available", shiny::tags$a("here.", href = "https://github.com/nathanielmorley2000/refuginator")),
                shiny::tags$h3("Attribution"),
                shiny::tags$p("An attribution consists of a citation to the theoretical background presented by Morley et al. (2026) for identifying refugia.  Please use the following citation as a guideline when formatting the bibliographic entry for this application:"),
                shiny::tags$p("Morley NED, Schneider CL, Cahill JF, Sullivan C, Leighton LR. 2026. Geohistorical data reveal an ice age refugium with implications for modern conservation. Commun Earth Environ. 7:704.", shiny::tags$a("https://doi.org/10.1038/s43247-026-03563-3.", href = "https://doi.org/10.1038/s43247-026-03563-3")),
                shiny::tags$h3("Neotoma Pollen Database"),
                shiny::tags$p("The Neotoma Paleoecology Database (Williams, Grimm et al., 2018) is licensed under a", shiny::tags$a("CC BY 4.0 license", href = "https://creativecommons.org/licenses/by/4.0/deed.en"),
                              "Users are free to use data from the Neotoma database, including from the", shiny::tags$b("Neotoma Pollen Database"), "functionality of this dashboard, provided they abide by Neotoma's",
                              shiny::tags$a("data use and embargo policy.", href = "https://www.neotomadb.org/data/data-use-and-embargo-policy"), " The creators of the Refuginator dashboard will not be held responsible for any violations or abuses of this policy."),
                shiny::tags$p("Williams JW, Grimm EC, et al. 2018. The Neotoma Paleoecology Database, a multiproxy, international, community-curated data resource. Quat Res. 89(1):156-177.", shiny::tags$a("https://doi.org/10.1017/qua.2017.105", href = "https://doi.org/10.1017/qua.2017.105")),
                shiny::tags$br(),
                shiny::tags$p(shiny::tags$b("By clicking the box below, users agree to abide by the Refuginator's Usage Policy.")),
                
                
                 #shiny::tags$iframe(src = "UsagePolicy.html", width = "100%", height = "600px"),
                  #shiny::includeHTML(system.file("html/UsagePolicy.html", package = "refuginator")),

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
