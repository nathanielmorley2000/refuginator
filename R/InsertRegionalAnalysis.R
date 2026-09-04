insertRegionalAnalysis <- function() {
  
  # Dynamically insert new REGIONAL ANALYSIS tab
  shiny::insertTab(inputId = "main_tabs",
            shiny::tabPanel("Regional Analysis",
                     value = paste("Regional Analysis"),
                     shiny::sidebarLayout(
                       
                       # Sidebar panel for Monte Carlo inputs
                       shiny::sidebarPanel(
                         shiny::tags$h2("Monte Carlo Analysis"),
                         shiny::tags$div(style = "height: 10px;"),
                         shiny::numericInput("entry", "Entry Presence", value = 9),
                         shiny::numericInput("decline", "Decline Presence", value = 5),
                         shiny::numericInput("duration", "Duration of Decline (Number of Time Bins)", value = 1),
                         shiny::numericInput("nit", "Number of Iterations", value = 10000),
                         shiny::actionButton("calcButton", "Calculate"),
                         shiny::tags$hr(),
                         shiny::tags$h4("Realized p-value:"),
                         shiny::verbatimTextOutput("calcResult")
                       ),
                       
                       # Main panel for displaying outputs
                       shiny::mainPanel(
                         shiny::tags$h2("Regional Presence Plot"),
                         shinycssloaders::withSpinner(plotly::plotlyOutput("dataPlot"), type = 6),
                         shiny::tags$hr(),
                         shiny::tags$h2("Animated Heat Map"),
                         shiny::imageOutput("static_animation"),
                         shiny::tags$hr(),
                         shiny::downloadButton("downloadAnimation", "Download Animation")
                       ))),
            
            # Place new tab after UPLOAD DATA
            target = "Upload Data",
            position = "after"
  )
}