insertRegionalAnalysis <- function() {
  
  # Dynamically insert new REGIONAL ANALYSIS tab
  insertTab(inputId = "main_tabs",
            tabPanel("Regional Analysis",
                     value = paste("Regional Analysis"),
                     sidebarLayout(
                       
                       # Sidebar panel for Monte Carlo inputs
                       sidebarPanel(
                         h2("Monte Carlo Analysis"),
                         tags$div(style = "height: 10px;"),
                         numericInput("entry", "Entry Presence", value = 9),
                         numericInput("decline", "Decline Presence", value = 5),
                         numericInput("duration", "Duration of Decline (Number of Time Bins)", value = 1),
                         numericInput("nit", "Number of Iterations", value = 10000),
                         actionButton("calcButton", "Calculate"),
                         tags$hr(),
                         h4("Realized p-value:"),
                         verbatimTextOutput("calcResult")
                       ),
                       
                       # Main panel for displaying outputs
                       mainPanel(
                         h2("Regional Presence Plot"),
                         shinycssloaders::withSpinner(plotly::plotlyOutput("dataPlot"), type = 6),
                         tags$hr(),
                         h2("Animated Heat Map"),
                         uiOutput("plotUI"),
                         #actionButton("play", "Play Animation"),
                         #actionButton("pause", "Pause Animation"),
                         #uiOutput("time_slider_ui"),
                         downloadButton("downloadAnimation", "Download Animation")
                       ))),
            
            # Place new tab after UPLOAD DATA
            target = "Upload Data",
            position = "after"
  )
}