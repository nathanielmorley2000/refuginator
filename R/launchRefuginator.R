launchRefuginator <- function(inbrowser = TRUE) {
  appDir <- system.file("shinyApp", package = "refuginator")
  if (appDir == "") {
    stop("Could not find shinyApp. Try re-installing `refuginator`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = inbrowser)
}
