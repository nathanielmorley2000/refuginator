#' Launch the Refuginator Application in your Browser
#'
#' @param defaultbrowser Choose whether to launch the application in your default browser.
#'
#' @return Launches Refuginator application in your browser. All features are contained within the application. Read the "refuginatorIntro" vignette for a guide to the features.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples 
#' if (interactive()) {
#'   launchRefuginator()
#' }
#' 


launchRefuginator <- function(defaultbrowser = TRUE) {
  
  app <- shiny::shinyApp(
    ui = app_ui(),
    server = app_server
  )
  

  shiny::runApp(app, display.mode = "normal", launch.browser = defaultbrowser)
}
