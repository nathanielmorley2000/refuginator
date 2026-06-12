#' Launch the Refuginator Application in your Browser
#'
#' @param defaultbrowser Choose whether to launch the application in your default browser.
#'
#' @return Launches Refuginator application in your browser. All features are contained within the application.
#' @export
#'
#' @examples \dontrun{launchRefuginator()}
#' 


launchRefuginator <- function(defaultbrowser = TRUE) {
  appDir <- system.file("shinyApp", package = "refuginator")
  if (appDir == "") {
    stop("Could not find shinyApp. Try re-installing `refuginator`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = defaultbrowser)
}
