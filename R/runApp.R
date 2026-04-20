#' runApp
#' @description
#' Runs Wildfire-SIM  Shiny application
#' 
#' @returns NULL
#' @export
#' 
runApp <- function() {
  app_dir <- system.file( "app", package = "Cell2FireR")
  shiny::runApp(app_dir)
}
