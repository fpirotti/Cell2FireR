#' runWildfireApp
#' @description
#' Runs Wildfire-SIM  Shiny application
#' 
#' @returns NULL
#' @export
#' 
runWildfireApp <- function() {
  app_dir <- system.file( "app", package = "Cell2FireR")
  shiny::runApp(app_dir)
}
