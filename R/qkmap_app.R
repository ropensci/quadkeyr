#' Launch the Shiny App
#'
#' @description This function launches the Shiny app.
#'
#' @return This function does not return any value. It launches the Shiny app.
#'
#' @export
#'
#' @examples
#' if(interactive()){
#' qkmap_app()
#' }
qkmap_app <- function() {
  # Get the path to the Shiny app directory
  shiny_app_path <- system.file("app",
    package = "quadkeyr"
  )

  # Launch the Shiny app
  shiny::runApp(shiny_app_path)
}
