#' Launch the Shiny app bundled with assign4nspack
#'
#' Opens the Shiny app located in `inst/app/` inside the installed package.
#'
#' @return Invisibly returns the result of [shiny::runApp()].
#' @examples
#' \dontrun{
#'   run_app()
#' }
#' @export
run_app <- function(){
  app_dir <- system.file("app", package = "assign4nspack")
  if (app_dir == "") stop("App directory not found. Reinstall assign4nspack.", call. = FALSE)
  shiny::runApp(app_dir, display.mode = "normal")
}
