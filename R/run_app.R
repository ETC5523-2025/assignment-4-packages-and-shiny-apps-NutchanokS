#' Launch the Shiny app
#'
#' This function launches the interactive explorer bundled with the package.
#' It uses the packaged datasets (no reading from disk).
#'
#' @export
#' @examples
#' \dontrun{
#'   run_app()
#' }
run_app <- function() {
  # Check required packages for the app
  required <- c("shiny","dplyr","ggplot2","plotly","tidyr","bslib")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Please install missing packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  # Locate the app directory (should be inst/app/)
  appDir <- system.file("app", package = "assign4nspack")
  if (appDir == "" || !file.exists(file.path(appDir, "app.R"))) {
    stop("App directory not found. Expecting inst/app/app.R inside the package.", call. = FALSE)
  }

  # Launch the app
  shiny::runApp(appDir, display.mode = "normal")
}
