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
  # check required packages for the app
  required <- c("shiny","dplyr","ggplot2","plotly","tidyr")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Please install missing packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  appDir <- system.file("shiny", package = "assign4nspack")
  shiny::runApp(appDir, display.mode = "normal")
}

