#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
                    ...) {
  ## Allow uploads up to 150 Mb (set at runtime: a top-level option in the package code
  ## is only evaluated at build time and is lost once the package is installed)
  options(shiny.maxRequestSize = 150 * 1024^2)
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server
    ),
    golem_opts = list(...)
  )
}
