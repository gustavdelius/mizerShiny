#' Launch mizerShiny
#'
#' @param params A MizerParams object. Defaults to NS_params.
#' @param ... further arguments passed on to `shiny::runApp()`
#'            (e.g. `host`, `port`, `launch.browser`).
#' @import shiny
#' @import mizer
#' @import ggplot2
#' @import bslib
#' @import plotly
#' @import gridlayout
#' @import thematic
#' @import dplyr
#' @import forcats
#' @import shinyBS
#' @import rintrojs
#' @import patchwork
#' @import here
#' @import sortable
#' @import shinyjs
#' @import shinyWidgets
#'
#' @examples
#' \dontrun{
#' # Run with default model
#' mizerShiny()
#'
#' # Run with custom model
#' mizerShiny(NS_params)
#'}
#' @export
mizerShiny <- function(params = default_params, ...) {

  ## make the params object available to the Shiny session
  shiny::shinyOptions("default_params" = default_params)

  ## Locate and run the Shiny app
  dev_path <- file.path("inst", "app", "app.R")
  if (file.exists(dev_path)) {
    return(shiny::runApp(dev_path, ...))
  }

  pkg_app <- system.file("app", "app.R", package = "mizerShiny")
  if (pkg_app == "") {
    stop("Could not find Shiny app; reinstall or rebuild `mizerShiny`.", call. = FALSE)
  }

  shiny::runApp(pkg_app, ...)
}
