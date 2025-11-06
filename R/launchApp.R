#' Launch mizerShiny
#'
#' @param params A MizerParams object. Defaults to NS_params.
#' @param fishery_strategy_tabs A character vector specifying which tabs to include
#'   in the Fishery Strategy module. Tab names that don't correspond to existing
#'   tabs will be ignored.
#' @param species_role_tabs A character vector specifying which tabs to include
#'   in the Species Role module. Tab names that don't correspond to existing
#'   tabs will be ignored.
#' @param ... further arguments passed on to `shiny::runApp()`
#'            (e.g. `host`, `port`, `launch.browser`).
#'
#' @examples
#' \dontrun{
#' # Run with default model
#' mizerShiny()
#'
#' # Run with custom model
#' mizerShiny(NS_params)
#'
#' # Run with only specific tabs
#' mizerShiny(fishery_strategy_tabs = c("Biomass", "Yield"),
#'            species_role_tabs = c("Biomass", "Diet"))
#'}
#' @export
mizerShiny <-
    function(params = default_params,
             fishery_strategy_tabs = c("Biomass", "Biomass change",
                                       "Yield", "Yield change",
                                       "Nutrition change", "Length",
                                       "Guild"),
             species_role_tabs = c("Biomass", "Size", "Guilds", "Diet"), ...) {

  ## make the params object available to the Shiny session
  shiny::shinyOptions("default_params" = params)
  shiny::shinyOptions("fishery_strategy_tabs" = fishery_strategy_tabs)
  shiny::shinyOptions("species_role_tabs" = species_role_tabs)

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

mizerShinyAllTabs <- function(params = default_params, ...) {
    mizerShiny(params,
               fishery_strategy_tabs = c("Biomass", "Biomass change",
                                       "Yield", "Yield change",
                                       "Nutrition change", "Length",
                                       "Guild", "Yield Composition", "Size",
                                       "Spectra", "Diet"),
               ...)
}
