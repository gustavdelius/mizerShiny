#' Launch mizerShiny
#'
#' @param params A MizerParams object. Defaults to NS_params.
#' @param guildparams An optional data frame defining guilds for the model.
#'  See the 'Guilds' section of the package vignette for details.
#' @param nutrition An optional data frame defining nutritional values
#'  for the model. See the 'Nutrition' section of the package vignette for details.
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
             guildparams = default_guildparams, nutrition = default_nutrition,
             fishery_strategy_tabs = c("Biomass", "Biomass % Change",
                                       "Yield", "Yield % Change",
                                       "Nutrition", "Length",
                                       "Guild"),
             species_role_tabs = c("Biomass", "Size", "Guilds", "Diet"), ...) {

  ## make the arguments available to the Shiny session
  shiny::shinyOptions("params" = params)
  shiny::shinyOptions("guildparams" = guildparams)
  shiny::shinyOptions("nutrition" = nutrition)
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

#' Launch mizerShiny with all tabs enabled
#'
#' A convenience wrapper around `mizerShiny()` that launches the app with all
#' available tabs enabled in the Fishery Strategy module.
#' @inheritParams mizerShiny
#' @param ... further arguments passed on to `shiny::runApp()`
#'           (e.g. `host`, `port`, `launch.browser`).
#' @export
mizerShinyAllTabs <- function(params = default_params,
                              guildparams = NULL, nutrition = NULL, ...) {
    mizerShiny(params,
               guildparams = guildparams, nutrition = nutrition,
               fishery_strategy_tabs = c("Biomass", "Biomass % Change",
                                       "Yield", "Yield % Change",
                                       "Nutrition change", "Length",
                                       "Guild", "Yield Composition", "Size",
                                       "Spectra", "Diet"),
               ...)
}
