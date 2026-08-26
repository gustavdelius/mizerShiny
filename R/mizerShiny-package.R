#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import mizer
#' @import ggplot2
#' @import dplyr
#' @import shiny
#' @importFrom plotly ggplotly
#' @importFrom rlang .data
#' @importFrom reshape2 melt
#' @importFrom tibble tibble
## usethis namespace: end
NULL

# Global variables to suppress R CMD check notes
utils::globalVariables(c(
  ".data", "BarWidth", "Biomass", "Catch density", "Class", "Difference",
  "Freq", "Gear", "Guild", "Nutrient", "Percentage", "Percentage_Change",
  "Proportion", "Proportion.x", "Proportion.y", "Relative", "Sim", "Species",
  "SpeciesNum", "Strategy", "TimeClass", "TimeNum", "Value", "XPos", "Year",
  "Yield", "Ymax", "Ymin", "biomass", "default_guildparams", "default_nutrition",
  "default_params", "default_sim", "fill_group", "gear", "harvested_value",
  "normalized_value", "percentage_diff", "predator", "prey", "sim", "sp",
  "time", "tooltip_text", "total", "unharvested_value", "value", "value.x",
  "value.y", "w", "x", "y", "y1", "y3", "yield"
))
