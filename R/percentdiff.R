#' Percentage difference between harvested and unharvested values by species
#'
#' Joins two data frames by `Species` and returns percentage change of
#' harvested relative to unharvested, excluding the `Resource` row.
#'
#' @param harvested Data frame with columns `Species` and `value` for harvested
#' @param unharvested Data frame with columns `Species` and `value` for baseline
#' @return A data frame with columns `Species` and `percentage_diff`
#' @examples
#' percentdiff(
#'   harvested = data.frame(Species = c("A","B"), value = c(120, 80)),
#'   unharvested = data.frame(Species = c("A","B"), value = c(100, 100))
#' )
percentdiff <- function(harvested, unharvested) {
  harvested |>
    dplyr::left_join(unharvested, by = "Species") |>
    dplyr::mutate(percentage_diff = ((value.x - value.y) / value.y) * 100) |>
    dplyr::select(Species, percentage_diff) |>
    dplyr::filter(!Species %in% ("Resource"))
}


