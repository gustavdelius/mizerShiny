#' Percentage difference between two values by species
#'
#' Joins two data frames by `Species` and returns percentage change of
#' values in first data frame relative to the second, excluding the `Resource` row.
#'
#' @param df Data frame with columns `Species` and `value`
#' @param df_0 Data frame with columns `Species` and `value` for baseline
#' @return A data frame with columns `Species` and `percentage_diff`
#' @examples
#' percentdiff(
#'   df = data.frame(Species = c("A","B"), value = c(120, 80)),
#'   df_0 = data.frame(Species = c("A","B"), value = c(100, 100))
#' )
percentdiff <- function(df, df_0) {
  df |>
    dplyr::left_join(df_0, by = "Species") |>
    dplyr::mutate(percentage_diff = ((value.x - value.y) / value.y) * 100) |>
    dplyr::select(Species, percentage_diff) |>
    dplyr::filter(!Species %in% ("Resource"))
}


