percentdiff <- function(harvested, unharvested) {
  harvested |>
    dplyr::left_join(unharvested, by = "Species") |>
    dplyr::mutate(percentage_diff = ((value.x - value.y) / value.y) * 100) |>
    dplyr::select(Species, percentage_diff) |>
    dplyr::filter(!Species %in% ("Resource"))
}


