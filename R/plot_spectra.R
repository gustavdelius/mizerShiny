# Spectra-related plots used in Shiny app

#' Relative community size spectrum (single vs baseline)
#'
#' Computes percent change in the community spectrum of one simulation relative
#' to a baseline over a time range and draws a line plot.
#'
#' @param object1 Baseline mizer projection
#' @param object2 Comparison mizer projection
#' @param time1,time2 Integer start and end time indices
#' @return A ggplot object
#' @keywords internal
plotSpectraRelative <- function(object1, object2, time1, time2) {
  sf1 <- mizer::plotSpectra(object1, return_data = TRUE,
                            resource = FALSE, background = FALSE,
                            time_range = time1:time2)
  sf2 <- mizer::plotSpectra(object2, return_data = TRUE,
                            resource = FALSE, background = FALSE,
                            time_range = time1:time2)

  sf <- dplyr::left_join(sf2, sf1, by = c("w", "Legend")) |>
    dplyr::group_by(w) |>
    dplyr::summarise(x = sum(value.x, na.rm = TRUE),
                     y = sum(value.y, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Percentage_Change = (2 * (y - x) / (x + y)) * 100)

  ggplot2::ggplot() +
    ggplot2::geom_line(data = sf, ggplot2::aes(x = w, y = Percentage_Change), color = "#2FA4E7") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "dark grey", linewidth = 0.75) +
    ggplot2::labs(x = "Size (g)", y = "Biomass % Change") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14, hjust = 1, vjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 14),
                   legend.position = "none",
                   axis.title.x = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 16)) +
    ggplot2::xlim(NA, 10000) + ggplot2::scale_x_log10()
}

#' Relative spectra for two sims vs baseline
#'
#' Compares each of two simulations to a baseline and overlays two lines showing
#' percent change of community spectra over a time range.
#'
#' @param object1 Baseline mizer projection
#' @param object2 First comparison projection
#' @param object3 Second comparison projection
#' @inheritParams plotSpectraRelative
#' @return A ggplot object
#' @keywords internal
plotSpectraRelative2 <- function(object1, object2, object3, time1, time2) {
  sf1 <- mizer::plotSpectra(object1, return_data = TRUE,
                            resource = FALSE, background = FALSE,
                            time_range = time1:time2)
  sf2 <- mizer::plotSpectra(object2, return_data = TRUE,
                            resource = FALSE, background = FALSE,
                            time_range = time1:time2)
  sf3 <- mizer::plotSpectra(object3, return_data = TRUE,
                            resource = FALSE, background = FALSE,
                            time_range = time1:time2)

  df1 <- dplyr::left_join(sf2, sf1, by = c("w", "Legend")) |>
    dplyr::group_by(w) |>
    dplyr::summarise(x = sum(value.x, na.rm = TRUE), y1 = sum(value.y, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Percentage_Change = 2 * (y1 - x) / (x + y1) * 100)

  df3 <- dplyr::left_join(sf2, sf3, by = c("w", "Legend")) |>
    dplyr::group_by(w) |>
    dplyr::summarise(x = sum(value.x, na.rm = TRUE), y3 = sum(value.y, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Percentage_Change = 2 * (y3 - x) / (x + y3) * 100)

  ggplot2::ggplot() +
    ggplot2::geom_line(data = df1, ggplot2::aes(x = w, y = Percentage_Change, color = "Sim 1"), linetype = "solid") +
    ggplot2::geom_line(data = df3, ggplot2::aes(x = w, y = Percentage_Change, color = "Sim 2"), linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "dark grey", linewidth = 0.75) +
    ggplot2::labs(x = "Size (g)", y = "Biomass % Change", color = "Comparison") +
    ggplot2::scale_color_manual(values = c("Sim 1" = "#2FA4E7", "Sim 2" = "#E76F51")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14, hjust = 1, vjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 14),
                   axis.title.x = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 16)) +
    ggplot2::xlim(NA, 10000) + ggplot2::scale_x_log10()
}


