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
    dplyr::summarise(
      x = sum(value.x, na.rm = TRUE),
      y1 = sum(value.y, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(Percentage_Change = 2 * (y1 - x) / (x + y1) * 100)

  df3 <- dplyr::left_join(sf2, sf3, by = c("w", "Legend")) |>
    dplyr::group_by(w) |>
    dplyr::summarise(
      x = sum(value.x, na.rm = TRUE),
      y3 = sum(value.y, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(Percentage_Change = 2 * (y3 - x) / (x + y3) * 100)

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = df1,
                       aes(x = w, y = Percentage_Change, color = "Sim 1"),
                       linetype = "solid") +
    ggplot2::geom_line(data = df3,
                       aes(x = w, y = Percentage_Change, color = "Sim 2"),
                       linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = "dark grey", linewidth = 0.75) +
    ggplot2::labs(x = "Size (g)", y = "Biomass % Change", color = "Comparison") +
    ggplot2::scale_color_manual(values = c("Sim 1" = "#2FA4E7", "Sim 2" = "#E76F51")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14, hjust = 1, vjust = 0.5),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 16),
      axis.title.y = ggplot2::element_text(size = 16)
    ) +
    ggplot2::xlim(NA, 10000) +
    ggplot2::scale_x_log10()


  return(p)
}
