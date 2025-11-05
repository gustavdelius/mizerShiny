#' Plot size spectra for two harvested simulations
#'
#' Produces log-log spectra lines comparing two harvested projections over a
#' time range, distinguishing simulations by linetype.
#'
#' @param harvestedprojection First harvested mizer projection
#' @param harvestedprojection2 Second harvested mizer projection
#' @param time1,end1 Integer start and end time for both simulations
#' @param time2,end2 Unused; kept for compatibility
#' @return A ggplot object
#' @keywords internal
plotSpectra2 <- function(harvestedprojection, harvestedprojection2, time1, end1, time2, end2) {
  data1 <- plotSpectra(harvestedprojection, time_range = time1:end1, return_data = TRUE) |>
    dplyr::mutate(sim = "Sim 1")
  data2 <- plotSpectra(harvestedprojection2, time_range = time1:end1, return_data = TRUE) |>
    dplyr::mutate(sim = "Sim 2")
  combined_data <- dplyr::bind_rows(data1, data2)
  ggplot2::ggplot(combined_data, ggplot2::aes(x = w, y = value, color = Species, linetype = sim,
                            group = interaction(Species, sim))) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", linewidth = 0.75) +
    ggplot2::labs(x = "Size (g)", y = "Biomass Density") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14, hjust = 1, vjust = 0.5),
          axis.text.y = ggplot2::element_text(size = 14),
          axis.title.x = ggplot2::element_text(size = 16),
          axis.title.y = ggplot2::element_text(size = 16),
          legend.position = "bottom") +
    ggplot2::xlim(NA, 10000) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::scale_linetype_manual(values = c("Sim 1" = "solid", "Sim 2" = "dashed")) +
    ggplot2::guides(linetype = ggplot2::guide_legend(title = "Simulation"),
           color = ggplot2::guide_legend(title = "Species"))
}


