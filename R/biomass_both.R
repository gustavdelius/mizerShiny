#' Plot biomass time series for one species across two simulations
#'
#' Produces a log-scale biomass time series for a selected `specie` from two
#' simulations, with linetype distinguishing simulations. If `specie` is NULL,
#' uses all species from the params object. Mostly used internally by the Shiny app.
#'
#' @param sim A mizer projection object for simulation 1
#' @param sim2 A mizer projection object for simulation 2
#' @param specie Optional character scalar of species to highlight; if NULL plots all
#' @param start_time,end_time Integer time indices for `sim`
#' @param start_time2,end_time2 Integer time indices for `sim2`
#' @param y_ticks Number of ticks on the y-axis
#' @param ylim Numeric vector of length 2 with y limits (log-scale)
#' @param highlight Optional species name to emphasize (unused currently)
#' @param ... Passed to ggplot2 theme functions
#' @return A ggplot object
#' @keywords internal
plotbothbiomass <- function(sim, sim2, specie = NULL, 
                            start_time, end_time, 
                            start_time2, end_time2,
                            y_ticks = 6, ylim = c(NA, NA), highlight = NULL, ...) {
  if (is.null(specie)) {
    specie <- sim@params@species_params$species
  }
  bm1 <- getBiomass(sim)
  bm1 <- bm1[as.numeric(dimnames(bm1)[[1]]) >= start_time & as.numeric(dimnames(bm1)[[1]]) <= end_time, , drop = FALSE]
  bm1 <- reshape2::melt(bm1)
  names(bm1) <- c("Year", "Species", "Biomass")
  bm1 <- bm1[bm1$Species %in% c("Total", specie), ]
  bm1$Sim <- "sim1"

  bm2 <- getBiomass(sim2)
  bm2 <- bm2[as.numeric(dimnames(bm2)[[1]]) >= start_time2 & as.numeric(dimnames(bm2)[[1]]) <= end_time2, , drop = FALSE]
  bm2 <- reshape2::melt(bm2)
  names(bm2) <- c("Year", "Species", "Biomass")
  bm2 <- bm2[bm2$Species %in% c("Total", specie), ]
  bm2$Sim <- "sim2"

  plot_dat <- rbind(bm1, bm2)
  ggplot2::ggplot(plot_dat, ggplot2::aes(x = Year, y = Biomass, color = Species, linetype = Sim)) +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(trans = "log10", name = "Biomass [g]") +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_linetype_manual(values = c("sim1" = "solid", "sim2" = "dashed")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
      axis.text.y = ggplot2::element_text(size = 14), legend.position = "none",
      axis.title.x = ggplot2::element_text(size = 16), axis.title.y = ggplot2::element_text(size = 16))
}


