# Two-simulation variants of species/guild plots

#' Plot species biomass change for two simulations
#'
#' Uses shared processing to compute percentage biomass changes for each species
#' at selected times, and renders separate facets for two simulations.
#'
#' @param harvestedprojection1 First harvested mizer projection
#' @param harvestedprojection2 Second harvested mizer projection
#' @param unharvestedprojection Baseline unharvested mizer projection
#' @param chosenyear Integer defining full period; quarter/half derived
#' @param mode Either "triple" or "chosen"
#' @return A ggplot object
#' @keywords internal
plotSpeciesWithTimeRange2 <- function(harvestedprojection1, harvestedprojection2,
                                      unharvestedprojection, chosenyear, mode = c("triple", "chosen")) {
  mode <- match.arg(mode)
  df1 <- process_sim_shared(harvestedprojection1, unharvestedprojection, chosenyear, mode)
  df2 <- process_sim_shared(harvestedprojection2, unharvestedprojection, chosenyear, mode)

  df1$sim <- "Sim 1"
  df2$sim <- "Sim 2"
  plot_df <- dplyr::bind_rows(df1, df2)

  ggplot2::ggplot(plot_df, ggplot2::aes(x = Species, y = percentage_diff, fill = fill_group)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed", linewidth = 0.5) +
    ggplot2::labs(x = "Species", y = "Biomass % Change") +
    ggplot2::scale_fill_manual(values = change_colours()) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 14),
                   legend.position = "none",
                   axis.title.x = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 16),
                   panel.spacing.y = grid::unit(2, "lines")) +
    ggplot2::facet_wrap(~ sim, nrow = 2)
}

#' Plot guild biomass change for two simulations
#'
#' Compares guild-level biomass between each harvested projection and a common
#' unharvested baseline at one or three time ranges.
#'
#' @inheritParams plotSpeciesWithTimeRange2
#' @param guildparams Data frame of guild rules
#' @param celticsim Unused; present for compatibility
#' @param mode Either "chosen" or "triple"
#' @return A ggplot object
#' @keywords internal
guildplot_both <- function(harvestedprojection1, harvestedprojection2,
                           unharvestedprojection, chosenyear, guildparams, celticsim,
                           mode = c("chosen", "triple")) {
  mode <- match.arg(mode)

  quarter_year <- max(1, ceiling(chosenyear * 0.25))
  half_year    <- max(1, ceiling(chosenyear * 0.5))
  full_year    <- chosenyear

  process_guilds <- function(mizerprojection) {
    assign_guild <- function(dat, rules) {
      dat <- dat |> dplyr::mutate(Guild = NA_character_)
      for (i in seq_len(nrow(rules))) {
        dat <- dat |>
          dplyr::mutate(
            Guild = dplyr::case_when(
              w < 0.05                                      ~ "Plank",
              is.na(Guild) & w >= rules$minw[i] & w < rules$maxw[i] ~ rules$Feeding.guild[i],
              TRUE                                          ~ Guild
            )
          )
      }
      dat
    }

    mizerprojection |>
      dplyr::group_by(Species) |>
      dplyr::group_modify(function(.x, .y){
        rules <- guildparams |> dplyr::filter(Species == unique(.x$Legend))
        if (nrow(rules) == 0) .x else assign_guild(.x, rules)
      }) |>
      dplyr::ungroup() |>
      tidyr::drop_na(Guild) |>
      dplyr::group_by(Guild) |>
      dplyr::summarise(value = mean(value), .groups = "drop")
  }

  if (mode == "chosen") {
    harvested_full1   <- plotSpectra(harvestedprojection1,   time_range = full_year, return_data = TRUE)
    harvested_full2   <- plotSpectra(harvestedprojection2,   time_range = full_year, return_data = TRUE)
    unharvested_full  <- plotSpectra(unharvestedprojection, time_range = full_year,  return_data = TRUE)

    sim1_final <- process_guilds(harvested_full1)  |> dplyr::mutate(time = "full") |>
      dplyr::full_join(process_guilds(unharvested_full) |> dplyr::mutate(time = "full"), by = c("Guild", "time")) |>
      dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
      dplyr::select(Guild, time, percentage_diff) |>
      dplyr::mutate(sim = "Sim 1")

    sim2_final <- process_guilds(harvested_full2)  |> dplyr::mutate(time = "full") |>
      dplyr::full_join(process_guilds(unharvested_full) |> dplyr::mutate(time = "full"), by = c("Guild", "time")) |>
      dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
      dplyr::select(Guild, time, percentage_diff) |>
      dplyr::mutate(sim = "Sim 2")

    joinedguilds <- dplyr::bind_rows(sim1_final, sim2_final)
  } else {
    # triple mode (similar to original)
    hs_quarter1 <- plotSpectra(harvestedprojection1, time_range = quarter_year, return_data = TRUE)
    hs_half1    <- plotSpectra(harvestedprojection1, time_range = half_year,    return_data = TRUE)
    hs_full1    <- plotSpectra(harvestedprojection1, time_range = full_year,    return_data = TRUE)
    hs_quarter2 <- plotSpectra(harvestedprojection2, time_range = quarter_year, return_data = TRUE)
    hs_half2    <- plotSpectra(harvestedprojection2, time_range = half_year,    return_data = TRUE)
    hs_full2    <- plotSpectra(harvestedprojection2, time_range = full_year,    return_data = TRUE)
    us_quarter  <- plotSpectra(unharvestedprojection, time_range = quarter_year, return_data = TRUE)
    us_half     <- plotSpectra(unharvestedprojection, time_range = half_year,    return_data = TRUE)
    us_full     <- plotSpectra(unharvestedprojection, time_range = full_year,    return_data = TRUE)

    guilds1 <- dplyr::bind_rows(
      process_guilds(hs_quarter1) |> dplyr::mutate(time = "quarter"),
      process_guilds(hs_half1)    |> dplyr::mutate(time = "half"),
      process_guilds(hs_full1)    |> dplyr::mutate(time = "full")
    )
    guilds2 <- dplyr::bind_rows(
      process_guilds(hs_quarter2) |> dplyr::mutate(time = "quarter"),
      process_guilds(hs_half2)    |> dplyr::mutate(time = "half"),
      process_guilds(hs_full2)    |> dplyr::mutate(time = "full")
    )
    unharv  <- dplyr::bind_rows(
      process_guilds(us_quarter)  |> dplyr::mutate(time = "quarter"),
      process_guilds(us_half)     |> dplyr::mutate(time = "half"),
      process_guilds(us_full)     |> dplyr::mutate(time = "full")
    )

    sim1_final <- guilds1 |>
      dplyr::group_by(Guild, time) |>
      dplyr::summarise(value = sum(value), .groups = "drop") |>
      dplyr::full_join(unharv, by = c("Guild","time")) |>
      dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
      dplyr::select(Guild, time, percentage_diff) |>
      dplyr::mutate(sim = "Sim 1")

    sim2_final <- guilds2 |>
      dplyr::group_by(Guild, time) |>
      dplyr::summarise(value = sum(value), .groups = "drop") |>
      dplyr::full_join(unharv, by = c("Guild","time")) |>
      dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
      dplyr::select(Guild, time, percentage_diff) |>
      dplyr::mutate(sim = "Sim 2")

    joinedguilds <- dplyr::bind_rows(sim1_final, sim2_final)
  }

  joinedguilds$time <- factor(joinedguilds$time, levels = c("quarter", "half", "full"))
  joinedguilds$fill_group <- interaction(joinedguilds$percentage_diff >= 0, joinedguilds$time)
  joinedguilds$Class <- factor(joinedguilds$fill_group,
                               levels = c("FALSE.quarter", "TRUE.quarter", "FALSE.half", "TRUE.half", "FALSE.full", "TRUE.full"),
                               labels = c("Quarter, Negative", "Quarter, Positive", "Half, Negative", "Half, Positive", "Full, Negative", "Full, Positive"))
  joinedguilds$Percentage <- joinedguilds$percentage_diff

  ggplot2::ggplot(joinedguilds, ggplot2::aes(Guild, Percentage, fill = Class)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey", linetype = "dashed", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = change_colours(), drop = FALSE) +
    ggplot2::labs(x = "Guild", y = "Biomass % Change") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14, angle = 90, vjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 14),
                   legend.position = "none",
                   axis.title = ggplot2::element_text(size = 16)) +
    ggplot2::facet_wrap(~ sim, nrow = 2)
}

#' Plot species actual biomass for two simulations
#'
#' Uses shared processing to compute actual biomass values for each species
#' at selected times, and renders separate facets for two simulations.
#'
#' @param harvestedprojection1 First harvested mizer projection
#' @param harvestedprojection2 Second harvested mizer projection
#' @param chosenyear Integer defining full period; quarter/half derived
#' @param mode Either "triple" or "chosen"
#' @return A ggplot object
#' @keywords internal
plotSpeciesActualBiomass2 <- function(harvestedprojection1, harvestedprojection2,
                                      chosenyear, mode = c("triple", "chosen")) {
  mode <- match.arg(mode)
  df1 <- process_sim_shared_actual(harvestedprojection1, chosenyear, mode)
  df2 <- process_sim_shared_actual(harvestedprojection2, chosenyear, mode)

  df1$sim <- "Sim 1"
  df2$sim <- "Sim 2"
  plot_df <- dplyr::bind_rows(df1, df2)

  ggplot2::ggplot(plot_df, ggplot2::aes(x = Species, y = biomass, fill = fill_group)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::labs(x = "Species", y = "Biomass [g]") +
    ggplot2::scale_fill_manual(values = abs_colours()) +
    # ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 14),
                   legend.position = "none",
                   axis.title.x = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 16),
                   panel.spacing.y = grid::unit(2, "lines")) +
    ggplot2::facet_wrap(~ sim, nrow = 2)
}

#' Plot species actual yield for two simulations
#'
#' Uses shared processing to compute actual yield values for each species
#' at selected times, and renders separate facets for two simulations.
#'
#' @param harvestedprojection1 First harvested mizer projection
#' @param harvestedprojection2 Second harvested mizer projection
#' @param chosenyear Integer defining full period; quarter/half derived
#' @param mode Either "triple" or "chosen"
#' @return A ggplot object
#' @keywords internal
plotSpeciesActualYield2 <- function(harvestedprojection1, harvestedprojection2,
                                      chosenyear, mode = c("triple", "chosen")) {
  mode <- match.arg(mode)
  df1 <- process_sim_shared_actual_yield(harvestedprojection1, chosenyear, mode)
  df2 <- process_sim_shared_actual_yield(harvestedprojection2, chosenyear, mode)

  df1$sim <- "Sim 1"
  df2$sim <- "Sim 2"
  plot_df <- dplyr::bind_rows(df1, df2)

  # Get unique gears and assign colors
  gears <- sort(unique(plot_df$Gear))
  n_gears <- length(gears)

  # Generate colors for gears
  if (n_gears <= 8) {
    gear_colors <- RColorBrewer::brewer.pal(max(3, n_gears), "Set2")[1:n_gears]
  } else {
    gear_colors <- grDevices::rainbow(n_gears)
  }
  names(gear_colors) <- gears

  # Define opacities for different time points
  opacity_values <- c(
    "Initial" = 0.4,
    "Quarter" = 0.6,
    "Half"    = 0.8,
    "Full"    = 1.0
  )

  # Calculate cumulative positions for stacking manually
  plot_df <- plot_df |>
    dplyr::arrange(sim, Species, class, Gear) |>
    dplyr::group_by(sim, Species, class) |>
    dplyr::mutate(
      Ymin = dplyr::lag(cumsum(yield), default = 0),
      Ymax = cumsum(yield)
    ) |>
    dplyr::ungroup()

  # Create position offsets for dodging bars by time
  time_levels <- c("initial", "quarter", "half", "full")
  n_times <- length(unique(plot_df$class))
  dodge_width <- 0.8
  dodge_offset <- (seq_len(n_times) - (n_times + 1) / 2) * dodge_width / n_times

  plot_df <- plot_df |>
    dplyr::mutate(
      TimeNum = as.numeric(factor(class, levels = time_levels)),
      SpeciesNum = as.numeric(factor(Species)),
      XPos = SpeciesNum + dodge_offset[TimeNum],
      BarWidth = dodge_width / n_times,
      TimeClass = fill_group
    )

  # Get unique species in order for labeling
  species_order <- unique(plot_df$Species)

  # Use geom_rect for manual stacking and dodging
  p <- ggplot2::ggplot(plot_df) +
    ggplot2::geom_rect(ggplot2::aes(xmin = XPos - BarWidth/2,
                  xmax = XPos + BarWidth/2,
                  ymin = Ymin, ymax = Ymax,
                  fill = Gear, alpha = TimeClass)) +
    ggplot2::scale_x_continuous(breaks = seq_along(species_order),
                       labels = species_order) +
    ggplot2::scale_fill_manual(values = gear_colors, name = "Gear") +
    ggplot2::scale_alpha_manual(values = opacity_values, name = "Time") +
    ggplot2::labs(x = "Species", y = "Yield [g/year]") +
    # ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 14),
                   legend.position = "right",
                   axis.title.x = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 16),
                   panel.spacing.y = grid::unit(2, "lines")) +
    ggplot2::facet_wrap(~ sim, nrow = 2)

  p
}

#' Plot species yield change for two simulations
#'
#' Uses shared processing to compute percentage yield changes for each species
#' at selected times, and renders separate facets for two simulations.
#'
#' @param harvestedprojection1 First harvested mizer projection
#' @param harvestedprojection2 Second harvested mizer projection
#' @param unharvestedprojection Baseline unharvested mizer projection
#' @param chosenyear Integer defining full period; quarter/half derived
#' @param mode Either "triple" or "chosen"
#' @return A ggplot object
#' @keywords internal
plotSpeciesYieldChange2 <- function(harvestedprojection1, harvestedprojection2,
                                      unharvestedprojection, chosenyear, mode = c("triple", "chosen")) {
  mode <- match.arg(mode)
  df1 <- process_sim_shared_yield(harvestedprojection1, unharvestedprojection, chosenyear, mode)
  df2 <- process_sim_shared_yield(harvestedprojection2, unharvestedprojection, chosenyear, mode)

  df1$sim <- "Sim 1"
  df2$sim <- "Sim 2"
  plot_df <- dplyr::bind_rows(df1, df2)

  ggplot2::ggplot(plot_df, ggplot2::aes(x = Species, y = percentage_diff, fill = fill_group)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed", linewidth = 0.5) +
    ggplot2::labs(x = "Species", y = "Yield % Change") +
    ggplot2::scale_fill_manual(values = change_colours()) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 14),
                   legend.position = "none",
                   axis.title.x = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 16),
                   panel.spacing.y = grid::unit(2, "lines")) +
    ggplot2::facet_wrap(~ sim, nrow = 2)
}


