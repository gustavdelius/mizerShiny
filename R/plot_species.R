# Helpers for species-level plots

#' Shared processing for species biomass plots
#' @keywords internal
process_sim_shared <- function(harvestedprojection, unharvestedprojection, chosenyear, mode = c("triple", "chosen")) {
  mode <- match.arg(mode)

  quarter_year <- max(1, ceiling(chosenyear * 0.25))
  half_year    <- max(1, ceiling(chosenyear * 0.5))
  full_year    <- chosenyear

  unharvestedbio_full <- getBiomass(unharvestedprojection)[full_year, , drop = FALSE] |>
    melt() |>
    dplyr::group_by(sp) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE))

  harvestedbio_full <- getBiomass(harvestedprojection)[full_year, , drop = FALSE] |>
    melt() |>
    dplyr::group_by(sp) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE))

  percentage_diff_full <- dplyr::left_join(harvestedbio_full, unharvestedbio_full, by = "sp") |>
    dplyr::mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
                  Species = sp) |>
    dplyr::select(Species, percentage_diff) |>
    dplyr::filter(!Species %in% "Resource") |>
    dplyr::mutate(class = "full")

  if (mode == "triple") {
    unharvestedbio_quarter <- getBiomass(unharvestedprojection)[quarter_year, , drop = FALSE] |>
      melt() |>
      dplyr::group_by(sp) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE))

    harvestedbio_quarter <- getBiomass(harvestedprojection)[quarter_year, , drop = FALSE] |>
      melt() |>
      dplyr::group_by(sp) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE))

    percentage_diff_quarter <- dplyr::left_join(harvestedbio_quarter, unharvestedbio_quarter, by = "sp") |>
      dplyr::mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
                    Species = sp) |>
      dplyr::select(Species, percentage_diff) |>
      dplyr::filter(!Species %in% "Resource") |>
      dplyr::mutate(class = "quarter")

    unharvestedbio_half <- getBiomass(unharvestedprojection)[half_year, , drop = FALSE] |>
      melt() |>
      dplyr::group_by(sp) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE))

    harvestedbio_half <- getBiomass(harvestedprojection)[half_year, , drop = FALSE] |>
      melt() |>
      dplyr::group_by(sp) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE))

    percentage_diff_half <- dplyr::left_join(harvestedbio_half, unharvestedbio_half, by = "sp") |>
      dplyr::mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
                    Species = sp) |>
      dplyr::select(Species, percentage_diff) |>
      dplyr::filter(!Species %in% "Resource") |>
      dplyr::mutate(class = "half")

    plot_data <- dplyr::bind_rows(percentage_diff_quarter, percentage_diff_half, percentage_diff_full)
  } else {
    plot_data <- percentage_diff_full
  }

  plot_data$class <- factor(plot_data$class, levels = c("quarter", "half", "full"))
  plot_data$fill_group <- interaction(plot_data$percentage_diff >= 0, plot_data$class)
  plot_data$fill_group <- factor(
    plot_data$fill_group,
    levels = c("FALSE.quarter", "TRUE.quarter",
               "FALSE.half", "TRUE.half",
               "FALSE.full", "TRUE.full"),
    labels = c("Quarter, Negative", "Quarter, Positive",
               "Half, Negative", "Half, Positive",
               "Full, Negative", "Full, Positive")
  )
  plot_data
}

#' Plot species biomass change at selected times
#' @keywords internal
plotSpeciesWithTimeRange <- function(harvestedprojection, unharvestedprojection, chosenyear,
                                     mode = c("triple", "chosen")) {
  mode <- match.arg(mode)
  percentage_diff <- process_sim_shared(harvestedprojection, unharvestedprojection, chosenyear, mode)
  percentage_diff$Percentage <- percentage_diff$percentage_diff
  percentage_diff$Class      <- percentage_diff$fill_group

  ggplot(percentage_diff, aes(x = Species, y = Percentage, fill = Class)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 0, color = "grey", linetype = "dashed", linewidth = 0.5) +
    labs(x = "Species", y = "Biomass % Change") +
    scale_fill_manual(values = c(
      "Quarter, Negative" = "#F2A488",
      "Quarter, Positive" = "#2FA4E799",
      "Half, Negative"    = "#E98C6B",
      "Half, Positive"    = "#2FA4E7cc",
      "Full, Negative"    = "#E76F51",
      "Full, Positive"    = "#2FA4E7"
    )) +
    theme_minimal() +
    theme(
      axis.text.x   = element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
      axis.text.y   = element_text(size = 14),
      legend.position = "none",
      axis.title.x  = element_text(size = 16),
      axis.title.y  = element_text(size = 16)
    )
}


