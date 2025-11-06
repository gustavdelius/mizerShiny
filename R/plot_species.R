# Helpers for species-level plots

#' Shared processing for species biomass plots
#'
#' Computes percentage biomass differences per species between harvested and
#' unharvested projections at one or three time ranges.
#'
#' @param harvestedprojection Harvested mizer projection
#' @param unharvestedprojection Unharvested mizer projection
#' @param chosenyear Integer defining the full period; quarter/half derived
#' @param mode Either "triple" or "chosen"
#' @return A data frame with Species, percentage_diff and class/fill_group columns
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
#'
#' Renders a bar chart of species biomass percentage change at selected times for
#' a single harvested vs unharvested comparison.
#'
#' @inheritParams process_sim_shared
#' @return A ggplot object
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
    scale_fill_manual(values = change_colours()) +
    theme_minimal() +
    theme(
      axis.text.x   = element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
      axis.text.y   = element_text(size = 14),
      legend.position = "none",
      axis.title.x  = element_text(size = 16),
      axis.title.y  = element_text(size = 16)
    )
}

#' Shared processing for actual species biomass plots
#'
#' Computes actual biomass values per species from harvested projections at one or three time ranges.
#'
#' @param harvestedprojection Harvested mizer projection
#' @param chosenyear Integer defining the full period; quarter/half derived
#' @param mode Either "triple" or "chosen"
#' @return A data frame with Species, biomass and class/fill_group columns
#' @keywords internal
process_sim_shared_actual <- function(harvestedprojection, chosenyear, mode = c("triple", "chosen")) {
  mode <- match.arg(mode)

  quarter_year <- max(1, ceiling(chosenyear * 0.25))
  half_year    <- max(1, ceiling(chosenyear * 0.5))
  full_year    <- chosenyear
  time_0       <- 1  # Initial time (time 0)

  # Get initial biomass (time 0)
  harvestedbio_initial <- getBiomass(harvestedprojection)[time_0, , drop = FALSE] |>
    melt() |>
    dplyr::group_by(sp) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE)) |>
    dplyr::mutate(Species = sp, biomass = value, class = "initial") |>
    dplyr::select(Species, biomass, class) |>
    dplyr::filter(!Species %in% "Resource")

  harvestedbio_full <- getBiomass(harvestedprojection)[full_year, , drop = FALSE] |>
    melt() |>
    dplyr::group_by(sp) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE)) |>
    dplyr::mutate(Species = sp, biomass = value, class = "full") |>
    dplyr::select(Species, biomass, class) |>
    dplyr::filter(!Species %in% "Resource")

  if (mode == "triple") {
    harvestedbio_quarter <- getBiomass(harvestedprojection)[quarter_year, , drop = FALSE] |>
      melt() |>
      dplyr::group_by(sp) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE)) |>
      dplyr::mutate(Species = sp, biomass = value, class = "quarter") |>
      dplyr::select(Species, biomass, class) |>
      dplyr::filter(!Species %in% "Resource")

    harvestedbio_half <- getBiomass(harvestedprojection)[half_year, , drop = FALSE] |>
      melt() |>
      dplyr::group_by(sp) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE)) |>
      dplyr::mutate(Species = sp, biomass = value, class = "half") |>
      dplyr::select(Species, biomass, class) |>
      dplyr::filter(!Species %in% "Resource")

    plot_data <- dplyr::bind_rows(harvestedbio_initial, harvestedbio_quarter, harvestedbio_half, harvestedbio_full)
  } else {
    plot_data <- dplyr::bind_rows(harvestedbio_initial, harvestedbio_full)
  }

  plot_data$class <- factor(plot_data$class, levels = c("initial", "quarter", "half", "full"))
  plot_data$fill_group <- factor(
    plot_data$class,
    levels = c("initial", "quarter", "half", "full"),
    labels = c("Initial", "Quarter", "Half", "Full")
  )
  plot_data
}

#' Plot species actual biomass at selected times
#'
#' Renders a bar chart of species actual biomass values at selected times for
#' a harvested projection.
#'
#' @param harvestedprojection Harvested mizer projection
#' @param chosenyear Integer defining the full period; quarter/half derived
#' @param mode Either "triple" or "chosen"
#' @return A ggplot object
#' @keywords internal
plotSpeciesActualBiomass <- function(harvestedprojection, chosenyear,
                                     mode = c("triple", "chosen")) {
  mode <- match.arg(mode)
  biomass_data <- process_sim_shared_actual(harvestedprojection, chosenyear, mode)
  biomass_data$Biomass <- biomass_data$biomass
  biomass_data$Class   <- biomass_data$fill_group

  ggplot(biomass_data, aes(x = Species, y = Biomass, fill = Class)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    labs(x = "Species", y = "Biomass [g]") +
    scale_fill_manual(values = abs_colours()) +
    # scale_y_continuous(trans = "log10") +
    theme_minimal() +
    theme(
      axis.text.x   = element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
      axis.text.y   = element_text(size = 14),
      legend.position = "none",
      axis.title.x  = element_text(size = 16),
      axis.title.y  = element_text(size = 16)
    )
}

#' Shared processing for actual species yield plots
#'
#' Computes actual yield values per species and gear from harvested projections at one or three time ranges.
#'
#' @param harvestedprojection Harvested mizer projection
#' @param chosenyear Integer defining the full period; quarter/half derived
#' @param mode Either "triple" or "chosen"
#' @return A data frame with Species, Gear, yield, class and fill_group columns
#' @keywords internal
process_sim_shared_actual_yield <- function(harvestedprojection, chosenyear, mode = c("triple", "chosen")) {
  mode <- match.arg(mode)

  quarter_year <- max(1, ceiling(chosenyear * 0.25))
  half_year    <- max(1, ceiling(chosenyear * 0.5))
  full_year    <- chosenyear
  time_0       <- 1  # Initial time (time 0)

  # Get yield gear data (time x gear x species)
  yield_gear_array <- getYieldGear(harvestedprojection)

  # Get initial yield (time 0) by gear
  harvestedyield_initial <- yield_gear_array[time_0, , , drop = FALSE] |>
    melt() |>
    dplyr::filter(!sp %in% "Resource") |>
    dplyr::group_by(sp, gear) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Species = sp, Gear = gear, yield = value, class = "initial") |>
    dplyr::select(Species, Gear, yield, class)

  harvestedyield_full <- yield_gear_array[full_year, , , drop = FALSE] |>
    melt() |>
    dplyr::filter(!sp %in% "Resource") |>
    dplyr::group_by(sp, gear) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Species = sp, Gear = gear, yield = value, class = "full") |>
    dplyr::select(Species, Gear, yield, class)

  if (mode == "triple") {
    harvestedyield_quarter <- yield_gear_array[quarter_year, , , drop = FALSE] |>
      melt() |>
      dplyr::filter(!sp %in% "Resource") |>
      dplyr::group_by(sp, gear) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(Species = sp, Gear = gear, yield = value, class = "quarter") |>
      dplyr::select(Species, Gear, yield, class)

    harvestedyield_half <- yield_gear_array[half_year, , , drop = FALSE] |>
      melt() |>
      dplyr::filter(!sp %in% "Resource") |>
      dplyr::group_by(sp, gear) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(Species = sp, Gear = gear, yield = value, class = "half") |>
      dplyr::select(Species, Gear, yield, class)

    plot_data <- dplyr::bind_rows(harvestedyield_initial, harvestedyield_quarter, harvestedyield_half, harvestedyield_full)
  } else {
    plot_data <- dplyr::bind_rows(harvestedyield_initial, harvestedyield_full)
  }

  plot_data$class <- factor(plot_data$class, levels = c("initial", "quarter", "half", "full"))
  plot_data$fill_group <- factor(
    plot_data$class,
    levels = c("initial", "quarter", "half", "full"),
    labels = c("Initial", "Quarter", "Half", "Full")
  )
  plot_data
}

#' Plot species actual yield at selected times
#'
#' Renders a stacked bar chart of species actual yield values by gear at selected times for
#' a harvested projection.
#'
#' @param harvestedprojection Harvested mizer projection
#' @param chosenyear Integer defining the full period; quarter/half derived
#' @param mode Either "triple" or "chosen"
#' @return A ggplot object
#' @keywords internal
plotSpeciesActualYield <- function(harvestedprojection, chosenyear,
                                     mode = c("triple", "chosen")) {
  mode <- match.arg(mode)
  yield_data <- process_sim_shared_actual_yield(harvestedprojection, chosenyear, mode)
  yield_data$Yield <- yield_data$yield
  yield_data$TimeClass <- yield_data$fill_group

  # Get unique gears and assign colors
  gears <- sort(unique(yield_data$Gear))
  n_gears <- length(gears)

  # Generate colors for gears (using a color palette)
  if (n_gears <= 8) {
    gear_colors <- RColorBrewer::brewer.pal(max(3, n_gears), "Set2")[1:n_gears]
  } else {
    gear_colors <- grDevices::rainbow(n_gears)
  }
  names(gear_colors) <- gears

  # Define opacities for different time points
  opacity_values <- c(
    "Initial" = 1.0,
    "Quarter" = 0.8,
    "Half"    = 0.6,
    "Full"    = 0.4
  )

  # Calculate cumulative positions for stacking manually
  # Sort by Species, then by Time, then by Gear (for consistent stacking order)
  yield_data <- yield_data |>
    dplyr::arrange(Species, class, Gear) |>
    dplyr::group_by(Species, class) |>
    dplyr::mutate(
      Ymin = dplyr::lag(cumsum(Yield), default = 0),
      Ymax = cumsum(Yield)
    ) |>
    dplyr::ungroup()

  # Create position offsets for dodging bars by time
  time_levels <- c("initial", "quarter", "half", "full")
  # Get actual time classes present in data, in order
  actual_time_classes <- unique(yield_data$class)
  actual_time_classes <- actual_time_classes[order(match(actual_time_classes, time_levels))]
  n_times <- length(actual_time_classes)
  dodge_width <- 0.8
  dodge_offset <- (seq_len(n_times) - (n_times + 1) / 2) * dodge_width / n_times

  yield_data <- yield_data |>
    dplyr::mutate(
      TimeNum = as.numeric(factor(class, levels = actual_time_classes)),
      SpeciesNum = as.numeric(factor(Species)),
      XPos = SpeciesNum + dodge_offset[TimeNum],
      BarWidth = dodge_width / n_times
    )

  # Convert Species to factor to use discrete scale
  # This allows the module to override with scale_x_discrete(limits = ...)
  yield_data$Species <- factor(yield_data$Species, levels = unique(yield_data$Species))

  # Create text for tooltip
  yield_data$tooltip_text <- paste0("Gear: ", yield_data$Gear, "<br>",
                                     "TimeClass: ", yield_data$TimeClass, "<br>",
                                     "Yield: ", round(yield_data$Yield, 2), " g/year")

  # Use geom_rect for manual stacking and dodging
  # Map Species to numeric positions but keep as factor for discrete scale
  # Note: 'text' is a plotly aesthetic, not ggplot2, so we suppress the warning
  p <- suppressWarnings(
    ggplot(yield_data) +
      geom_rect(aes(xmin = XPos - BarWidth/2,
                    xmax = XPos + BarWidth/2,
                    ymin = Ymin, ymax = Ymax,
                    fill = Gear, alpha = TimeClass, text = tooltip_text),
                show.legend = c(fill = TRUE, alpha = FALSE))
  ) +
    scale_x_continuous(breaks = seq_along(levels(yield_data$Species)),
                       labels = levels(yield_data$Species)) +
    scale_fill_manual(values = gear_colors, name = "Gear") +
    scale_alpha_manual(values = opacity_values, guide = "none") +
    guides(alpha = "none", fill = guide_legend(override.aes = list(alpha = 1))) +
    labs(x = "Species", y = "Yield [g/year]") +
    # scale_y_continuous(trans = "log10") +
    theme_minimal() +
    theme(
      axis.text.x   = element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
      axis.text.y   = element_text(size = 14),
      legend.position = "right",
      axis.title.x  = element_text(size = 16),
      axis.title.y  = element_text(size = 16)
    )

  p
}

#' Shared processing for species yield change plots
#'
#' Computes percentage yield differences per species between harvested and
#' unharvested projections at one or three time ranges.
#'
#' @param harvestedprojection Harvested mizer projection
#' @param unharvestedprojection Unharvested mizer projection
#' @param chosenyear Integer defining the full period; quarter/half derived
#' @param mode Either "triple" or "chosen"
#' @return A data frame with Species, percentage_diff and class/fill_group columns
#' @keywords internal
process_sim_shared_yield <- function(harvestedprojection, unharvestedprojection, chosenyear, mode = c("triple", "chosen")) {
  mode <- match.arg(mode)

  quarter_year <- max(1, ceiling(chosenyear * 0.25))
  half_year    <- max(1, ceiling(chosenyear * 0.5))
  full_year    <- chosenyear

  unharvestedyield_full <- getYield(unharvestedprojection)[full_year, , drop = FALSE] |>
    melt() |>
    dplyr::group_by(sp) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE))

  harvestedyield_full <- getYield(harvestedprojection)[full_year, , drop = FALSE] |>
    melt() |>
    dplyr::group_by(sp) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE))

  percentage_diff_full <- dplyr::left_join(harvestedyield_full, unharvestedyield_full, by = "sp") |>
    dplyr::mutate(percentage_diff = ifelse(value.y == 0,
                                           ifelse(value.x == 0, 0, Inf),
                                           ((value.x - value.y) / value.y) * 100),
                  Species = sp) |>
    dplyr::select(Species, percentage_diff) |>
    dplyr::filter(!Species %in% "Resource") |>
    dplyr::mutate(class = "full")

  if (mode == "triple") {
    unharvestedyield_quarter <- getYield(unharvestedprojection)[quarter_year, , drop = FALSE] |>
      melt() |>
      dplyr::group_by(sp) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE))

    harvestedyield_quarter <- getYield(harvestedprojection)[quarter_year, , drop = FALSE] |>
      melt() |>
      dplyr::group_by(sp) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE))

    percentage_diff_quarter <- dplyr::left_join(harvestedyield_quarter, unharvestedyield_quarter, by = "sp") |>
      dplyr::mutate(percentage_diff = ifelse(value.y == 0,
                                             ifelse(value.x == 0, 0, Inf),
                                             ((value.x - value.y) / value.y) * 100),
                    Species = sp) |>
      dplyr::select(Species, percentage_diff) |>
      dplyr::filter(!Species %in% "Resource") |>
      dplyr::mutate(class = "quarter")

    unharvestedyield_half <- getYield(unharvestedprojection)[half_year, , drop = FALSE] |>
      melt() |>
      dplyr::group_by(sp) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE))

    harvestedyield_half <- getYield(harvestedprojection)[half_year, , drop = FALSE] |>
      melt() |>
      dplyr::group_by(sp) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE))

    percentage_diff_half <- dplyr::left_join(harvestedyield_half, unharvestedyield_half, by = "sp") |>
      dplyr::mutate(percentage_diff = ifelse(value.y == 0,
                                             ifelse(value.x == 0, 0, Inf),
                                             ((value.x - value.y) / value.y) * 100),
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

#' Plot species yield change at selected times
#'
#' Renders a bar chart of species yield percentage change at selected times for
#' a single harvested vs unharvested comparison.
#'
#' @inheritParams process_sim_shared_yield
#' @return A ggplot object
#' @keywords internal
plotSpeciesYieldChange <- function(harvestedprojection, unharvestedprojection, chosenyear,
                                     mode = c("triple", "chosen")) {
  mode <- match.arg(mode)
  percentage_diff <- process_sim_shared_yield(harvestedprojection, unharvestedprojection, chosenyear, mode)
  percentage_diff$Percentage <- percentage_diff$percentage_diff
  percentage_diff$Class      <- percentage_diff$fill_group

  ggplot(percentage_diff, aes(x = Species, y = Percentage, fill = Class)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 0, color = "grey", linetype = "dashed", linewidth = 0.5) +
    labs(x = "Species", y = "Yield % Change") +
    scale_fill_manual(values = change_colours()) +
    theme_minimal() +
    theme(
      axis.text.x   = element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
      axis.text.y   = element_text(size = 14),
      legend.position = "none",
      axis.title.x  = element_text(size = 16),
      axis.title.y  = element_text(size = 16)
    )
}

