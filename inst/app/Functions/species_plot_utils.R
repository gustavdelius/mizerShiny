# Shared utility function for processing species biomass data
# Used by both single and two-simulation species plots

process_sim_shared <- function(harvestedprojection, unharvestedprojection, chosenyear, mode = c("triple", "chosen")) {
  mode <- match.arg(mode)
  
  # Calculate three specific time points
  quarter_year <- max(1, ceiling(chosenyear * 0.25))
  half_year <- max(1, ceiling(chosenyear * 0.5))
  full_year <- chosenyear

  # Get biomass at the full year (chosen time point)
  unharvestedbio_full <-
    getBiomass(unharvestedprojection)[full_year, , drop = FALSE] %>%
    melt() %>%
    group_by(sp) %>%
    summarise(value = mean(value, na.rm = TRUE))

  harvestedbio_full <-
    getBiomass(harvestedprojection)[full_year, , drop = FALSE] %>%
    melt() %>%
    group_by(sp) %>%
    summarise(value = mean(value, na.rm = TRUE))

  percentage_diff_full <-
    left_join(harvestedbio_full, unharvestedbio_full, by = "sp") %>%
    mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
           Species = sp) %>%
    select(Species, percentage_diff) %>%
    filter(!Species %in% "Resource") %>%
    mutate(class = "full")

  if (mode == "triple") {
    # Get biomass at quarter year
    unharvestedbio_quarter <-
      getBiomass(unharvestedprojection)[quarter_year, , drop = FALSE] %>%
      melt() %>%
      group_by(sp) %>%
      summarise(value = mean(value, na.rm = TRUE))

    harvestedbio_quarter <-
      getBiomass(harvestedprojection)[quarter_year, , drop = FALSE] %>%
      melt() %>%
      group_by(sp) %>%
      summarise(value = mean(value, na.rm = TRUE))

    percentage_diff_quarter <-
      left_join(harvestedbio_quarter, unharvestedbio_quarter, by = "sp") %>%
      mutate(
        percentage_diff = ((value.x - value.y) / value.y) * 100,
        Species = sp
      ) %>%
      select(Species, percentage_diff) %>%
      filter(!Species %in% "Resource") %>%
      mutate(class = "quarter")

    # Get biomass at half year
    unharvestedbio_half <-
      getBiomass(unharvestedprojection)[half_year, , drop = FALSE] %>%
      melt() %>%
      group_by(sp) %>%
      summarise(value = mean(value, na.rm = TRUE))

    harvestedbio_half <-
      getBiomass(harvestedprojection)[half_year, , drop = FALSE] %>%
      melt() %>%
      group_by(sp) %>%
      summarise(value = mean(value, na.rm = TRUE))

    percentage_diff_half <-
      left_join(harvestedbio_half, unharvestedbio_half, by = "sp") %>%
      mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
             Species = sp) %>%
      select(Species, percentage_diff) %>%
      filter(!Species %in% "Resource") %>%
      mutate(class = "half")

    plot_data <- bind_rows(percentage_diff_quarter,
                           percentage_diff_half,
                           percentage_diff_full)
  } else {  # mode == "chosen"
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
  return(plot_data)
} 