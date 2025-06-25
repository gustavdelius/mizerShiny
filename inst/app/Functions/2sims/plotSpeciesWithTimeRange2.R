process_sim <- function(harvestedprojection, unharvestedprojection, chosenyear) {

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

plotSpeciesWithTimeRange2 <- function(harvestedprojection1, harvestedprojection2,
                                      unharvestedprojection, chosenyear) {
  df1 <- process_sim(harvestedprojection1, unharvestedprojection, chosenyear)
  df2 <- process_sim(harvestedprojection2, unharvestedprojection, chosenyear)

  df1$sim <- "Sim 1"
  df2$sim <- "Sim 2"

  plot_df <- bind_rows(df1, df2)

  p <- ggplot(plot_df, aes(x = Species, y = percentage_diff, fill = fill_group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 0, color = "grey", linetype = "dashed", linewidth = 0.5) +
    labs(x = "Species", y = "% Change") +
    scale_fill_manual(values = c(
      "Quarter, Negative"  = "#F2A488",
      "Quarter, Positive"  = "#2FA4E799",
      "Half, Negative" = "#E98C6B",
      "Half, Positive" = "#2FA4E7cc",
      "Full, Negative"   = "#E76F51",
      "Full, Positive"   = "#2FA4E7"
    )) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 14),
          legend.position = "none",
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          panel.spacing.y = unit(2, "lines")) +
    facet_wrap(~ sim, nrow = 2)

  return(p)
}
