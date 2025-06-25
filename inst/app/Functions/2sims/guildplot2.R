guildplot_both <- function(harvestedprojection1, harvestedprojection2,
                           unharvestedprojection,
                           chosenyear,
                           guildparams, celticsim,
                           mode = c("chosen", "triple")) {

  mode <- match.arg(mode)

  # Calculate three specific time points
  quarter_year <- max(1, ceiling(chosenyear * 0.25))
  half_year <- max(1, ceiling(chosenyear * 0.5))
  full_year <- chosenyear

  process_guilds <- function(mizerprojection) {

    assign_guild <- function(dat, rules) {
      dat <- dat |> dplyr::mutate(Guild = NA_character_)
      for (i in seq_len(nrow(rules))) {
        dat <- dat |>
          dplyr::mutate(
            Guild = dplyr::case_when(
              w < 0.05                                      ~ "Plank",
              is.na(Guild) &
                w >= rules$minw[i] & w < rules$maxw[i]      ~ rules$Feeding.guild[i],
              TRUE                                          ~ Guild
            )
          )
      }
      dat
    }

    mizerprojection |>
      dplyr::group_by(Species) |>
      dplyr::group_modify(\(.x, .y) {
        rules <- guildparams |>
          dplyr::filter(Species == unique(.x$Legend))
        if (nrow(rules) == 0) .x else assign_guild(.x, rules)
      }) |>
      dplyr::ungroup() |>
      tidyr::drop_na(Guild) |>
      dplyr::group_by(Guild) |>
      dplyr::summarise(value = mean(value), .groups = "drop")
  }

  if (mode == "chosen") {
    harvested_full1 <- plotSpectra(harvestedprojection1,
                                    time_range = full_year,
                                    return_data = TRUE)
    harvested_full2 <- plotSpectra(harvestedprojection2,
                                    time_range = full_year,
                                    return_data = TRUE)
    unharvested_full <- plotSpectra(unharvestedprojection,
                                     time_range = full_year,
                                     return_data = TRUE)

    guilds_full1  <- process_guilds(harvested_full1)  |> dplyr::mutate(time = "full")
    guilds_full2  <- process_guilds(harvested_full2)  |> dplyr::mutate(time = "full")
    unguilds_full <- process_guilds(unharvested_full) |> dplyr::mutate(time = "full")

    harv1_all <- guilds_full1
    harv2_all <- guilds_full2
    unharv_all <- unguilds_full

    sim1_final <- harv1_all |>
      dplyr::full_join(unharv_all, by = c("Guild", "time")) |>
      dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
      dplyr::select(Guild, time, percentage_diff) |>
      dplyr::mutate(sim = "Sim 1")

    sim2_final <- harv2_all |>
      dplyr::full_join(unharv_all, by = c("Guild", "time")) |>
      dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
      dplyr::select(Guild, time, percentage_diff) |>
      dplyr::mutate(sim = "Sim 2")

    joinedguilds <- dplyr::bind_rows(sim1_final, sim2_final)

  } else {  # mode == "triple"

    # Get spectra data at all three time points for both simulations
    hs_quarter1   <- plotSpectra(harvestedprojection1,
                               time_range = quarter_year,
                               return_data = TRUE)
    hs_half1      <- plotSpectra(harvestedprojection1,
                               time_range = half_year,
                               return_data = TRUE)
    hs_full1      <- plotSpectra(harvestedprojection1,
                               time_range = full_year,
                               return_data = TRUE)

    hs_quarter2   <- plotSpectra(harvestedprojection2,
                               time_range = quarter_year,
                               return_data = TRUE)
    hs_half2      <- plotSpectra(harvestedprojection2,
                               time_range = half_year,
                               return_data = TRUE)
    hs_full2      <- plotSpectra(harvestedprojection2,
                               time_range = full_year,
                               return_data = TRUE)

    us_quarter    <- plotSpectra(unharvestedprojection,
                               time_range = quarter_year,
                               return_data = TRUE)
    us_half       <- plotSpectra(unharvestedprojection,
                               time_range = half_year,
                               return_data = TRUE)
    us_full       <- plotSpectra(unharvestedprojection,
                               time_range = full_year,
                               return_data = TRUE)

    guilds_quarter1   <- process_guilds(hs_quarter1)   |> dplyr::mutate(time = "quarter")
    guilds_half1      <- process_guilds(hs_half1)      |> dplyr::mutate(time = "half")
    guilds_full1      <- process_guilds(hs_full1)      |> dplyr::mutate(time = "full")

    guilds_quarter2   <- process_guilds(hs_quarter2)   |> dplyr::mutate(time = "quarter")
    guilds_half2      <- process_guilds(hs_half2)      |> dplyr::mutate(time = "half")
    guilds_full2      <- process_guilds(hs_full2)      |> dplyr::mutate(time = "full")

    unguilds_quarter  <- process_guilds(us_quarter)    |> dplyr::mutate(time = "quarter")
    unguilds_half     <- process_guilds(us_half)       |> dplyr::mutate(time = "half")
    unguilds_full     <- process_guilds(us_full)       |> dplyr::mutate(time = "full")

    harv1_all <- dplyr::bind_rows(guilds_quarter1, guilds_half1, guilds_full1) |>
      dplyr::group_by(Guild, time) |>
      dplyr::summarise(value = sum(value), .groups = "drop")

    harv2_all <- dplyr::bind_rows(guilds_quarter2, guilds_half2, guilds_full2) |>
      dplyr::group_by(Guild, time) |>
      dplyr::summarise(value = sum(value), .groups = "drop")

    unharv_all <- dplyr::bind_rows(unguilds_quarter, unguilds_half, unguilds_full) |>
      dplyr::group_by(Guild, time) |>
      dplyr::summarise(value = sum(value), .groups = "drop")

    sim1_final <- harv1_all |>
      dplyr::full_join(unharv_all, by = c("Guild", "time")) |>
      dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
      dplyr::select(Guild, time, percentage_diff) |>
      dplyr::mutate(sim = "Sim 1")

    sim2_final <- harv2_all |>
      dplyr::full_join(unharv_all, by = c("Guild", "time")) |>
      dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
      dplyr::select(Guild, time, percentage_diff) |>
      dplyr::mutate(sim = "Sim 2")

    joinedguilds <- dplyr::bind_rows(sim1_final, sim2_final)

  }

  joinedguilds$time <- factor(
    joinedguilds$time,
    levels = c("quarter", "half", "full")
  )
  joinedguilds$fill_group <- interaction(joinedguilds$percentage_diff >= 0,
                                         joinedguilds$time)

  joinedguilds$Class <- factor(
    joinedguilds$fill_group,
    levels = c("FALSE.quarter", "TRUE.quarter", "FALSE.half", "TRUE.half", "FALSE.full", "TRUE.full"),
    labels = c("Quarter, Negative", "Quarter, Positive", "Half, Negative", "Half, Positive", "Full, Negative", "Full, Positive")
  )

  joinedguilds$Percentage <- joinedguilds$percentage_diff

  ggplot(joinedguilds, aes(Guild, Percentage, fill = Class)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 0, colour = "grey", linetype = "dashed", linewidth = 0.5) +
    scale_fill_manual(
      values = c(
        "Quarter, Negative"  = "#F2A488",
        "Quarter, Positive"  = "#2FA4E799",
        "Half, Negative" = "#E98C6B",
        "Half, Positive" = "#2FA4E7cc",
        "Full, Negative"   = "#E76F51",
        "Full, Positive"   = "#2FA4E7"
      ),
      drop = FALSE
    ) +
    labs(x = "Guild", y = "Percentage Change") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5),
      axis.text.y = element_text(size = 14),
      legend.position = "none",
      axis.title = element_text(size = 16)
    ) +
    facet_wrap(~ sim,nrow = 2)

}
