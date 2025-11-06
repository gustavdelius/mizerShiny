# Guild plots

#' Plot guild biomass change
#'
#' Computes mean biomass within guilds over selected time ranges and visualizes
#' percentage differences between harvested and unharvested projections.
#'
#' @param harvestedprojection A mizer projection with harvesting
#' @param unharvestedprojection A mizer projection without harvesting
#' @param chosenyear Integer year index defining the full period; quarter/half derived
#' @param guildparams Data frame with guild assignment rules per species
#' @param celticsim Unused; kept for backward compatibility
#' @param mode Either "chosen" for full only or "triple" for quarter/half/full
#' @return A ggplot object
#' @keywords internal
guildplot <- function(harvestedprojection, unharvestedprojection, chosenyear, guildparams, celticsim,
                      mode = c("chosen", "triple")) {
  mode <- match.arg(mode)

  quarter_year <- max(1, ceiling(chosenyear * 0.25))
  half_year    <- max(1, ceiling(chosenyear * 0.5))
  full_year    <- chosenyear

  harvested_full   <- plotSpectra(harvestedprojection,   time_range = full_year,   return_data = TRUE)
  unharvested_full <- plotSpectra(unharvestedprojection, time_range = full_year,   return_data = TRUE)

  if (mode == "triple") {
    harvested_quarter   <- plotSpectra(harvestedprojection,   time_range = quarter_year, return_data = TRUE)
    harvested_half      <- plotSpectra(harvestedprojection,   time_range = half_year,    return_data = TRUE)
    unharvested_quarter <- plotSpectra(unharvestedprojection, time_range = quarter_year, return_data = TRUE)
    unharvested_half    <- plotSpectra(unharvestedprojection, time_range = half_year,    return_data = TRUE)
  }

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

  guilds_full   <- process_guilds(harvested_full)   |> dplyr::mutate(time = "full")
  unguilds_full <- process_guilds(unharvested_full) |> dplyr::mutate(time = "full")

  if (mode == "triple") {
    guilds_quarter   <- process_guilds(harvested_quarter)   |> dplyr::mutate(time = "quarter")
    guilds_half      <- process_guilds(harvested_half)      |> dplyr::mutate(time = "half")
    unguilds_quarter <- process_guilds(unharvested_quarter) |> dplyr::mutate(time = "quarter")
    unguilds_half    <- process_guilds(unharvested_half)    |> dplyr::mutate(time = "half")

    harv_all   <- dplyr::bind_rows(guilds_full, guilds_quarter, guilds_half)
    unharv_all <- dplyr::bind_rows(unguilds_full, unguilds_quarter, unguilds_half)
  } else {
    harv_all   <- guilds_full
    unharv_all <- unguilds_full
  }

  joinedguilds <- harv_all |>
    dplyr::full_join(unharv_all, by = c("Guild","time")) |>
    dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
    dplyr::select(Guild, time, percentage_diff)

  joinedguilds$time <- factor(joinedguilds$time, levels = if (mode == "chosen") "full" else c("quarter","half","full"))
  joinedguilds$fill_group <- interaction(joinedguilds$percentage_diff >= 0, joinedguilds$time)
  joinedguilds$Class <- factor(joinedguilds$fill_group,
    levels = c("FALSE.quarter", "TRUE.quarter", "FALSE.half", "TRUE.half", "FALSE.full", "TRUE.full"),
    labels = c("Quarter, Negative", "Quarter, Positive", "Half, Negative", "Half, Positive", "Full, Negative", "Full, Positive")
  )
  joinedguilds$Percentage <- joinedguilds$percentage_diff

  ggplot2::ggplot(joinedguilds, ggplot2::aes(Guild, Percentage, fill = Class)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey", linetype = "dashed", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = change_colours(), drop = FALSE) +
    ggplot2::labs(x = "Guild", y = "Biomass % Change") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14, angle = 90, vjust = 0.5),
      axis.text.y = ggplot2::element_text(size = 14),
      legend.position = "none",
      axis.title = ggplot2::element_text(size = 16)
    )
}


