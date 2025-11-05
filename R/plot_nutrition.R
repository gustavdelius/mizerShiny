# Nutrition plot helper and data loading

#' Calculate total nutrients from a simulation at given time steps
#'
#' Helper function to compute nutrient totals from yield data.
#'
#' @param sim Mizer projection object
#' @param steps Integer time indices to evaluate (can be a range)
#' @return Named vector of nutrient totals
#' @keywords internal
calc_nutrition_totals <- function(sim, steps) {
  nut_cols <- setdiff(names(nut), "species")
  y <- getYield(sim)
  yield_df <- tibble(
    fmp   = colnames(y),
    Yield = colMeans((y[steps, , drop = FALSE]))
  )
  yield_df |>
    dplyr::left_join(all_matches,  by = "fmp") |>
    dplyr::left_join(nut,           by = c(nut_match = "species")) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(nut_cols), ~ .x * Yield)) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(nut_cols), ~ sum(.x, na.rm = TRUE))) |>
    unlist(use.names = TRUE)
}

#' Shared processing for nutrition change plots
#'
#' Computes percentage nutrition differences per nutrient between harvested and
#' unharvested projections at one or three time ranges.
#'
#' @param harvestedprojection Harvested mizer projection
#' @param unharvestedprojection Unharvested mizer projection
#' @param chosenyear Integer defining the full period; quarter/half derived
#' @param mode Either "triple" or "chosen"
#' @return A data frame with Nutrient, percentage_diff and class/fill_group columns
#' @keywords internal
process_nutrition_change <- function(harvestedprojection, unharvestedprojection, chosenyear, mode = c("triple", "chosen")) {
  mode <- match.arg(mode)
  nut_cols <- setdiff(names(nut), "species")

  quarter_year <- max(1, ceiling(chosenyear * 0.25))
  half_year    <- max(1, ceiling(chosenyear * 0.5))
  full_year    <- chosenyear

  # Calculate nutrition totals for full year
  unharvested_nut_full <- calc_nutrition_totals(unharvestedprojection, full_year)
  harvested_nut_full <- calc_nutrition_totals(harvestedprojection, full_year)

  percentage_diff_full <- tibble(
    Nutrient = nut_cols,
    unharvested_value = unharvested_nut_full[nut_cols],
    harvested_value = harvested_nut_full[nut_cols]
  ) |>
    dplyr::mutate(
      percentage_diff = ifelse(unharvested_value == 0,
                               ifelse(harvested_value == 0, 0, Inf),
                               ((harvested_value - unharvested_value) / unharvested_value) * 100),
      class = "full"
    ) |>
    dplyr::select(Nutrient, percentage_diff, class) |>
    dplyr::filter(!is.na(percentage_diff))

  if (mode == "triple") {
    # Calculate nutrition totals for quarter year
    unharvested_nut_quarter <- calc_nutrition_totals(unharvestedprojection, quarter_year)
    harvested_nut_quarter <- calc_nutrition_totals(harvestedprojection, quarter_year)

    percentage_diff_quarter <- tibble(
      Nutrient = nut_cols,
      unharvested_value = unharvested_nut_quarter[nut_cols],
      harvested_value = harvested_nut_quarter[nut_cols]
    ) |>
      dplyr::mutate(
        percentage_diff = ifelse(unharvested_value == 0,
                                 ifelse(harvested_value == 0, 0, Inf),
                                 ((harvested_value - unharvested_value) / unharvested_value) * 100),
        class = "quarter"
      ) |>
      dplyr::select(Nutrient, percentage_diff, class) |>
      dplyr::filter(!is.na(percentage_diff))

    # Calculate nutrition totals for half year
    unharvested_nut_half <- calc_nutrition_totals(unharvestedprojection, half_year)
    harvested_nut_half <- calc_nutrition_totals(harvestedprojection, half_year)

    percentage_diff_half <- tibble(
      Nutrient = nut_cols,
      unharvested_value = unharvested_nut_half[nut_cols],
      harvested_value = harvested_nut_half[nut_cols]
    ) |>
      dplyr::mutate(
        percentage_diff = ifelse(unharvested_value == 0,
                                 ifelse(harvested_value == 0, 0, Inf),
                                 ((harvested_value - unharvested_value) / unharvested_value) * 100),
        class = "half"
      ) |>
      dplyr::select(Nutrient, percentage_diff, class) |>
      dplyr::filter(!is.na(percentage_diff))

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

#' Plot nutrition change at selected times
#'
#' Renders a bar chart of nutrition percentage change at selected times for
#' a single harvested vs unharvested comparison.
#'
#' @inheritParams process_nutrition_change
#' @return A ggplot object
#' @keywords internal
plotNutritionChange <- function(harvestedprojection, unharvestedprojection, chosenyear,
                                 mode = c("triple", "chosen")) {
  mode <- match.arg(mode)
  percentage_diff <- process_nutrition_change(harvestedprojection, unharvestedprojection, chosenyear, mode)
  percentage_diff$Percentage <- percentage_diff$percentage_diff
  percentage_diff$Class      <- percentage_diff$fill_group

  ggplot2::ggplot(percentage_diff, ggplot2::aes(x = Nutrient, y = Percentage, fill = Class)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed", linewidth = 0.5) +
    ggplot2::labs(x = "Nutrient", y = "Nutrition % Change") +
    ggplot2::scale_fill_manual(values = c(
      "Quarter, Negative" = "#F2A488",
      "Quarter, Positive" = "#2FA4E799",
      "Half, Negative"    = "#E98C6B",
      "Half, Positive"    = "#2FA4E7cc",
      "Full, Negative"    = "#E76F51",
      "Full, Positive"    = "#2FA4E7"
    )) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x   = ggplot2::element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
      axis.text.y   = ggplot2::element_text(size = 14),
      legend.position = "none",
      axis.title.x  = ggplot2::element_text(size = 16),
      axis.title.y  = ggplot2::element_text(size = 16)
    )
}

#' Plot nutrition change for two simulations
#'
#' Uses shared processing to compute percentage nutrition changes for each nutrient
#' at selected times, and renders separate facets for two simulations.
#'
#' @param harvestedprojection1 First harvested mizer projection
#' @param harvestedprojection2 Second harvested mizer projection
#' @param unharvestedprojection Baseline unharvested mizer projection
#' @param chosenyear Integer defining full period; quarter/half derived
#' @param mode Either "triple" or "chosen"
#' @return A ggplot object
#' @keywords internal
plotNutritionChange2 <- function(harvestedprojection1, harvestedprojection2,
                                  unharvestedprojection, chosenyear, mode = c("triple", "chosen")) {
  mode <- match.arg(mode)
  df1 <- process_nutrition_change(harvestedprojection1, unharvestedprojection, chosenyear, mode)
  df2 <- process_nutrition_change(harvestedprojection2, unharvestedprojection, chosenyear, mode)

  df1$sim <- "Sim 1"
  df2$sim <- "Sim 2"
  plot_df <- dplyr::bind_rows(df1, df2)

  ggplot2::ggplot(plot_df, ggplot2::aes(x = Nutrient, y = percentage_diff, fill = fill_group)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed", linewidth = 0.5) +
    ggplot2::labs(x = "Nutrient", y = "Nutrition % Change") +
    ggplot2::scale_fill_manual(values = c(
      "Quarter, Negative"  = "#F2A488",
      "Quarter, Positive"  = "#2FA4E799",
      "Half, Negative"     = "#E98C6B",
      "Half, Positive"     = "#2FA4E7cc",
      "Full, Negative"     = "#E76F51",
      "Full, Positive"     = "#2FA4E7"
    )) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 14),
                   legend.position = "none",
                   axis.title.x = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 16),
                   panel.spacing.y = grid::unit(2, "lines")) +
    ggplot2::facet_wrap(~ sim, nrow = 2)
}

#' Plot relative nutritional output compared to a reference simulation
#'
#' Aggregates nutrient yields from simulations at a given time `step` and
#' compares each to a reference, returning a grouped bar plot of percentage
#' differences per nutrient.
#'
#' @param sims List of mizer projection objects to compare
#' @param ref Reference mizer projection
#' @param step Integer time index to evaluate
#' @return A ggplot object
#' @keywords internal
plotNutrition <- function(sims, ref, step) {
  nut_cols <- setdiff(names(nut), "species")

  tot_nuts <- function(sim) {
    calc_nutrition_totals(sim, step)
  }

  ref_tot  <- tot_nuts(ref)[nut_cols]
  sim_tots  <- lapply(sims, tot_nuts)

  rel_list <- lapply(seq_along(sim_tots), function(i) {
    tibble(
      Nutrient = nut_cols,
      Relative = sim_tots[[i]][nut_cols] / ref_tot,
      Sim      = paste0("Sim ", i)
    )
  })
  plot_dat <- dplyr::bind_rows(rel_list)|>
    dplyr::filter(!is.na(Relative)) |>
    dplyr::mutate(Value = (Relative - 1) * 100)

  dodge <- if (length(sims) == 2) ggplot2::position_dodge(width = 0.7) else "identity"
  ggplot2::ggplot(plot_dat, ggplot2::aes(Nutrient, Value, fill = Sim)) +
    ggplot2::geom_col(position = dodge, width = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(y = "Relative value (%)", x = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

# Data loading helpers (on package load, mirror previous behavior)
#' Locate app files from installed package or development tree
#'
#' @param ... Path components under the app directory
#' @return A character path, preferring installed package, falling back to `inst/app`
#' @keywords internal
app_path <- function(...) {
  p <- system.file("app", ..., package = "mizerShiny")
  if (p == "") p <- file.path("inst", "app", ...)
  p
}

#' Initialize nutrition data on package load
#'
#' Populates internal datasets `nut` and `all_matches` used by nutrition plots.
#' Not part of the public API.
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  nutrition_file <- app_path("Including", "Nutrition", "checkNutrition", "nutrition.csv")
  if (file.exists(nutrition_file)) {
    nut <<- withCallingHandlers(
      readr::read_csv(
        nutrition_file,
        locale        = readr::locale(encoding = "ISO-8859-1"),
        show_col_types = FALSE
      ),
      message = function(m) {
        if (grepl("New names:", m$message)) invokeRestart("muffleMessage")
      }
    ) |>
      dplyr::select(common_name, dplyr::matches("\\(")) |>
      dplyr::rename_with(~ toupper(gsub("\\s*\\(.*\\)", "", .x)), dplyr::everything()) |>
      dplyr::rename(species = COMMON_NAME)
  }

  match_dir  <- app_path("Including", "Nutrition", "nutritionMatch")
  match_file <- list.files(match_dir, pattern = "\\.RData$", full.names = TRUE)
  if (length(match_file) == 1L) {
    tmp <- new.env()
    obj <- load(match_file, envir = tmp)[1]
    all_matches <<- tmp[[obj]]
  }
}


