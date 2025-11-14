# Nutrition plot helper and data loading

#' Calculate total nutrients from a simulation at given time steps
#'
#' Helper function to compute nutrient totals from yield data.
#'
#' @param nutrition Data frame of per-species nutrient contents
#' @param sim Mizer projection object
#' @param steps Integer time indices to evaluate (can be a range)
#' @return Named vector of nutrient totals
#' @keywords internal
#'
# Alter nutrient column names to be more readable and drop unwanted nutrients in csv (e.g., due to 0 content in assemblage - change if required)
normalise_nutrition_cols <- function(nutrition) {
    upper_names <- toupper(names(nutrition))

    # Columns we want to drop entirely (e.g. all-zero Vitamin D2)
    drop_targets <- c("D2_ERGCAL", "D2 ERGCAL", "D2 ERGCAL(MCG)")
    keep_idx <- !upper_names %in% toupper(drop_targets)
    nutrition <- nutrition[, keep_idx, drop = FALSE]
    upper_names <- toupper(names(nutrition))  # refresh after dropping

    # Map from UPPERCASE column names to nice labels
    name_map <- c(
        "SELENIUM"    = "Selenium",
        "ZINC"        = "Zinc",
        "OMEGA_3"     = "Omega-3",
        "CALCIUM"     = "Calcium",
        "IRON"        = "Iron",
        "VITAMIN_A"   = "Vitamin A",
        "VITAMIN A"   = "Vitamin A",
        "VITAMIN_D"   = "Vitamin D",
        "VITAMIN D"   = "Vitamin D",
        "D3_CHOCAL"   = "Vitamin D3",
        "D3 CHOCAL"   = "Vitamin D3",
        "FOLATE"      = "Folate",
        "VITAMIN_B12" = "Vitamin B12",
        "VITAMIN B12" = "Vitamin B12",
        "IODINE"      = "Iodine"
    )

    # Rename any columns that appear in the map (leave "species" alone)
    for (i in seq_along(upper_names)) {
        u <- upper_names[i]
        if (!identical(names(nutrition)[i], "species") && u %in% names(name_map)) {
            names(nutrition)[i] <- name_map[[u]]
        }
    }

    nutrition
}



calc_nutrition_totals <- function(nutrition, sim, steps, return_yield = FALSE) {
    nut_cols <- setdiff(names(nutrition), "species")
    y <- getYield(sim)

    yield_df <- tibble(
        species = colnames(y),
        Yield   = colMeans(y[steps, , drop = FALSE])
    )

    joined <- yield_df |>
        dplyr::left_join(nutrition, by = "species")

    nut_vec <- joined |>
        dplyr::mutate(dplyr::across(dplyr::all_of(nut_cols), ~ .x * Yield)) |>
        dplyr::summarise(dplyr::across(dplyr::all_of(nut_cols), ~ sum(.x, na.rm = TRUE))) |>
        unlist(use.names = TRUE)

    if (isTRUE(return_yield)) {
        total_yield <- sum(joined$Yield, na.rm = TRUE)
        return(list(nutrients = nut_vec, total_yield = total_yield))
    } else {
        return(nut_vec)
    }
}

#' Shared processing for nutrition change plots
#'
#' Computes percentage nutrition differences per nutrient between harvested and
#' unharvested projections at one or three time ranges.
#'
#' @param nutrition Data frame of per-species nutrient contents
#' @param harvestedprojection Harvested mizer projection
#' @param sim_0 Unharvested mizer projection
#' @param chosenyear Integer defining the full period; quarter/half derived
#' @param mode Either "triple" or "chosen"
#' @return A data frame with Nutrient, percentage_diff and class/fill_group columns
#' @keywords internal
process_nutrition_change <- function(nutrition,
                                     harvestedprojection, sim_0,
                                     chosenyear,
                                     mode      = c("triple", "chosen"),
                                     normalise = c("total", "per_yield")) {
    mode      <- match.arg(mode)
    normalise <- match.arg(normalise)

    nutrition <- normalise_nutrition_cols(nutrition)
    nut_cols  <- setdiff(names(nutrition), "species")

    quarter_year <- max(1, ceiling(chosenyear * 0.25))
    half_year    <- max(1, ceiling(chosenyear * 0.5))
    full_year    <- chosenyear

    ## ---- FULL YEAR ----
    if (normalise == "total") {
        unharvested_nut_full <- calc_nutrition_totals(nutrition, sim_0, full_year)
        harvested_nut_full   <- calc_nutrition_totals(nutrition, harvestedprojection, full_year)
    } else {
        u_full <- calc_nutrition_totals(nutrition, sim_0, full_year, return_yield = TRUE)
        h_full <- calc_nutrition_totals(nutrition, harvestedprojection, full_year, return_yield = TRUE)

        unharvested_nut_full <- u_full$nutrients / u_full$total_yield
        harvested_nut_full   <- h_full$nutrients / h_full$total_yield
    }

    percentage_diff_full <- tibble(
        Nutrient          = nut_cols,
        unharvested_value = unharvested_nut_full[nut_cols],
        harvested_value   = harvested_nut_full[nut_cols]
    ) |>
        dplyr::mutate(
            percentage_diff = ifelse(
                unharvested_value == 0,
                ifelse(harvested_value == 0, 0, Inf),
                ((harvested_value - unharvested_value) / unharvested_value) * 100
            ),
            class = "full"
        ) |>
        dplyr::select(Nutrient, percentage_diff, class) |>
        dplyr::filter(!is.na(percentage_diff))

    ## ---- QUARTER & HALF (if triple mode) ----
    if (mode == "triple") {

        # Quarter year
        if (normalise == "total") {
            unharvested_nut_quarter <- calc_nutrition_totals(nutrition, sim_0, quarter_year)
            harvested_nut_quarter   <- calc_nutrition_totals(nutrition, harvestedprojection, quarter_year)
        } else {
            u_quarter <- calc_nutrition_totals(nutrition, sim_0, quarter_year, return_yield = TRUE)
            h_quarter <- calc_nutrition_totals(nutrition, harvestedprojection, quarter_year, return_yield = TRUE)

            unharvested_nut_quarter <- u_quarter$nutrients / u_quarter$total_yield
            harvested_nut_quarter   <- h_quarter$nutrients / h_quarter$total_yield
        }

        percentage_diff_quarter <- tibble(
            Nutrient          = nut_cols,
            unharvested_value = unharvested_nut_quarter[nut_cols],
            harvested_value   = harvested_nut_quarter[nut_cols]
        ) |>
            dplyr::mutate(
                percentage_diff = ifelse(
                    unharvested_value == 0,
                    ifelse(harvested_value == 0, 0, Inf),
                    ((harvested_value - unharvested_value) / unharvested_value) * 100
                ),
                class = "quarter"
            ) |>
            dplyr::select(Nutrient, percentage_diff, class) |>
            dplyr::filter(!is.na(percentage_diff))

        # Half year
        if (normalise == "total") {
            unharvested_nut_half <- calc_nutrition_totals(nutrition, sim_0, half_year)
            harvested_nut_half   <- calc_nutrition_totals(nutrition, harvestedprojection, half_year)
        } else {
            u_half <- calc_nutrition_totals(nutrition, sim_0, half_year, return_yield = TRUE)
            h_half <- calc_nutrition_totals(nutrition, harvestedprojection, half_year, return_yield = TRUE)

            unharvested_nut_half <- u_half$nutrients / u_half$total_yield
            harvested_nut_half   <- h_half$nutrients / h_half$total_yield
        }

        percentage_diff_half <- tibble(
            Nutrient          = nut_cols,
            unharvested_value = unharvested_nut_half[nut_cols],
            harvested_value   = harvested_nut_half[nut_cols]
        ) |>
            dplyr::mutate(
                percentage_diff = ifelse(
                    unharvested_value == 0,
                    ifelse(harvested_value == 0, 0, Inf),
                    ((harvested_value - unharvested_value) / unharvested_value) * 100
                ),
                class = "half"
            ) |>
            dplyr::select(Nutrient, percentage_diff, class) |>
            dplyr::filter(!is.na(percentage_diff))

        # Combine all three time slices
        plot_data <- dplyr::bind_rows(
            percentage_diff_quarter,
            percentage_diff_half,
            percentage_diff_full
        )

    } else {
        # Only full-year slice
        plot_data <- percentage_diff_full
    }

    # Common factor / fill handling
    plot_data$class <- factor(plot_data$class, levels = c("quarter", "half", "full"))
    plot_data$fill_group <- interaction(plot_data$percentage_diff >= 0, plot_data$class)
    plot_data$fill_group <- factor(
        plot_data$fill_group,
        levels = c("FALSE.quarter", "TRUE.quarter",
                   "FALSE.half",    "TRUE.half",
                   "FALSE.full",    "TRUE.full"),
        labels = c("Quarter, Negative", "Quarter, Positive",
                   "Half, Negative",    "Half, Positive",
                   "Full, Negative",    "Full, Positive")
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
    plotNutritionChange <- function(nutrition, harvestedprojection,
                                    sim_0, chosenyear,
                                    mode      = c("triple", "chosen"),
                                    normalise = c("total", "per_yield")) {
        mode      <- match.arg(mode)
        normalise <- match.arg(normalise)

        percentage_diff <- process_nutrition_change(
            nutrition,
            harvestedprojection, sim_0,
            chosenyear,
            mode      = mode,
            normalise = normalise
        )

        percentage_diff$Percentage <- percentage_diff$percentage_diff
        percentage_diff$Class      <- percentage_diff$fill_group

        y_lab <- if (normalise == "total") {
            "Nutrition % Change (total landed)"
        } else {
            "Nutrition % Change (per tonne of yield)"
        }

        ggplot2::ggplot(percentage_diff,
                        ggplot2::aes(x = Nutrient, y = Percentage, fill = Class)) +
            ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9)) +
            ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed", linewidth = 0.5) +
            ggplot2::labs(x = "Nutrient", y = y_lab) +
            ggplot2::scale_fill_manual(values = change_colours()) +
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
#' @param nutrition Data frame of per-species nutrient contents
#' @param harvestedprojection1 First harvested mizer projection
#' @param harvestedprojection2 Second harvested mizer projection
#' @param sim_0 Baseline unharvested mizer projection
#' @param chosenyear Integer defining full period; quarter/half derived
#' @param mode Either "triple" or "chosen"
#' @return A ggplot object
#' @keywords internal
    plotNutritionChange2 <- function(nutrition,
                                     harvestedprojection1, harvestedprojection2,
                                     sim_0, chosenyear,
                                     mode      = c("triple", "chosen"),
                                     normalise = c("total", "per_yield")) {
        mode      <- match.arg(mode)
        normalise <- match.arg(normalise)

        df1 <- process_nutrition_change(
            nutrition,
            harvestedprojection1, sim_0,
            chosenyear,
            mode      = mode,
            normalise = normalise
        )
        df2 <- process_nutrition_change(
            nutrition,
            harvestedprojection2, sim_0,
            chosenyear,
            mode      = mode,
            normalise = normalise
        )

        df1$sim <- "Strategy 1"
        df2$sim <- "Strategy 2"
        plot_df <- dplyr::bind_rows(df1, df2)

        y_lab <- if (normalise == "total") {
            "Nutrition % Change (total landed)"
        } else {
            "Nutrition % Change (per tonne of yield)"
        }

        ggplot2::ggplot(plot_df,
                        ggplot2::aes(x = Nutrient, y = percentage_diff, fill = fill_group)) +
            ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9)) +
            ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed", linewidth = 0.5) +
            ggplot2::labs(x = "Nutrient", y = y_lab) +
            ggplot2::scale_fill_manual(values = change_colours()) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
                axis.text.y = ggplot2::element_text(size = 14),
                legend.position = "none",
                axis.title.x = ggplot2::element_text(size = 16),
                axis.title.y = ggplot2::element_text(size = 16),
                panel.spacing.y = grid::unit(2, "lines")
            ) +
            ggplot2::facet_wrap(~ sim, nrow = 2)
    }


#' Plot relative nutritional output compared to a reference simulation
#'
#' Aggregates nutrient yields from simulations at a given time `step` and
#' compares each to a reference, returning a grouped bar plot of percentage
#' differences per nutrient.
#'
#' @param nutrition Data frame of per-species nutrient contents
#' @param sims List of mizer projection objects to compare
#' @param ref Reference mizer projection
#' @param step Integer time index to evaluate
#' @return A ggplot object
#' @keywords internal
plotNutrition <- function(nutrition, sims, ref, step) {
    # Normalise labels and drop unused nutrients (e.g. Vitamin D2)
    nutrition <- normalise_nutrition_cols(nutrition)
    nut_cols  <- setdiff(names(nutrition), "species")

    tot_nuts <- function(sim) {
        calc_nutrition_totals(nutrition, sim, step)
    }

  ref_tot  <- tot_nuts(ref)[nut_cols]
  sim_tots  <- lapply(sims, tot_nuts)

  rel_list <- lapply(seq_along(sim_tots), function(i) {
    tibble(
      Nutrient = nut_cols,
      Relative = sim_tots[[i]][nut_cols] / ref_tot,
      Strategy      = paste0("Strategy ", i)
    )
  })
  plot_dat <- dplyr::bind_rows(rel_list) |>
    dplyr::filter(!is.na(Relative)) |>
    dplyr::mutate(Value = (Relative - 1) * 100)

  dodge <- if (length(sims) == 2) ggplot2::position_dodge(width = 0.7) else "identity"
  ggplot2::ggplot(plot_dat, ggplot2::aes(Nutrient, Value, fill = Strategy)) +
    ggplot2::geom_col(position = dodge, width = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(y = "Relative value (%)", x = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
