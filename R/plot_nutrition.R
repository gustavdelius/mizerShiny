# Nutrition plot helper and data loading

plotNutrition <- function(sims, ref, step) {
  nut_cols <- setdiff(names(nut), "species")

  tot_nuts <- function(sim) {
    y <- getYield(sim)
    yield_df <- tibble(
      fmp   = colnames(y),
      Yield = colMeans((y[step, , drop = FALSE]))
    )
    nut_cols <- setdiff(names(nut), "species")
    yield_df |>
      dplyr::left_join(all_matches,  by = "fmp") |>
      dplyr::left_join(nut,           by = c(nut_match = "species")) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(nut_cols), ~ .x * Yield)) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(nut_cols), ~ sum(.x, na.rm = TRUE)))|>
      unlist(use.names = TRUE)
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
app_path <- function(...) {
  p <- system.file("app", ..., package = "mizerShiny")
  if (p == "") p <- file.path("inst", "app", ...)
  p
}

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


