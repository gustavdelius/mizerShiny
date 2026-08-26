test_that("plotYieldVsSize works with a single sim (ggplot)", {
  sim <- default_sim
  p <- mizerShiny:::plotYieldVsSize(sim, x_var = "Weight", return_data = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plotYieldVsSize returns expected columns with return_data", {
  sim <- default_sim

  df_w <- mizerShiny:::plotYieldVsSize(sim, x_var = "Weight", return_data = TRUE)
  expect_true(all(c("w", "Catch density", "Strategy", "Species") %in% names(df_w)))

  df_l <- mizerShiny:::plotYieldVsSize(sim, x_var = "Length", return_data = TRUE)
  expect_true(all(c("l", "Catch density", "Strategy", "Species") %in% names(df_l)))
})

test_that("plotYieldVsSize normalizes additional sims using baseline totals", {
  # Unnamed list uses default labels: first is "Current", second becomes "Strategy 2"
  sims <- list(default_sim, default_sim)
  df <- mizerShiny:::plotYieldVsSize(sims, x_var = "Weight", return_data = TRUE)

  # Helper to compute area under the curve for each species using baseline dw
  params <- mizer::finalParams(sims[[1]])
  params <- mizer::set_species_param_default(params, "a", 0.006)
  params <- mizer::set_species_param_default(params, "b", 3)

  species_names <- unique(as.character(df$Species))

  # For each species, if baseline total > 0 then area ~ 1; else area should be 0 for all sims
  for (s in species_names) {
    iSpecies <- which(mizer::species_params(params)$species == s)
    if (length(iSpecies) == 0) next

    w_min_idx <- sum(mizer::w(params) < (mizer::species_params(params)$w_mat[[iSpecies]] / 100))
    w_max_idx <- sum(mizer::w(params) <= mizer::species_params(params)$w_max[[iSpecies]])
    w_sel <- seq(w_min_idx, w_max_idx, by = 1)

    # If there are no rows for this species, skip
    if (!any(df$Species == s)) next

    # Subset one sim to get the sequence of w to align dw accordingly
    sub_current <- df[df$Species == s & df$Strategy == "Strategy 1", , drop = FALSE]
    if (nrow(sub_current) == 0) next
    # Find matching indices in w(params) for these w values
    w_index <- match(sub_current$w, mizer::w(params))
    # baseline dw for integration aligned to returned data
    dw_sel <- mizer::dw(params)[w_index]

    f_mort_b <- mizer::getFMort(params)[iSpecies, w_index]
    catch_w_b <- f_mort_b * mizer::initialN(params)[iSpecies, w_index]
    baseline_total <- sum(catch_w_b * mizer::dw(params)[w_index])

    for (sim_label in c("Strategy 1", "Strategy 2")) {
      sub <- df[df$Species == s & df$Strategy == sim_label, , drop = FALSE]
      # Ensure matching order by increasing w
      sub <- sub[order(sub$w), ]
      if (baseline_total > 0) {
        expect_equal(sum(sub$`Catch density` * dw_sel), 1, tolerance = 1e-6)
      } else {
        expect_equal(sum(sub$`Catch density` * dw_sel), 0, tolerance = 1e-12)
      }
    }
  }
})

test_that("plotYieldVsSize respects provided sim names", {
  sims <- list(baseline = default_sim, changed = default_sim)
  df <- mizerShiny:::plotYieldVsSize(sims, x_var = "Weight", return_data = TRUE)
  expect_true(all(c("baseline", "changed") %in% unique(df$Strategy)))
})


