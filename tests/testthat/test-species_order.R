test_that("compute_ordered_species respects choice and excludes Resource", {
  params <- default_params
  species <- mizer::species_params(params)$species
  custom_order <- rev(species[1:2])

  # Default/custom fallback
  expect_equal(compute_ordered_species(params, choice = "Custom",
                                       custom_order = custom_order),
               custom_order)

  # Size ordering
  expected_size_order <- mizer::species_params(params) |>
    dplyr::arrange(.data$w_mat) |>
    dplyr::pull(.data$species)
  expect_equal(compute_ordered_species(params, choice = "Size"),
               expected_size_order)

  # Guild fallback when no guildparams
  expect_equal(compute_ordered_species(params, choice = "Guild"), species)

  # Guild with rules
  guildparams <- data.frame(Species = species[1:2],
                            Feeding.guild = c("G1", "G2"),
                            maxw = c(10, 20))
  expect_equal(compute_ordered_species(params, guildparams = guildparams,
                                       choice = "Guild"),
               species[1:2])
})

