test_that("compute_ordered_species respects choice and excludes Resource", {
  methods::setClass("FakeParams", slots = list(species_params = "data.frame"))
  params <- methods::new("FakeParams", species_params = data.frame(species = c("A","Resource","B"), w_mat = c(5, NA, 2)))

  # Default/custom fallback
  expect_equal(compute_ordered_species(params, choice = "Custom", custom_order = c("B","A")), c("B","A"))

  # Size ordering
  expect_equal(compute_ordered_species(params, choice = "Size"), c("B","A"))

  # Guild fallback when no guildparams
  expect_equal(sort(compute_ordered_species(params, choice = "Guild")), c("A","B"))

  # Guild with rules
  guildparams <- data.frame(Species = c("A","B"), Feeding.guild = c("G1","G2"), maxw = c(10, 20))
  expect_equal(compute_ordered_species(params, guildparams = guildparams, choice = "Guild"), c("A","B"))
})


