test_that("compute_ordered_species returns custom when provided", {
    dp <- default_params
    ord <- mizerShiny:::compute_ordered_species(dp, choice = "Custom",
                                                custom_order = c("Megrim", "Plaice"))
    expect_equal(ord, c("Megrim", "Plaice"))
})

test_that("compute_ordered_species size ordering excludes Resource", {
    dp <- default_params
    ord <- mizerShiny:::compute_ordered_species(dp, choice = "Size")
    expect_equal(ord, c("Horse mackerel", "Blue whiting", "Megrim", "Herring",
                        "Sole", "Whiting", "Mackerel", "Plaice",
                        "Haddock", "Monkfish", "Cod", "Hake"))
})

test_that("compute_ordered_species guild ordering intersects species", {
    dp <- default_params
    guildparams <- data.frame(Species = c("Cod","Haddock","Other"),
                              Feeding.guild = c("G1","G2","G0"),
                              maxw = c(10, 20, 5))
    ord <- mizerShiny:::compute_ordered_species(dp, guildparams = guildparams,
                                                choice = "Guild")
    expect_true(all(ord %in% c("Cod","Haddock")))
    expect_false("Resource" %in% ord)
})

test_that("compute_ordered_species respects choice and excludes Resource", {
  params <- suppressMessages(newMultispeciesParams(data.frame(species = c("A", "B"), w_mat = c(5, 2), w_max = c(10, 10))))

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


