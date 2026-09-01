test_that("compute_ordered_species returns custom when provided", {
  dp <- default_params
  ord <- mizerShiny:::compute_ordered_species(dp, choice = "Custom",
                                              custom_order = c("Dab", "Plaice"))
  expect_equal(ord, c("Dab", "Plaice"))
})

test_that("compute_ordered_species size ordering excludes Resource", {
  dp <- default_params
  ord <- mizerShiny:::compute_ordered_species(dp, choice = "Size")
  expect_equal(ord, c("Horse mackerel", "Blue whiting", "Megrim", "Herring",
                      "Sole", "Whiting", "Mackerel", "Plaice", "Haddock",
                      "Monkfish", "Cod", "Hake"))
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

