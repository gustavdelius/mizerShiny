test_that("compute_ordered_species returns custom when provided", {
  dp <- structure(list(species_params = data.frame(species = c("A","B","Resource"),
                                                   w_mat = c(2,1,0))), class = "MizerParams")
  ord <- mizerShiny:::compute_ordered_species(dp, choice = "Custom", custom_order = c("B","A"))
  expect_equal(ord, c("B","A"))
})

test_that("compute_ordered_species size ordering excludes Resource", {
  dp <- structure(list(species_params = data.frame(species = c("A","B","Resource"),
                                                   w_mat = c(2,1,0))), class = "MizerParams")
  ord <- mizerShiny:::compute_ordered_species(dp, choice = "Size")
  expect_equal(ord, c("B","A"))
})

test_that("compute_ordered_species guild ordering intersects species", {
  dp <- structure(list(species_params = data.frame(species = c("Cod","Haddock","Resource"))), class = "MizerParams")
  guildparams <- data.frame(Species = c("Cod","Haddock","Other"),
                            Feeding.guild = c("G1","G2","G0"),
                            maxw = c(10, 20, 5))
  ord <- mizerShiny:::compute_ordered_species(dp, guildparams = guildparams, choice = "Guild")
  expect_true(all(ord %in% c("Cod","Haddock")))
  expect_false("Resource" %in% ord)
})


