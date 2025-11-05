test_that("plotSpeciesWithTimeRange2 works with default_sim", {
  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "chosen"

  p <- mizerShiny:::plotSpeciesWithTimeRange2(
    harvested1, harvested2, unharvested, chosenyear, mode = mode
  )

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesWithTimeRange2 works with triple mode", {
  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "triple"

  p <- mizerShiny:::plotSpeciesWithTimeRange2(
    harvested1, harvested2, unharvested, chosenyear, mode = mode
  )

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesWithTimeRange2 snapshot test", {
  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  p <- mizerShiny:::plotSpeciesWithTimeRange2(
    harvested1, harvested2, unharvested, chosenyear = 10, mode = "chosen"
  )

  expect_snapshot_output(print(p))
})

test_that("plotSpeciesActualBiomass2 works with default_sim", {
  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)

  chosenyear <- 10
  mode <- "chosen"

  p <- mizerShiny:::plotSpeciesActualBiomass2(harvested1, harvested2, chosenyear, mode = mode)

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesActualBiomass2 works with triple mode", {
  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)

  chosenyear <- 10
  mode <- "triple"

  p <- mizerShiny:::plotSpeciesActualBiomass2(harvested1, harvested2, chosenyear, mode = mode)

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesActualBiomass2 snapshot test", {
  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)

  p <- mizerShiny:::plotSpeciesActualBiomass2(harvested1, harvested2, chosenyear = 10, mode = "chosen")

  expect_snapshot_output(print(p))
})

test_that("plotSpeciesActualYield2 works with default_sim", {
  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)

  chosenyear <- 10
  mode <- "chosen"

  p <- mizerShiny:::plotSpeciesActualYield2(harvested1, harvested2, chosenyear, mode = mode)

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesActualYield2 works with triple mode", {
  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)

  chosenyear <- 10
  mode <- "triple"

  p <- mizerShiny:::plotSpeciesActualYield2(harvested1, harvested2, chosenyear, mode = mode)

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesActualYield2 snapshot test", {
  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)

  p <- mizerShiny:::plotSpeciesActualYield2(harvested1, harvested2, chosenyear = 10, mode = "chosen")

  expect_snapshot_output(print(p))
})

test_that("plotSpeciesYieldChange2 works with default_sim", {
  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "chosen"

  p <- mizerShiny:::plotSpeciesYieldChange2(
    harvested1, harvested2, unharvested, chosenyear, mode = mode
  )

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesYieldChange2 works with triple mode", {
  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "triple"

  p <- mizerShiny:::plotSpeciesYieldChange2(
    harvested1, harvested2, unharvested, chosenyear, mode = mode
  )

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesYieldChange2 snapshot test", {
  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  p <- mizerShiny:::plotSpeciesYieldChange2(
    harvested1, harvested2, unharvested, chosenyear = 10, mode = "chosen"
  )

  expect_snapshot_output(print(p))
})

test_that("guildplot_both works with default_sim", {
  # Create mock guildparams
  params <- default_sim@params
  species_names <- params@species_params$species[params@species_params$species != "Resource"]
  guildparams <- data.frame(
    Species = rep(species_names[1:min(3, length(species_names))], each = 2),
    minw = c(0.1, 10),
    maxw = c(10, 100),
    Feeding.guild = c("Plank", "Piscivore")
  )

  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "chosen"
  celticsim <- NULL

  p <- mizerShiny:::guildplot_both(
    harvested1, harvested2, unharvested, chosenyear, guildparams, celticsim, mode = mode
  )

  expect_s3_class(p, "ggplot")
})

test_that("guildplot_both works with triple mode", {
  params <- default_sim@params
  species_names <- params@species_params$species[params@species_params$species != "Resource"]
  guildparams <- data.frame(
    Species = rep(species_names[1:min(3, length(species_names))], each = 2),
    minw = c(0.1, 10),
    maxw = c(10, 100),
    Feeding.guild = c("Plank", "Piscivore")
  )

  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "triple"
  celticsim <- NULL

  p <- mizerShiny:::guildplot_both(
    harvested1, harvested2, unharvested, chosenyear, guildparams, celticsim, mode = mode
  )

  expect_s3_class(p, "ggplot")
})

test_that("guildplot_both snapshot test", {
  params <- default_sim@params
  species_names <- params@species_params$species[params@species_params$species != "Resource"]
  guildparams <- data.frame(
    Species = rep(species_names[1:min(3, length(species_names))], each = 2),
    minw = c(0.1, 10),
    maxw = c(10, 100),
    Feeding.guild = c("Plank", "Piscivore")
  )

  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  p <- mizerShiny:::guildplot_both(
    harvested1, harvested2, unharvested, chosenyear = 10, guildparams, NULL, mode = "chosen"
  )

  expect_snapshot_output(print(p))
})

