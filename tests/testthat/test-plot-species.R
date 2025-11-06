test_that("plotSpeciesWithTimeRange works with default_sim", {
  # Create test projections
  params <- default_sim@params
  harvested <- default_sim
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "chosen"

  # Test basic functionality
  p <- mizerShiny:::plotSpeciesWithTimeRange(harvested, unharvested, chosenyear, mode = mode)

  expect_s3_class(p, "ggplot")
  expect_true(all(c("x", "y") %in% names(p$mapping)))
})

test_that("plotSpeciesWithTimeRange works with triple mode", {
  params <- default_sim@params
  harvested <- default_sim
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "triple"

  p <- mizerShiny:::plotSpeciesWithTimeRange(harvested, unharvested, chosenyear, mode = mode)

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesWithTimeRange snapshot test", {
  params <- default_sim@params
  harvested <- default_sim
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  p <- mizerShiny:::plotSpeciesWithTimeRange(harvested, unharvested, chosenyear = 10, mode = "chosen")

  expect_doppelganger("plotSpeciesWithTimeRange", p)
})

test_that("plotSpeciesActualBiomass works with default_sim", {
  harvested <- default_sim
  chosenyear <- 10
  mode <- "chosen"

  p <- mizerShiny:::plotSpeciesActualBiomass(harvested, chosenyear, mode = mode)

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesActualBiomass works with triple mode", {
  harvested <- default_sim
  chosenyear <- 10
  mode <- "triple"

  p <- mizerShiny:::plotSpeciesActualBiomass(harvested, chosenyear, mode = mode)

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesActualBiomass snapshot test", {
  harvested <- default_sim
  p <- mizerShiny:::plotSpeciesActualBiomass(harvested, chosenyear = 10, mode = "chosen")

  expect_doppelganger("plotSpeciesActualBiomass", p)
})

test_that("plotSpeciesActualYield works with default_sim", {
  harvested <- default_sim
  chosenyear <- 10
  mode <- "chosen"

  p <- mizerShiny:::plotSpeciesActualYield(harvested, chosenyear, mode = mode)

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesActualYield works with triple mode", {
  harvested <- default_sim
  chosenyear <- 10
  mode <- "triple"

  p <- mizerShiny:::plotSpeciesActualYield(harvested, chosenyear, mode = mode)

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesActualYield snapshot test", {
  harvested <- default_sim
  p <- mizerShiny:::plotSpeciesActualYield(harvested, chosenyear = 10, mode = "chosen")

  expect_doppelganger("plotSpeciesActualYield", p)
})

test_that("plotSpeciesYieldChange works with default_sim", {
  params <- default_sim@params
  harvested <- default_sim
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "chosen"

  p <- mizerShiny:::plotSpeciesYieldChange(harvested, unharvested, chosenyear, mode = mode)

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesYieldChange works with triple mode", {
  params <- default_sim@params
  harvested <- default_sim
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "triple"

  p <- mizerShiny:::plotSpeciesYieldChange(harvested, unharvested, chosenyear, mode = mode)

  expect_s3_class(p, "ggplot")
})

test_that("plotSpeciesYieldChange snapshot test", {
  params <- default_sim@params
  harvested <- default_sim
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  p <- mizerShiny:::plotSpeciesYieldChange(harvested, unharvested, chosenyear = 10, mode = "chosen")

  expect_doppelganger("plotSpeciesYieldChange", p)
})

