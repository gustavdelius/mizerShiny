library(mizer)
test_that("plotbothbiomass works with default_sim", {
  params <- default_sim@params
  sim <- default_sim
  sim2 <- project(params, t_max = 10, effort = 1)

  specie <- NULL
  start_time <- 1
  end_time <- 10
  start_time2 <- 1
  end_time2 <- 10

  p <- mizerShiny:::plotbothbiomass(
    sim, sim2, specie, start_time, end_time, start_time2, end_time2
  )

  expect_s3_class(p, "ggplot")
})

test_that("plotbothbiomass works with specific species", {
  params <- default_sim@params
  species_names <- params@species_params$species[params@species_params$species != "Resource"]

  sim <- default_sim
  sim2 <- mizer::project(params, t_max = 10, effort = 1)

  specie <- species_names[1]
  start_time <- 1
  end_time <- 10
  start_time2 <- 1
  end_time2 <- 10

  p <- mizerShiny:::plotbothbiomass(
    sim, sim2, specie, start_time, end_time, start_time2, end_time2
  )

  expect_s3_class(p, "ggplot")
})

test_that("plotbothbiomass snapshot test", {
  params <- default_sim@params
  sim <- default_sim
  sim2 <- mizer::project(params, t_max = 10, effort = 1)

  p <- mizerShiny:::plotbothbiomass(
    sim, sim2, specie = NULL, start_time = 1, end_time = 10,
    start_time2 = 1, end_time2 = 10
  )

  expect_doppelganger("plotbothbiomass", p)
})

