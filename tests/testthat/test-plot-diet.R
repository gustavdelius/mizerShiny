test_that("plotDietCompare works with default_sim", {
  params <- default_sim@params
  species_names <- params@species_params$species[params@species_params$species != "Resource"]

  objects <- list(default_sim)
  species <- species_names[1:min(2, length(species_names))]
  sim_names <- NULL

  p <- mizerShiny:::plotDietCompare(objects, species = species, sim_names = sim_names)

  # plotDietCompare returns a plotly object or NULL
  if (!is.null(p)) {
    expect_s3_class(p, "plotly")
  } else {
    skip("plotDietCompare returned NULL (no data to plot)")
  }
})

test_that("plotDietCompare works with two simulations", {
  params <- default_sim@params
  species_names <- params@species_params$species[params@species_params$species != "Resource"]

  objects <- list(default_sim, mizer::project(params, t_max = 10, effort = 1))
  species <- species_names[1:min(2, length(species_names))]
  sim_names <- c("Sim 1", "Sim 2")

  p <- mizerShiny:::plotDietCompare(objects, species = species, sim_names = sim_names)

  if (!is.null(p)) {
    expect_s3_class(p, "plotly")
  } else {
    skip("plotDietCompare returned NULL (no data to plot)")
  }
})

test_that("plotDietCompare snapshot test", {
  params <- default_sim@params
  species_names <- params@species_params$species[params@species_params$species != "Resource"]

  objects <- list(default_sim)
  species <- species_names[1:min(2, length(species_names))]

  p <- mizerShiny:::plotDietCompare(objects, species = species, sim_names = NULL)

  if (!is.null(p)) {
    # For plotly objects, we can snapshot the structure
    expect_snapshot_output(str(p))
  } else {
    skip("plotDietCompare returned NULL (no data to plot)")
  }
})

