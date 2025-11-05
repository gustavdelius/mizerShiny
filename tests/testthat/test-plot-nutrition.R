test_that("plotNutrition works with default_sim", {
  # Check if nutrition data is available
  if (!exists("nut") || !exists("all_matches")) {
    skip("Nutrition data (nut, all_matches) not available")
  }

  params <- default_sim@params
  ref <- mizer::project(params, t_max = 10, effort = 0)
  sims <- list(default_sim, mizer::project(params, t_max = 10, effort = 1))
  step <- 10

  p <- mizerShiny:::plotNutrition(sims, ref, step)

  expect_s3_class(p, "ggplot")
})

test_that("plotNutrition snapshot test", {
  if (!exists("nut") || !exists("all_matches")) {
    skip("Nutrition data (nut, all_matches) not available")
  }

  params <- default_sim@params
  ref <- mizer::project(params, t_max = 10, effort = 0)
  sims <- list(default_sim)
  step <- 10

  p <- mizerShiny:::plotNutrition(sims, ref, step)

  expect_doppelganger("plotNutrition", p)
})

