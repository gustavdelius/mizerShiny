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

test_that("calc_nutrition_totals works with default_sim", {
  if (!exists("nut") || !exists("all_matches")) {
    skip("Nutrition data (nut, all_matches) not available")
  }

  params <- default_sim@params
  sim <- default_sim
  steps <- 10

  result <- mizerShiny:::calc_nutrition_totals(sim, steps)

  expect_type(result, "double")
  expect_true(length(result) > 0)
  expect_true(all(!is.na(result)))
})

test_that("calc_nutrition_totals works with multiple steps", {
  if (!exists("nut") || !exists("all_matches")) {
    skip("Nutrition data (nut, all_matches) not available")
  }

  params <- default_sim@params
  sim <- default_sim
  steps <- 5:10

  result <- mizerShiny:::calc_nutrition_totals(sim, steps)

  expect_type(result, "double")
  expect_true(length(result) > 0)
})

test_that("process_nutrition_change works with default_sim", {
  if (!exists("nut") || !exists("all_matches")) {
    skip("Nutrition data (nut, all_matches) not available")
  }

  params <- default_sim@params
  harvested <- default_sim
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "chosen"

  result <- mizerShiny:::process_nutrition_change(harvested, unharvested, chosenyear, mode = mode)

  expect_s3_class(result, "data.frame")
  expect_true("Nutrient" %in% names(result))
  expect_true("percentage_diff" %in% names(result))
  expect_true("class" %in% names(result))
  expect_true("fill_group" %in% names(result))
  expect_true(nrow(result) > 0)
})

test_that("process_nutrition_change works with triple mode", {
  if (!exists("nut") || !exists("all_matches")) {
    skip("Nutrition data (nut, all_matches) not available")
  }

  params <- default_sim@params
  harvested <- default_sim
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "triple"

  result <- mizerShiny:::process_nutrition_change(harvested, unharvested, chosenyear, mode = mode)

  expect_s3_class(result, "data.frame")
  expect_true("Nutrient" %in% names(result))
  expect_true("percentage_diff" %in% names(result))
  expect_true("class" %in% names(result))
  expect_true("fill_group" %in% names(result))
  expect_true(nrow(result) > 0)

  # Check that we have data for all three time periods
  classes <- unique(result$class)
  expect_true("quarter" %in% classes)
  expect_true("half" %in% classes)
  expect_true("full" %in% classes)
})

test_that("plotNutritionChange works with default_sim", {
  if (!exists("nut") || !exists("all_matches")) {
    skip("Nutrition data (nut, all_matches) not available")
  }

  params <- default_sim@params
  harvested <- default_sim
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "chosen"

  p <- mizerShiny:::plotNutritionChange(harvested, unharvested, chosenyear, mode = mode)

  expect_s3_class(p, "ggplot")
})

test_that("plotNutritionChange works with triple mode", {
  if (!exists("nut") || !exists("all_matches")) {
    skip("Nutrition data (nut, all_matches) not available")
  }

  params <- default_sim@params
  harvested <- default_sim
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "triple"

  p <- mizerShiny:::plotNutritionChange(harvested, unharvested, chosenyear, mode = mode)

  expect_s3_class(p, "ggplot")
})

test_that("plotNutritionChange snapshot test", {
  if (!exists("nut") || !exists("all_matches")) {
    skip("Nutrition data (nut, all_matches) not available")
  }

  params <- default_sim@params
  harvested <- default_sim
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  p <- mizerShiny:::plotNutritionChange(harvested, unharvested, chosenyear = 10, mode = "chosen")

  expect_doppelganger("plotNutritionChange", p)
})

test_that("plotNutritionChange2 works with default_sim", {
  if (!exists("nut") || !exists("all_matches")) {
    skip("Nutrition data (nut, all_matches) not available")
  }

  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "chosen"

  p <- mizerShiny:::plotNutritionChange2(
    harvested1, harvested2, unharvested, chosenyear, mode = mode
  )

  expect_s3_class(p, "ggplot")
})

test_that("plotNutritionChange2 works with triple mode", {
  if (!exists("nut") || !exists("all_matches")) {
    skip("Nutrition data (nut, all_matches) not available")
  }

  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  chosenyear <- 10
  mode <- "triple"

  p <- mizerShiny:::plotNutritionChange2(
    harvested1, harvested2, unharvested, chosenyear, mode = mode
  )

  expect_s3_class(p, "ggplot")
})

test_that("plotNutritionChange2 snapshot test", {
  if (!exists("nut") || !exists("all_matches")) {
    skip("Nutrition data (nut, all_matches) not available")
  }

  params <- default_sim@params
  harvested1 <- default_sim
  harvested2 <- mizer::project(params, t_max = 10, effort = 1)
  unharvested <- mizer::project(params, t_max = 10, effort = 0)

  p <- mizerShiny:::plotNutritionChange2(
    harvested1, harvested2, unharvested, chosenyear = 10, mode = "chosen"
  )

  expect_doppelganger("plotNutritionChange2", p)
})

