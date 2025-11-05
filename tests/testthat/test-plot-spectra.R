test_that("plotSpectraRelative works with default_sim", {
  params <- default_sim@params
  object1 <- mizer::project(params, t_max = 10, effort = 0)
  object2 <- default_sim

  time1 <- 1
  time2 <- 10

  p <- mizerShiny:::plotSpectraRelative(object1, object2, time1, time2)

  expect_s3_class(p, "ggplot")
})

test_that("plotSpectraRelative snapshot test", {
  params <- default_sim@params
  object1 <- mizer::project(params, t_max = 10, effort = 0)
  object2 <- default_sim

  p <- mizerShiny:::plotSpectraRelative(object1, object2, time1 = 1, time2 = 10)

  expect_doppelganger("plotSpectraRelative", p)
})

test_that("plotSpectraRelative2 works with default_sim", {
  params <- default_sim@params
  object1 <- mizer::project(params, t_max = 10, effort = 0)
  object2 <- default_sim
  object3 <- mizer::project(params, t_max = 10, effort = 1)

  time1 <- 1
  time2 <- 10

  p <- mizerShiny:::plotSpectraRelative2(object1, object2, object3, time1, time2)

  expect_s3_class(p, "ggplot")
})

test_that("plotSpectraRelative2 snapshot test", {
  params <- default_sim@params
  object1 <- mizer::project(params, t_max = 10, effort = 0)
  object2 <- default_sim
  object3 <- mizer::project(params, t_max = 10, effort = 1)

  p <- mizerShiny:::plotSpectraRelative2(object1, object2, object3, time1 = 1, time2 = 10)

  expect_doppelganger("plotSpectraRelative2", p)
})

