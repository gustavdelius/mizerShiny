test_that("plotSpectra2 works with default_sim", {
  params <- default_sim@params
  harvestedprojection <- default_sim
  harvestedprojection2 <- mizer::project(params, t_max = 10, effort = 1)

  time1 <- 1
  end1 <- 10
  time2 <- 1
  end2 <- 10

  # Need to check if plotSpectra is available - it might be from mizer package
  p <- mizerShiny:::plotSpectra2(
    harvestedprojection, harvestedprojection2, time1, end1, time2, end2
  )

  expect_s3_class(p, "ggplot")
})

test_that("plotSpectra2 snapshot test", {
  params <- default_sim@params
  harvestedprojection <- default_sim
  harvestedprojection2 <- mizer::project(params, t_max = 10, effort = 1)

  p <- mizerShiny:::plotSpectra2(
    harvestedprojection, harvestedprojection2, time1 = 1, end1 = 10, time2 = 1, end2 = 10
  )

  expect_warning(expect_doppelganger("plotSpectra2", p))
})

