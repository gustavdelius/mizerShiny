test_that("generateYieldDashboard works with default_sim", {
  NS_sim <- list(default_sim)
  highlight_times <- NULL
  params <- default_sim@params
  
  p <- mizerShiny:::generateYieldDashboard(NS_sim, highlight_times, params)
  
  expect_s3_class(p, "plotly")
})

test_that("generateYieldDashboard works with two simulations", {
  params <- default_sim@params
  NS_sim <- list(default_sim, mizer::project(params, t_max = 10, effort = 1))
  highlight_times <- NULL
  params <- default_sim@params
  
  p <- mizerShiny:::generateYieldDashboard(NS_sim, highlight_times, params)
  
  expect_s3_class(p, "plotly")
})

test_that("generateYieldDashboard works with highlight_times", {
  NS_sim <- list(default_sim)
  highlight_times <- c(5, 10)
  params <- default_sim@params
  
  p <- mizerShiny:::generateYieldDashboard(NS_sim, highlight_times, params)
  
  expect_s3_class(p, "plotly")
})

test_that("generateYieldDashboard snapshot test", {
  NS_sim <- list(default_sim)
  params <- default_sim@params
  
  p <- mizerShiny:::generateYieldDashboard(NS_sim, highlight_times = NULL, params = params)
  
  # For plotly objects, snapshot the structure
  expect_snapshot_output(str(p))
})

