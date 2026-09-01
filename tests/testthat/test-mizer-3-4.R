test_that("plotYieldVsSize follows mizer's second-order quadrature", {
  params <- mizer::validParams(mizer::NS_params, info_level = 0)
  mizer::second_order_w(params) <- list(bin_average = TRUE)
  sim <- mizer::project(params, t_max = 1, effort = 1, method = "tr_bdf2")
  params <- mizer::finalParams(sim)

  sp <- mizer::species_params(params)
  min_w <- sp$w_mat / 100
  max_w <- sp$w_max
  f_mort <- mizer::getFMort(params)
  totals <- mizer::sizeIntegral(
    params,
    weighting = f_mort,
    min_w = min_w,
    max_w = max_w
  )
  species <- names(totals)[which(totals > 0)[1]]

  size_window <- mizer::get_size_range_array(
    params,
    min_w = min_w,
    max_w = max_w
  )
  catch_weight <- mizer::bin_average_weight(f_mort * size_window, params)
  expected <- mizer::initialN(params)[species, ] * catch_weight[species, ] /
    totals[[species]]

  result <- plotYieldVsSize(
    sim,
    species = species,
    x_var = "Weight",
    return_data = TRUE
  )
  w_idx <- match(result$w, mizer::w(params))

  expect_equal(result$`Catch density`, unname(expected[w_idx]))
})

test_that("bundled models use the mizer 3.4 S3 representation", {
  expect_s3_class(default_params, "MizerParams")
  expect_false(isS4(default_params))
  expect_no_warning(mizer::validParams(default_params, info_level = 0))

  expect_s3_class(default_sim, "MizerSim")
  expect_false(isS4(default_sim))
  expect_no_warning(mizer::validSim(default_sim))
})
