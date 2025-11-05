test_that("create_species_level_plot works with test data", {
  data <- data.frame(
    Species = c("Species1", "Species2", "Species3"),
    normalized_value = c(10, -5, 20)
  )
  plot_title <- "Test Plot"
  
  p <- mizerShiny:::create_species_level_plot(data, plot_title)
  
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, plot_title)
})

test_that("create_species_level_plot snapshot test", {
  data <- data.frame(
    Species = c("Species1", "Species2", "Species3"),
    normalized_value = c(10, -5, 20)
  )
  plot_title <- "Test Plot"
  
  p <- mizerShiny:::create_species_level_plot(data, plot_title)
  
  expect_snapshot_output(print(p))
})

