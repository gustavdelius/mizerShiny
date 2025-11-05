test_that("guildplot works with default_sim", {
  # Create mock guildparams
  params <- default_sim@params
  species_names <- params@species_params$species[params@species_params$species != "Resource"]
  guildparams <- data.frame(
    Species = rep(species_names[1:min(3, length(species_names))], each = 2),
    minw = c(0.1, 10),
    maxw = c(10, 100),
    Feeding.guild = c("Plank", "Piscivore")
  )

  harvestedprojection <- default_sim
  unharvestedprojection <- mizer::project(params, t_max = 10, effort = 0)
  chosenyear <- 10
  celticsim <- NULL
  mode <- "chosen"

  p <- mizerShiny:::guildplot(
    harvestedprojection, unharvestedprojection, chosenyear, guildparams, celticsim, mode = mode
  )

  expect_s3_class(p, "ggplot")
})

test_that("guildplot works with triple mode", {
  params <- default_sim@params
  species_names <- params@species_params$species[params@species_params$species != "Resource"]
  guildparams <- data.frame(
    Species = rep(species_names[1:min(3, length(species_names))], each = 2),
    minw = c(0.1, 10),
    maxw = c(10, 100),
    Feeding.guild = c("Plank", "Piscivore")
  )

  harvestedprojection <- default_sim
  unharvestedprojection <- mizer::project(params, t_max = 10, effort = 0)
  chosenyear <- 10
  celticsim <- NULL
  mode <- "triple"

  p <- mizerShiny:::guildplot(
    harvestedprojection, unharvestedprojection, chosenyear, guildparams, celticsim, mode = mode
  )

  expect_s3_class(p, "ggplot")
})

test_that("guildplot snapshot test", {
  params <- default_sim@params
  species_names <- params@species_params$species[params@species_params$species != "Resource"]
  guildparams <- data.frame(
    Species = rep(species_names[1:min(3, length(species_names))], each = 2),
    minw = c(0.1, 10),
    maxw = c(10, 100),
    Feeding.guild = c("Plank", "Piscivore")
  )

  harvestedprojection <- default_sim
  unharvestedprojection <- mizer::project(params, t_max = 10, effort = 0)

  p <- mizerShiny:::guildplot(
    harvestedprojection, unharvestedprojection, chosenyear = 10,
    guildparams, NULL, mode = "chosen"
  )

  expect_snapshot_output(print(p))
})

