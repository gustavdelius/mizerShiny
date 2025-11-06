library(testthat)
library(mizer)
library(mizerShiny)
library(ggplot2)
library(tibble)
library(dplyr)

# Need to use vdiffr conditionally
expect_doppelganger <- function(title, fig, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, ...)
}
