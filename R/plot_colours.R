#' Colour palettes for plots of absolute values
abs_colours <- function() {
  c(
    "Initial" = "#006400",
    "Quarter" = "#006400cc",
    "Half"    = "#00640099",
    "Full"    = "#00640066"
  )
}

#' Colour palettes for plots of percentage change
change_colours <- function() {
  c(
    "Quarter, Negative"  = "#E76F51",
    "Quarter, Positive"  = "#2FA4E7",
    "Half, Negative"     = "#E76F51cc",
    "Half, Positive"     = "#2FA4E7cc",
    "Full, Negative"     = "#E76F5199",
    "Full, Positive"     = "#2FA4E799"
  )
}
