# UI and server helpers used by the Shiny app

#' Legend popover button
#' @keywords internal
legendUI <- function(id, text) {
  h4(
    "Legend ",
    tags$button(
      id               = id,
      class            = "btn btn-info btn-sm legend-btn",
      type             = "button",
      `data-bs-toggle`    = "popover",
      `data-bs-html`      = "true",
      `data-bs-placement` = "right",
      `data-bs-container` = "body",
      `data-bs-content`   = text,
      tags$strong("View")
    )
  )
}

#' Setup +/- year controls associated with a slider
#' @keywords internal
setupYearControls <- function(input, session,
                              sliderId,
                              minusId,
                              plusId) {

  rv <- reactiveValues(
    origMin = NULL,
    origMax = NULL,
    curMax  = NULL
  )

  # Initialize the reactive values when the slider is first accessed
  observeEvent(input[[sliderId]], ignoreInit = FALSE, {
    if (is.null(rv$origMin)) {
      cfg            <- session$clientData[[ paste0("input_", sliderId, "_min") ]]
      rv$origMin     <- if (length(cfg)) cfg else 1
      cfg            <- session$clientData[[ paste0("input_", sliderId, "_max") ]]
      rv$origMax     <- if (length(cfg)) cfg else 100
      rv$curMax      <- rv$origMax
    }
  })

  # Update maximum value each time the user selects a new value
  observeEvent(input[[sliderId]], ignoreInit = TRUE, {
    # Set maximum to twice the selected value, capped between 12 and 100
    rv$curMax <- max(12, min(2 * input[[sliderId]], 100))
    updateSliderInput(session, sliderId, max = rv$curMax)
  })

  observeEvent(input[[plusId]], {
    if (is.null(rv$curMax)) return()
    newVal <- min( input[[sliderId]] + 1, rv$curMax )
    updateSliderInput(session, sliderId, value = newVal)
  })

  observeEvent(input[[minusId]], {
    if (is.null(rv$origMin)) return()
    newVal <- max( input[[sliderId]] - 1, rv$origMin )
    updateSliderInput(session, sliderId, value = newVal)
  })
}


