# UI and server helpers used by the Shiny app

#' Legend popover button
#'
#' Creates a small info-styled button that triggers a Bootstrap popover showing
#' explanatory legend text.
#'
#' @param id Element id used for the popover button
#' @param text HTML string to display inside the popover
#' @return A Shiny tag
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
#'
#' Attaches observers to increment/decrement a slider input and dynamically cap
#' its maximum to twice the selected value within the range from 12 to 100.
#'
#' @param input Shiny input
#' @param session Shiny session
#' @param sliderId Id of the slider input
#' @param minusId Id of the "-" button
#' @param plusId Id of the "+" button
#' @return Invisibly, NULL
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

#' Generate plot with error handling
#'
#' Wraps plot generation in tryCatch with fallback to last successful plot.
#' This eliminates code duplication across modules.
#'
#' @param plot_fun Function that generates the plot (called in tryCatch)
#' @param last_plot_reactive A reactiveVal that stores the last successful plot
#' @param condition Optional condition to check before rendering (e.g., data requirements)
#' @return The generated plot or the last successful plot on error
#' @keywords internal
generatePlotWithErrorHandling <- function(plot_fun, last_plot_reactive, condition = NULL) {
  # Check condition if provided
  if (!is.null(condition) && !isTRUE(condition)) {
    return(last_plot_reactive())
  }
  
  p <- tryCatch({
    plot_fun()
  }, error = function(e) {
    last_plot_reactive()
  })
  
  # Store successful plot and return
  last_plot_reactive(p)
  p
}

#' Determine which simulations to show based on sim_choice
#'
#' Helper function to determine which simulations should be included
#' based on the sim_choice input (sim1, sim2, or both).
#'
#' @param sim_choice Character: "sim1", "sim2", or "both"
#' @return Named list with logical values: show_sim1, show_sim2
#' @keywords internal
getSimVisibility <- function(sim_choice) {
  list(
    show_sim1 = sim_choice == "sim1" || sim_choice == "both",
    show_sim2 = sim_choice == "sim2" || sim_choice == "both"
  )
}

#' Filter simulations based on sim_choice
#'
#' Returns a list of simulations to include based on the sim_choice input.
#'
#' @param sim_data List containing sim1, sim2, and optionally unharv
#' @param sim_choice Character: "sim1", "sim2", or "both"
#' @return List of simulation objects to plot
#' @keywords internal
filterSimsByChoice <- function(sim_data, sim_choice) {
  sims <- list()
  if (sim_choice == "sim1" || sim_choice == "both") {
    if (!is.null(sim_data$sim1)) {
      sims <- c(sims, list(sim_data$sim1))
    }
  }
  if (sim_choice == "sim2" || sim_choice == "both") {
    if (!is.null(sim_data$sim2)) {
      sims <- c(sims, list(sim_data$sim2))
    }
  }
  sims
}

#' Check if time range is within simulation bounds
#'
#' Validates that a time range is within the bounds of a simulation object.
#'
#' @param sim MizerSim object
#' @param time_range Integer or list with start/end times
#' @return Logical: TRUE if within bounds, FALSE otherwise
#' @keywords internal
checkTimeRangeBounds <- function(sim, time_range) {
  sim_dims <- dim(sim@n)
  max_time <- sim_dims[1]
  
  if (is.list(time_range)) {
    return(time_range$start <= max_time && time_range$end <= max_time)
  } else {
    return(time_range <= max_time)
  }
}

#' Subset simulation data by time range
#'
#' Creates a subset of simulation data for a specific time range.
#' Handles errors gracefully.
#'
#' @param sim MizerSim object
#' @param time_range Integer or list with start/end times
#' @return Subsetted MizerSim object
#' @keywords internal
subsetSimByTimeRange <- function(sim, time_range) {
  if (is.list(time_range)) {
    start <- time_range$start
    end <- time_range$end
  } else {
    start <- time_range
    end <- time_range
  }
  
  tryCatch({
    sim@n <- sim@n[start:end, , , drop = FALSE]
    sim@n_pp <- sim@n_pp[start:end, , drop = FALSE]
    sim@n_other <- sim@n_other[start:end, , drop = FALSE]
    sim
  }, error = function(e) {
    message("Error in subsetSimByTimeRange: ", e$message)
    message("time_range: ", if (is.list(time_range)) paste(time_range$start, time_range$end) else time_range)
    message("sim@n dims: ", paste(dim(sim@n), collapse = "x"))
    stop(e)
  })
}

#' Create a reactive value for storing last successful plot
#'
#' Convenience function to create a reactiveVal for plot error handling.
#'
#' @return A reactiveVal initialized to NULL
#' @keywords internal
createLastPlotReactive <- function() {
  reactiveVal(NULL)
}

#' Get mode string from toggle input
#'
#' Converts a toggle input (TRUE/FALSE) to mode string ("triple" or "chosen").
#'
#' @param toggle_value Logical: TRUE for "triple", FALSE for "chosen"
#' @return Character: "triple" or "chosen"
#' @keywords internal
getModeFromToggle <- function(toggle_value) {
  if (isTRUE(toggle_value)) "triple" else "chosen"
}


