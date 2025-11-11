# Fishery Strategy Module
# Handles the Fishery Strategy tab UI and server logic

fishery_strategy_ui <- function(id, config, legends, have_guild_file,
                                have_nutrition_file,
                                fishery_strategy_tabs) {
  ns <- NS(id)

  grid_container(
    layout    = config$layout,
    row_sizes = config$row_sizes,
    col_sizes = config$col_sizes,
    gap_size  = config$gap_size,
    # Controls ----
    grid_card(
      area = "area1",
      card_body(
        ## Multispecies toggle ----
        div(
          style = "display:flex; flex-direction:row; align-items:center; gap:8px; padding: 2px 6px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb; min-height:32px;",
          `data-bs-toggle` = "popover",
          `data-bs-placement` = "right",
          `data-bs-html` = "true",
          `data-bs-content` = as.character(legends$fishery_multispecies),
          materialSwitch(
            inputId = ns("multispeciesToggle"),
            label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Multispecies effects</span>"),
            value   = TRUE,
            status  = "info"
          )
        ),

        ## Time Range ----
        div(
          style = "display:flex; flex-direction:column; gap:0px; padding: 6px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
          `data-bs-toggle` = "popover",
          `data-bs-placement` = "right",
          `data-bs-html` = "true",
          `data-bs-content` = as.character(legends$fishery_time_range),
          sliderInput(
            inputId = ns("fishyear"),
            label   = "Time Range",
            min     = 3,
            max     = config$max_year,
            value   = 5,
            step    = 1,
            width   = "100%"
          ) |> tagAppendAttributes(id = "fishyyear", style = "margin-bottom: 4px;"),
          div(
            id    = ns("yearAdjustButtons_fish"),
            style = "display:flex; justify-content:center; gap:4px; margin-top: 2px;",
            actionButton(ns("decYear_fish"), "-1 year", class = "btn-small"),
            actionButton(ns("incYear_fish"), "+1 year", class = "btn-small")
          )
        ),
        ## Simulation Choice ----
        div(
          style = "margin: 2px 0 0; padding: 2px 6px 0 6px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
          `data-bs-toggle` = "popover",
          `data-bs-placement` = "right",
          `data-bs-html` = "true",
          `data-bs-content` = as.character(legends$fishery_sim_choice),
          div(
            style = "display: flex; align-items: center; gap: 8px;",
            tags$span(
              style = "font-weight:500; font-size: 0.9em; color: var(--bs-heading-color); display:flex; align-items:center;",
              "Show:"
            ),
            tags$style(
              HTML(
                sprintf(
                  "#%s { margin: 0; }
#%s .shiny-options-group { margin-top: 0; display: flex; align-items: center; gap: 12px; }
#%s .form-check { margin-bottom: 0; }",
                  ns("sim_choice"),
                  ns("sim_choice"),
                  ns("sim_choice")
                )
              )
            ),
            radioButtons(
              inputId = ns("sim_choice"),
              label   = NULL,
              choices = c("Strategy 1" = "sim1", "Strategy 2" = "sim2", "Both" = "both"),
              selected = "sim1",
              inline = TRUE
            ) |>
              tagAppendAttributes(style = "margin: 0;")
          )
        ),
        ## Simulation Tabs ----
        tabsetPanel(
          tabPanel(
            title = "Strategy 1",
            div(id = "fishery_sliders", uiOutput(ns("fishery_sliders_ui")))
          ),
          tabPanel(
            title = span(
              "Strategy 2",
              `data-bs-toggle` = "popover",
              `data-bs-placement` = "top",
              `data-bs-html` = "true",
              `data-bs-content` = as.character(legends$fishery_slider_tabs)
            ),
            div(id = "fishery_sliders", uiOutput(ns("fishery_sliders_ui2")))
          )
        ) |>
          tagAppendAttributes(
            style = "margin-top: 0px;"
          )
      )
    ),

    # Tabs ----
    grid_card(
        area = "area0",
        card_body(
            div(
                class = "plot-card",
                style = "flex: 4.5; height:50vh; display:flex; flex-direction:column; overflow: hidden; margin-top: -0.5rem",
                ## Biomass Plot ----
                tabsetPanel(
                    id = ns("fishy_plots"),
                    if ("Biomass" %in% fishery_strategy_tabs) {
                        tabPanel(
                            value = "Biomass",
                            title = span(
                              "Biomass",
                              `data-bs-toggle` = "popover",
                              `data-bs-placement` = "bottom",
                              `data-bs-html` = "true",
                              `data-bs-container` = "body",
                              `data-bs-title` = as.character(legends$fishery_biomass)
                            ),
                            div(style = "flex:1; display:flex;",
                                plotlyOutput(ns("fishspeciesActualPlot"),
                                             height = "100%", width = "100%")
                            )
                        )
                    },
                    ## Biomass % Change Plot ----
                    if ("Biomass % Change" %in% fishery_strategy_tabs) {
                        tabPanel(
                            value = "Biomass % Change",
                            title = span(
                              "Biomass % Change",
                              `data-bs-toggle` = "popover",
                              `data-bs-placement` = "bottom",
                              `data-bs-html` = "true",
                              `data-bs-title` = as.character(legends$fishery_biomass_change)
                            ),
                            div(style = "flex:1; display:flex;",
                                plotlyOutput(ns("fishspeciesPlot"),
                                             height = "100%", width = "100%")
                            )
                        )
                    },
                    ## Yield Plot ----
                    if ("Yield" %in% fishery_strategy_tabs) {
                        tabPanel(
                            value = "Yield",
                            title = span(
                              "Yield",
                              `data-bs-toggle` = "popover",
                              `data-bs-placement` = "bottom",
                              `data-bs-html` = "true",
                              `data-bs-title` = as.character(legends$fishery_yield)
                            ),
                            div(style = "flex:1; display:flex;",
                                plotlyOutput(ns("fishspeciesYieldPlot"),
                                             height = "100%", width = "100%")
                            )
                        )
                    },
                    ## Yield % Change Plot ----
                    if ("Yield % Change" %in% fishery_strategy_tabs) {
                        tabPanel(
                            value = "Yield % Change",
                            title = span(
                              "Yield % Change",
                              `data-bs-toggle` = "popover",
                              `data-bs-placement` = "bottom",
                              `data-bs-html` = "true",
                              `data-bs-title` = as.character(legends$fishery_yield_change)
                            ),
                            div(style = "flex:1; display:flex;",
                                plotlyOutput(ns("fishspeciesYieldChangePlot"),
                                             height = "100%", width = "100%")
                            )
                        )
                    },
                    ## Nutrition Plot ----
                    if (have_nutrition_file &&
                        ("Nutrition change" %in% fishery_strategy_tabs)) {
                        tabPanel(
                            value = "Nutrition",
                            title = span(
                              "Nutrition",
                              `data-bs-toggle` = "popover",
                              `data-bs-placement` = "bottom",
                              `data-bs-html` = "true",
                              `data-bs-title` = as.character(legends$fishery_nutrition)
                            ),
                            div(style = "flex:1; display:flex;",
                                plotlyOutput(ns("nutritionplot"),
                                             height = "100%", width = "100%")
                            )
                        )
                    },
                    ## Length Plot ----
                    if ("Length" %in% fishery_strategy_tabs) {
                        tabPanel(
                            value = "Length",
                            title = span(
                              "Length",
                              `data-bs-toggle` = "popover",
                              `data-bs-placement` = "bottom",
                              `data-bs-html` = "true",
                              `data-bs-title` = as.character(legends$fishery_length)
                            ),
                            div(style = "flex:1; display:flex;",
                                plotlyOutput(ns("fishlengthPlot"),
                                             height = "100%", width = "100%")
                            )
                        )
                    },
                    ## Guild Plot ----
                    if (have_guild_file &&
                        ("Guild" %in% fishery_strategy_tabs)) {
                        tabPanel(
                            value = "Guild",
                            title = span(
                              "Guild",
                              `data-bs-toggle` = "popover",
                              `data-bs-placement` = "bottom",
                              `data-bs-html` = "true",
                              `data-bs-title` = as.character(legends$fishery_guild)
                            ),
                            div(style = "flex:1; display:flex;",
                                plotlyOutput(ns("fishguildPlot"),
                                             height = "100%", width = "100%")
                            )
                        )
                    },
                    ## Yield Composition Plot ----
                    if ("Yield Composition" %in% fishery_strategy_tabs) {
                        tabPanel(
                            value = "Yield Composition",
                            title = span(
                              "Yield Composition",
                              `data-bs-toggle` = "popover",
                              `data-bs-placement` = "bottom",
                              `data-bs-html` = "true",
                              `data-bs-title` = as.character(legends$fishery_yield_composition)
                            ),
                            div(style = "flex:1; display:flex;",
                                plotlyOutput(ns("yieldPlot"),
                                             height = "100%", width = "100%")
                            )
                        )
                    },
                    ## Size Plot ----
                    if ("Size" %in% fishery_strategy_tabs) {
                        tabPanel(
                            value = "Size",
                            title = span(
                              "Size",
                              `data-bs-toggle` = "popover",
                              `data-bs-placement` = "bottom",
                              `data-bs-html` = "true",
                              `data-bs-title` = as.character(legends$fishery_size)
                            ),
                            div(style = "flex:1; display:flex;",
                                plotlyOutput(ns("fishsizePlot"),
                                             height = "100%", width = "100%")
                            )
                        )
                    },
                    ## Spectra Plot ----
                    if ("Spectra" %in% fishery_strategy_tabs) {
                        tabPanel(
                            value = "Spectra",
                            title = span(
                              "Spectra",
                              `data-bs-toggle` = "popover",
                              `data-bs-placement` = "bottom",
                              `data-bs-html` = "true",
                              `data-bs-title` = as.character(legends$fishery_spectra)
                            ),
                            div(style = "flex:1; display:flex;",
                                plotlyOutput(ns("spectrumPlot"),
                                             height = "100%", width = "100%")
                            )
                        )
                    },
                    ## Diet Plot ----
                    if ("Diet" %in% fishery_strategy_tabs) {
                        tabPanel(
                            value = "Diet",
                            title = span(
                              "Diet",
                              `data-bs-toggle` = "popover",
                              `data-bs-placement` = "bottom",
                              `data-bs-html` = "true",
                              `data-bs-title` = as.character(legends$fishery_diet)
                            ),
                            div(style = "height:50vh; display:flex;",
                                plotlyOutput(ns("fishdietsingleplot"),
                                             height = "100%", width = "100%")
                            )
                        )
                    }
                )
            )
        ),

      # Conditional Panels ----
      card_body(
        style = "flex: auto",
        ## Yield Composition Time Range Slider ----
        conditionalPanel(
          condition = paste0("input['", ns("fishy_plots"), "'] == 'Yield Composition'"),
          div(style = "display: flex; align-items: center; gap: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
              HTML("<span style='margin-top:0px; margin-bottom:0.5rem; font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Yield Time Range</span>"),
              sliderInput(
                inputId = ns("fishyear2_yield"),
                label   = NULL,
                min     = 1,
                max     = config$max_year,
                value   = c(1, 10),
                step    = 1,
                width   = "200px"
              ) |> tagAppendAttributes(id = "fishyyear_yield")
          )
        ),
        ## Species Order and Show Intermediate Years Toggle ----
        conditionalPanel(
          condition = paste0("input['", ns("fishy_plots"), "'] == 'Biomass' || input['", ns("fishy_plots"), "'] == 'Biomass % Change' || input['", ns("fishy_plots"), "'] == 'Yield' || input['", ns("fishy_plots"), "'] == 'Yield % Change' || input['", ns("fishy_plots"), "'] == 'Guild' || input['", ns("fishy_plots"), "'] == 'Nutrition'"),
          div(style = "display: flex; align-items: center; gap: 15px; flex-wrap: wrap;",
              conditionalPanel(
                condition = paste0("input['", ns("fishy_plots"), "'] == 'Biomass' || input['", ns("fishy_plots"), "'] == 'Biomass % Change' || input['", ns("fishy_plots"), "'] == 'Yield' || input['", ns("fishy_plots"), "'] == 'Yield % Change'"),
                div(
                  style = "display: flex; align-items: center; gap: 10px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb; margin-top: 8px;",
                  `data-bs-toggle` = "popover",
                  `data-bs-placement` = "top",
                  `data-bs-html` = "true",
                  `data-bs-content` = as.character(legends$species_order_help),
                  tags$span(
                    style = "font-weight:500; color: var(--bs-heading-color); line-height:1.2; display:flex; align-items:center;",
                    "Species Order:"
                  ),
                  tags$style(
                    HTML(
                      sprintf(
                        "#%s { margin: 0; }
#%s .shiny-options-group { margin: 0; }
#%s .form-select { height: auto; padding-top: 4px; padding-bottom: 4px; }
#%s .selectize-control.single .selectize-input { padding: 4px 28px 4px 8px; }",
                        ns("species_order_fish"),
                        ns("species_order_fish"),
                        ns("species_order_fish"),
                        ns("species_order_fish")
                      )
                    )
                  ),
                  selectInput(
                    inputId = ns("species_order_fish"),
                    label   = NULL,
                    choices = c("Custom", "Size", "Guild"),
                    width = "120px"
                  ) |>
                    tagAppendAttributes(style = "margin: 0; align-self: center;"),
                  actionButton(
                    ns("customOrderInfo_fish"),
                    label = HTML("<strong>customise</strong>"),
                    class = "btn btn-info btn-xs no-focus-outline"
                  )
                )
              ),
              div(
                style = "display: flex; align-items: center; gap: 10px; padding: 10px; background-color: #f3e5f5; border-radius: 5px; border: 1px solid #e1bee7;",
                `data-bs-toggle` = "popover",
                `data-bs-placement` = "top",
                `data-bs-html` = "true",
                `data-bs-content` = as.character(legends$fishery_show_intermediate_years),
                materialSwitch(
                  inputId = ns("triplotToggleFish"),
                  label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show intermediate years</span>"),
                  value   = TRUE,
                  status  = "info"
                )
              )
          )
        ),
        ## Log Toggle for Size Plot ----
        conditionalPanel(
          condition = paste0("input['", ns("fishy_plots"), "'] == 'Size'"),
          div(style = "display: flex; align-items: center; gap: 15px;",
              div(style = "padding: 10px; background-color: #e8f5e8; border-radius: 5px; border: 1px solid #c8e6c9;",
                  materialSwitch(
                    inputId = ns("logToggle4"),
                    label   = HTML(paste0("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;' data-bs-toggle='popover' data-bs-placement='left' title='' data-bs-content='", as.character(legends$fishery_log), "'>Log</span>")),
                    value   = TRUE,
                    status  = "info"
                  )
              )
          )
        ),
        ## Log Toggle for Spectra Plot ----
        conditionalPanel(
          condition = paste0("input['", ns("fishy_plots"), "'] == 'Spectra'"),
          div(style = "display: flex; align-items: center; gap: 15px;",
              div(style = "padding: 10px; background-color: #fce4ec; border-radius: 5px; border: 1px solid #f8bbd9;",
                  materialSwitch(
                    inputId = ns("logToggle5"),
                    label   = HTML(paste0("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;' data-bs-toggle='popover' data-bs-placement='left' title='' data-bs-content='", as.character(legends$fishery_log), "'>Log</span>")),
                    value   = TRUE,
                    status  = "info"
                  )
              )
          )
        ),
        ## Diet Plot Species Selector ----
        conditionalPanel(
          condition = paste0("input['", ns("fishy_plots"), "'] == 'Diet'"),
          div(style = "display: flex; align-items: center; gap: 15px;",
              div(style = "display: flex; align-items: center; gap: 10px; padding: 10px; background-color: #e0f2f1; border-radius: 5px; border: 1px solid #b2dfdb;",
                  HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Select a Species:</span>"),
                  selectInput(
                    inputId = ns("fish_name_select"),
                    label   = NULL,
                    choices = NULL,
                    width = "200px"
                  )
              )
          )
        ),
      )
    )
  )
}

fishery_strategy_server <- function(id, sim_0,
                                   guildparams, ordered_species_reactive, species_list) {
  params <- sim_0@params
  # keep copies for switching between interacting and non-interacting
  params_interacting <- params
  params_noninteracting <- mizerEcopath::makeNoninteracting(params)

  moduleServer(id, function(input, output, session) {

    # Update species select input
    observe({
      updateSelectInput(session, "fish_name_select", choices = species_list)
    })

      # Effort sliders ----
    # Dynamic fishery effort sliders for Strategy 1
    output$fishery_sliders_ui <- renderUI({
      effort <- params@initial_effort
      gears <- unique(params@gear_params$gear)
      slider_list <- lapply(gears, function(gear) {
        sliderInput(
          inputId = session$ns(paste0("effort_", gear)),
          label = paste("Effort for", gear),
          min = 0,
          max = if (params@initial_effort[gear] == 0) {
            2
          } else (params@initial_effort[gear] * 2),
          value = params@initial_effort[gear],
          step = 0.05,
          width = "100%"
        )
      })
      div(
        id = "fishery_sliders",
        tagList(slider_list),
        div(
          style = "display:flex; justify-content:flex-end; margin-top: 8px;",
          actionButton(
            session$ns("reset_effort_sim1"),
            label = "Reset",
            class = "btn btn-secondary btn-sm"
          )
        )
      )
    })

    # Dynamic fishery effort sliders for Strategy 2
    output$fishery_sliders_ui2 <- renderUI({
      effort <- params@initial_effort
      gears <- unique(params@gear_params$gear)
      slider_list <- lapply(gears, function(gear) {
        sliderInput(
          inputId = session$ns(paste0("effort2_", gear)),
          label = paste("Effort for", gear),
          min = 0,
          max = if(params@initial_effort[gear]==0){
            2
          }else(params@initial_effort[gear]*2),
          value = params@initial_effort[gear],
          step = 0.05,
          width = "100%"
        )
      })
      div(
        id = "fishery_sliders",
        tagList(slider_list),
        div(
          style = "display:flex; justify-content:flex-end; margin-top: 8px;",
          actionButton(
            session$ns("reset_effort_sim2"),
            label = "Reset",
            class = "btn btn-secondary btn-sm"
          )
        )
      )
    })

    # Observers ----
    observeEvent(input$multispeciesToggle, {
      updated_params <- if (isTRUE(input$multispeciesToggle)) {
        params_interacting
      } else {
        params_noninteracting
      }

      params <<- updated_params

      max_year <- isolate(input$fishyear)
      notif_id <- shiny::showNotification(
        "Updating simulations …",
        type = "message",
        duration = NULL,
        closeButton = TRUE
      )
      on.exit(shiny::removeNotification(id = notif_id, session = session), add = TRUE)

      total_steps <- 3
      pb <- shiny::Progress$new(); on.exit(pb$close(), add = TRUE)
      pb$set(message = "Running simulation …", value = 0)

      gears <- unique(params@gear_params$gear)
      effort_sim1 <- makeEffort("effort_",  gears, params@initial_effort)
      effort_sim2 <- makeEffort("effort2_", gears, params@initial_effort)

      pb$inc(1 / total_steps, "Projecting Strategy 1 …")
      sim1 <- mizerShiny:::runSimulationWithErrorHandling(
        function() project(params, effort = effort_sim1, t_max = max_year),
        context = "fishery_multispecies_sim1"
      )

      pb$inc(1 / total_steps, "Projecting Strategy 2 …")
      sim2 <- mizerShiny:::runSimulationWithErrorHandling(
        function() project(params, effort = effort_sim2, t_max = max_year),
        context = "fishery_multispecies_sim2"
      )

      pb$inc(1 / total_steps, "Projecting base …")
      sim0 <- mizerShiny:::runSimulationWithErrorHandling(
        function() project(params, t_max = max_year),
        context = "fishery_multispecies_unharv"
      )

      fishSimData(list(sim1 = sim1, sim2 = sim2, unharv = sim0))
    }, ignoreInit = TRUE)

    # Changing the timerange to subset on the plot for yield
    observe({
      time1  <- input$fishyear  + 1
      time12 <- max(input$fishyear - 1, 1)
      new_max <- time1

      current <- input$fishyear2_yield
      new_val <- c(
        max(min(current[1], new_max), 0),
        max(min(current[2], new_max), time12)
      )

      updateSliderInput(
        session, "fishyear2_yield",
        max   = new_max,
        value = new_val
      )
    })

    # Helper function to reactively make new effort vector depending on the input
    makeEffort <- function(prefix, gears, base_effort) {
      ef <- base_effort
      for (g in gears) {
        id <- paste0(prefix, g)
        # Inside moduleServer, input is already namespaced
        if (!is.null(input[[id]]))
          ef[g] <- input[[id]]
      }
      ef
    }

    # Observer to automatically switch to "Both" when Strategy 2 sliders are changed
    observeEvent({
      gears <- unique(params@gear_params$gear)
      lapply(gears, function(gear) {
        input[[paste0("effort2_", gear)]]
      })
    }, {
      sim_choice <- isolate(input$sim_choice)
      if (sim_choice != "sim2" && sim_choice != "both") {
        updateRadioButtons(session, "sim_choice", selected = "both")
      }
    }, ignoreInit = TRUE)

    # Initialize fishSimData as a reactive value
    fishSimData <- reactiveVal(list(
      sim1 = sim_0,
      sim2 = sim_0,
      unharv = sim_0
    ))

    # Reactive observer that runs when time range changes
    observe({
      sims <- isolate(fishSimData())

      target_year <- input$fishyear
      # Determine current max years for each sim (years start at 0, so subtract 1)
      max_sim1   <- dim(sims$sim1@n)[1] - 1
      max_sim2   <- if (!is.null(sims$sim2)) dim(sims$sim2@n)[1] - 1 else NA_integer_
      max_unharv <- dim(sims$unharv@n)[1] - 1

      need_sim1   <- target_year > max_sim1
      need_sim2   <- !is.null(sims$sim2) && target_year > max_sim2
      need_unharv <- target_year > max_unharv

      if (!need_sim1 && !need_sim2 && !need_unharv) return()

      notif_id <- shiny::showNotification(
        "Extending simulation …",
        type = "message",
        duration = NULL,
        closeButton = TRUE
      )
      on.exit(shiny::removeNotification(id = notif_id, session = session), add = TRUE)

      # Count how many projections we will run
      steps <- sum(c(need_sim1, need_sim2, need_unharv))
      pb <- shiny::Progress$new(); on.exit(pb$close(), add = TRUE)
      pb$set(message = "Running simulation …", value = 0)

      sim1   <- sims$sim1
      sim2   <- sims$sim2
      unharv <- sims$unharv

      if (need_sim1) {
        t_max1 <- target_year - max_sim1 + 5
        pb$inc(1/steps, "Projecting Strategy 1 …")
        sim1 <- mizerShiny:::runSimulationWithErrorHandling(
          function() project(sims$sim1, t_max = t_max1),
          context = "fishery_time_range_sim1"
        )
      }

      if (need_sim2) {
        t_max2 <- target_year - max_sim2 + 5
        pb$inc(1/steps, "Projecting Strategy 2 …")
        sim2 <- mizerShiny:::runSimulationWithErrorHandling(
          function() project(sims$sim2, t_max = t_max2),
          context = "fishery_time_range_sim2"
        )
      }

      if (need_unharv) {
        t_maxu <- target_year - max_unharv + 5
        pb$inc(1/steps, "Projecting base …")
        unharv <- mizerShiny:::runSimulationWithErrorHandling(
          function() project(sims$unharv, t_max = t_maxu),
          context = "fishery_time_range_unharv"
        )
      }

      fishSimData(list(sim1 = sim1, sim2 = sim2, unharv = unharv))
    })

    # Re-run only Strategy 1 when Strategy 1 effort sliders change
    observeEvent({
      gears <- unique(params@gear_params$gear)
      lapply(gears, function(gear) input[[paste0("effort_", gear)]])
    }, {
      gears <- unique(params@gear_params$gear)
      effort1 <- makeEffort("effort_" , gears, params@initial_effort)
      max_year <- isolate(input$fishyear)

      pb <- shiny::Progress$new(); on.exit(pb$close(), add = TRUE)
      pb$set(message = "Running fishery simulation …", value = 0)

      pb$inc(1, "Projecting Strategy 1 …")
      sim1 <- mizerShiny:::runSimulationWithErrorHandling(
        function() project(params, effort = effort1, t_max = max_year + 5),
        context = "fishery_sim1"
      )

      sims <- isolate(fishSimData())
      fishSimData(list(sim1 = sim1, sim2 = sims$sim2, unharv = sims$unharv))
    }, ignoreInit = TRUE)

    # Re-run only Strategy 2 when Strategy 2 effort sliders change
    observeEvent({
      gears <- unique(params@gear_params$gear)
      lapply(gears, function(gear) input[[paste0("effort2_", gear)]])
    }, {
      gears <- unique(params@gear_params$gear)
      effort2 <- makeEffort("effort2_", gears, params@initial_effort)
      max_year <- isolate(input$fishyear)

      pb <- shiny::Progress$new(); on.exit(pb$close(), add = TRUE)
      pb$set(message = "Running fishery simulation …", value = 0)

      pb$inc(1, "Projecting Strategy 2 …")
      sim2 <- mizerShiny:::runSimulationWithErrorHandling(
        function() project(params, effort = effort2, t_max = max_year + 5),
        context = "fishery_sim2"
      )

      sims <- isolate(fishSimData())
      fishSimData(list(sim1 = sims$sim1, sim2 = sim2, unharv = sims$unharv))
    }, ignoreInit = TRUE)

    # Reset Strategy 1 effort sliders to base effort
    observeEvent(input$reset_effort_sim1, {
      base_effort <- params@initial_effort
      gears <- unique(params@gear_params$gear)
      lapply(gears, function(gear) {
        updateSliderInput(
          session,
          paste0("effort_", gear),
          value = unname(base_effort[gear])
        )
      })
    })

    # Reset Strategy 2 effort sliders to base effort
    observeEvent(input$reset_effort_sim2, {
      base_effort <- params@initial_effort
      gears <- unique(params@gear_params$gear)
      lapply(gears, function(gear) {
        updateSliderInput(
          session,
          paste0("effort2_", gear),
          value = unname(base_effort[gear])
        )
      })
    })

    # Setup year controls
    mizerShiny:::setupYearControls(
      input, session,
      sliderId  = "fishyear",
      minusId   = "decYear_fish",
      plusId    = "incYear_fish"
    )

    # Simple reactive for the time range
    fish_win1 <- reactive({
      input$fishyear
    })

    # Reactive storage for last successful plots
    lastYieldPlot                <- mizerShiny:::createLastPlotReactive()
    lastFishSpeciesPlot          <- mizerShiny:::createLastPlotReactive()
    lastFishSpeciesActualPlot    <- mizerShiny:::createLastPlotReactive()
    lastFishSpeciesYieldPlot     <- mizerShiny:::createLastPlotReactive()
    lastFishSpeciesYieldChangePlot <- mizerShiny:::createLastPlotReactive()
    lastFishSizePlot             <- mizerShiny:::createLastPlotReactive()
    lastFishGuildPlot            <- mizerShiny:::createLastPlotReactive()
    lastSpectrumPlot             <- mizerShiny:::createLastPlotReactive()
    lastFishDietSinglePlot       <- mizerShiny:::createLastPlotReactive()
    lastNutritionPlot            <- mizerShiny:::createLastPlotReactive()
    lastFishLengthPlot           <- mizerShiny:::createLastPlotReactive()

    # Yield dashboard ----
    output$yieldPlot <- renderPlotly({
      req(fishSimData())

      sims <- mizerShiny:::filterSimsByChoice(fishSimData(), input$sim_choice)

      mizerShiny:::generatePlotWithErrorHandling(
        plot_fun = function() {
          mizerShiny:::generateYieldDashboard(
            NS_sim          = sims,
            highlight_times = input$fishyear2_yield,
            params          = params
          )
        },
        last_plot_reactive = lastYieldPlot,
        condition = length(sims) > 0,
        context = "yieldPlot"
      )
    })

    # Biomass plot ----
    output$fishspeciesActualPlot <- renderPlotly({
      req(fishSimData())
      chosen_year <- fish_win1()

      vis <- mizerShiny:::getSimVisibility(input$sim_choice)

      mizerShiny:::generatePlotWithErrorHandling(
        plot_fun = function() {
          mode <- mizerShiny:::getModeFromToggle(input$triplotToggleFish)

          if (vis$show_sim1 && vis$show_sim2) {
            ggplotly(
              mizerShiny:::plotSpeciesActualBiomass2(
                fishSimData()$sim1,
                fishSimData()$sim2,
                chosen_year,
                mode = mode
              ) + scale_x_discrete(limits = ordered_species_reactive())
            )
          } else if (vis$show_sim1) {
            ggplotly(
              mizerShiny:::plotSpeciesActualBiomass(
                fishSimData()$sim1,
                chosen_year,
                mode = mode
              ) +
                scale_x_discrete(limits = ordered_species_reactive())
            )
          } else {
            ggplotly(
              mizerShiny:::plotSpeciesActualBiomass(
                fishSimData()$sim2,
                chosen_year,
                mode = mode
              ) +
                scale_x_discrete(limits = ordered_species_reactive())
            )
          }
        },
        last_plot_reactive = lastFishSpeciesActualPlot,
        condition = vis$show_sim1 || vis$show_sim2,
        context = "fishspeciesActualPlot"
      )
    })

    # Yield plot ----
    output$fishspeciesYieldPlot <- renderPlotly({
      req(fishSimData())
      chosen_year <- fish_win1()

      vis <- mizerShiny:::getSimVisibility(input$sim_choice)

      mizerShiny:::generatePlotWithErrorHandling(
        plot_fun = function() {
          mode <- mizerShiny:::getModeFromToggle(input$triplotToggleFish)

          if (vis$show_sim1 && vis$show_sim2) {
            ggplotly(
              suppressMessages(
                mizerShiny:::plotSpeciesActualYield2(
                  fishSimData()$sim1,
                  fishSimData()$sim2,
                  chosen_year,
                  mode = mode
                ) + scale_x_discrete(limits = ordered_species_reactive())
              ),
              tooltip = "text"
            )
          } else if (vis$show_sim1) {
            ggplotly(
                suppressMessages(
                mizerShiny:::plotSpeciesActualYield(
                  fishSimData()$sim1,
                  chosen_year,
                  mode = mode
                ) +
                  scale_x_discrete(limits = ordered_species_reactive())
              ),
              tooltip = "text"
            )
          } else {
            ggplotly(
                suppressMessages(
                mizerShiny:::plotSpeciesActualYield(
                  fishSimData()$sim2,
                  chosen_year,
                  mode = mode
                ) +
                  scale_x_discrete(limits = ordered_species_reactive())
              ),
              tooltip = "text"
            )
          }
        },
        last_plot_reactive = lastFishSpeciesYieldPlot,
        condition = vis$show_sim1 || vis$show_sim2,
        context = "fishspeciesYieldPlot"
      )
    })

    # Yield change plot ----
    output$fishspeciesYieldChangePlot <- renderPlotly({
      req(fishSimData())
      chosen_year <- fish_win1()

      vis <- mizerShiny:::getSimVisibility(input$sim_choice)

      mizerShiny:::generatePlotWithErrorHandling(
        plot_fun = function() {
          mode <- mizerShiny:::getModeFromToggle(input$triplotToggleFish)

          if (vis$show_sim1 && vis$show_sim2) {
            ggplotly(
              mizerShiny:::plotSpeciesYieldChange2(
                fishSimData()$sim1,
                fishSimData()$sim2,
                fishSimData()$unharv,
                chosen_year,
                mode = mode
              ) + scale_x_discrete(limits = ordered_species_reactive())
            )
          } else if (vis$show_sim1) {
            ggplotly(
              mizerShiny:::plotSpeciesYieldChange(
                fishSimData()$sim1,
                fishSimData()$unharv,
                chosen_year,
                mode = mode
              ) +
                scale_x_discrete(limits = ordered_species_reactive())
            )
          } else {
            ggplotly(
              mizerShiny:::plotSpeciesYieldChange(
                fishSimData()$sim2,
                fishSimData()$unharv,
                chosen_year,
                mode = mode
              ) +
                scale_x_discrete(limits = ordered_species_reactive())
            )
          }
        },
        last_plot_reactive = lastFishSpeciesYieldChangePlot,
        condition = vis$show_sim1 || vis$show_sim2,
        context = "fishspeciesYieldChangePlot"
      )
    })

    # Length (Yield vs Size by Length) plot ----
    output$fishlengthPlot <- renderPlotly({
      req(fishSimData())

      vis <- mizerShiny:::getSimVisibility(input$sim_choice)

      mizerShiny:::generatePlotWithErrorHandling(
        plot_fun = function() {
          sims <- list(Baseline = fishSimData()$unharv)
          if (vis$show_sim1) sims <- c(sims, list(`Strategy 1` = fishSimData()$sim1))
          if (vis$show_sim2) sims <- c(sims, list(`Strategy 2` = fishSimData()$sim2))
          ggplotly(mizerShiny:::plotYieldVsSize(sims, x_var = "Length"))
        },
        last_plot_reactive = lastFishLengthPlot,
        condition = TRUE,
        context = "fishlengthPlot"
      )
    })

    # Biomass change plot ----
    output$fishspeciesPlot <- renderPlotly({
      req(fishSimData())
      chosen_year <- fish_win1()

      vis <- mizerShiny:::getSimVisibility(input$sim_choice)

      mizerShiny:::generatePlotWithErrorHandling(
        plot_fun = function() {
          mode <- mizerShiny:::getModeFromToggle(input$triplotToggleFish)

          if (vis$show_sim1 && vis$show_sim2) {
            ggplotly(
              mizerShiny:::plotSpeciesWithTimeRange2(
                fishSimData()$sim1,
                fishSimData()$sim2,
                fishSimData()$unharv,
                chosen_year,
                mode = mode
              ) + scale_x_discrete(limits = ordered_species_reactive())
            )
          } else if (vis$show_sim1) {
            ggplotly(
              mizerShiny:::plotSpeciesWithTimeRange(
                fishSimData()$sim1,
                fishSimData()$unharv,
                chosen_year,
                mode = mode
              ) +
                scale_x_discrete(limits = ordered_species_reactive())
            )
          } else {
            ggplotly(
              mizerShiny:::plotSpeciesWithTimeRange(
                fishSimData()$sim2,
                fishSimData()$unharv,
                chosen_year,
                mode = mode
              ) +
                scale_x_discrete(limits = ordered_species_reactive())
            )
          }
        },
        last_plot_reactive = lastFishSpeciesPlot,
        condition = vis$show_sim1 || vis$show_sim2,
        context = "fishspeciesPlot"
      )
    })

    # Size plot ----
    output$fishsizePlot <- renderPlotly({
      req(fishSimData())

      vis <- mizerShiny:::getSimVisibility(input$sim_choice)

      mizerShiny:::generatePlotWithErrorHandling(
        plot_fun = function() {
          year <- fish_win1()

          if (vis$show_sim1 && vis$show_sim2) {
            g <- mizerShiny:::plotSpectraRelative2(
              fishSimData()$sim1,
              fishSimData()$unharv,
              fishSimData()$sim2,
              year,
              year
            )
          } else if (vis$show_sim1) {
            g <- mizerShiny:::plotSpectraRelative(
              fishSimData()$sim1,
              fishSimData()$unharv,
              year,
              year
            )
          } else {
            g <- mizerShiny:::plotSpectraRelative(
              fishSimData()$sim2,
              fishSimData()$unharv,
              year,
              year
            )
          }

          if (!isTRUE(input$logToggle4)) {
            g <- g + scale_x_continuous()
          }
          ggplotly(g)
        },
        last_plot_reactive = lastFishSizePlot,
        condition = vis$show_sim1 || vis$show_sim2,
        context = "fishsizePlot"
      )
    })

    # Guild plot ----
    output$fishguildPlot <- renderPlotly({
      req(fishSimData())
      validate(need(!is.null(guildparams), "Guild data not available"))
      chosen_year <- fish_win1()

      vis <- mizerShiny:::getSimVisibility(input$sim_choice)

      mizerShiny:::generatePlotWithErrorHandling(
        plot_fun = function() {
          mode <- mizerShiny:::getModeFromToggle(input$triplotToggleFish)

          if (vis$show_sim1 && vis$show_sim2) {
            ggplotly(
              mizerShiny:::guildplot_both(
                fishSimData()$sim1, fishSimData()$sim2, fishSimData()$unharv,
                chosen_year,
                guildparams, params,
                mode = mode
              )
            )
          } else if (vis$show_sim1) {
            ggplotly(
              mizerShiny:::guildplot(
                fishSimData()$sim1, fishSimData()$unharv,
                chosen_year,
                guildparams, params,
                mode = mode
              )
            )
          } else {
            ggplotly(
              mizerShiny:::guildplot(
                fishSimData()$sim2, fishSimData()$unharv,
                chosen_year,
                guildparams, params,
                mode = mode
              )
            )
          }
        },
        last_plot_reactive = lastFishGuildPlot,
        condition = vis$show_sim1 || vis$show_sim2,
        context = "fishguildPlot"
      )
    })

    # So that it saves the last plot, and remembers that the plot is already built
    built <- reactiveVal(FALSE)

    # Reset built when simulation data changes
    observe({
      fishSimData()
      built(FALSE)
    })

    # Reset built when simulation visibility switches change
    observe({
      input$sim_choice
      built(FALSE)
    })

    # Spectrum plot ----
    output$spectrumPlot <- renderPlotly({
      p <- tryCatch({
        sim <- fishSimData(); req(sim)
        selected_year <- fish_win1()
        log_toggle <- input$logToggle5

        df1 <- mizer::plotSpectra(sim$sim1,
                           time_range  = selected_year,
                           return_data = TRUE) |>
          mutate(sim = "Strategy 1")

        df2 <- mizer::plotSpectra(sim$sim2,
                           time_range  = selected_year,
                           return_data = TRUE) |>
          mutate(sim = "Strategy 2")

        all_species <- c()
        if (!is.null(df1)) all_species <- c(all_species, unique(df1$Species))
        if (!is.null(df2)) all_species <- c(all_species, unique(df2$Species))
        species <- sort(unique(all_species))

        maxn <- RColorBrewer::brewer.pal.info["Set3", "maxcolors"]
        if (length(species) <= maxn) {
          colors <- RColorBrewer::brewer.pal(length(species), "Set3")
        } else {
          colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(maxn, "Set3"))(length(species))
        }

        tmp <- plot_ly(source = "spec")
        for (sp in species) {
          i <- which(species == sp)

          if (!is.null(df1)) {
            sub1 <- df1 |> filter(Species == sp)
            if (nrow(sub1) > 0) {
              tmp <- add_lines(
                tmp,
                data        = sub1,
                x           = ~w,
                y           = ~value,
                name        = sp,
                legendgroup = sp,
                line        = list(color = colors[i], dash = "solid"),
                inherit     = FALSE
              )
            }
          }

          if (!is.null(df2)) {
            sub2 <- df2 |> filter(Species == sp)
            if (nrow(sub2) > 0) {
              tmp <- add_lines(
                tmp,
                data        = sub2,
                x           = ~w,
                y           = ~value,
                name        = sp,
                legendgroup = sp,
                line        = list(color = colors[i], dash = "dash"),
                inherit     = FALSE,
                showlegend  = FALSE
              )
            }
          }
        }

        axType <- if (isTRUE(log_toggle)) "log" else "linear"
        tmp <- layout(
          tmp,
          xaxis     = list(type = axType, title = "Weight"),
          yaxis     = list(type = "log", title = "Density"),
          hovermode = "closest"
        )
        tmp <- tmp |>
          event_register("plotly_restyle") |>
          event_register("plotly_legendclick") |>
          event_register("plotly_legenddoubleclick")

        lastSpectrumPlot(tmp)
        built(TRUE)
        tmp

      }, error = function(e) {
        mizerShiny:::logError("spectrumPlot", e)
        lastSpectrumPlot()
      })

      p
    })

    # Change the time plotted without having to replot everything
    observeEvent(
      fish_win1(),
      {
        req(built())

        tryCatch({
          selected_year <- fish_win1()
          sim <- fishSimData()

          df1 <- mizer::plotSpectra(sim$sim1,
                             time_range  = selected_year,
                             return_data = TRUE)
          spec1 <- split(df1, df1$Species)

          df2 <- mizer::plotSpectra(sim$sim2,
                             time_range  = selected_year,
                             return_data = TRUE)
          spec2 <- split(df2, df2$Species)

          species <- sort(unique(c(names(spec1), names(spec2))))
          px <- plotlyProxy(ns("spectrumPlot"), session)

          i <- 0L
          for (sp in species) {
            if (sp %in% names(spec1)) {
              sub1 <- spec1[[sp]]
              plotlyProxyInvoke(
                px, "restyle",
                list(x = list(sub1$w),
                     y = list(sub1$value)),
                list(i))
              i <- i + 1L
            }
            if (sp %in% names(spec2)) {
              sub2 <- spec2[[sp]]
              plotlyProxyInvoke(
                px, "restyle",
                list(x = list(sub2$w),
                     y = list(sub2$value)),
                list(i))
              i <- i + 1L
            }
          }
        }, error = function(e) {
          mizerShiny:::logError("spectrumPlot_proxy", e)
        })
      },
      ignoreInit = TRUE
    )

    # Diet plot ----
    output$fishdietsingleplot <- renderPlotly({
      req(fishSimData())

      win <- fish_win1()
      vis <- mizerShiny:::getSimVisibility(input$sim_choice)

      mizerShiny:::generatePlotWithErrorHandling(
        plot_fun = function() {
          # Check bounds
          if (!mizerShiny:::checkTimeRangeBounds(fishSimData()$sim1, win)) {
            details <- paste0("win = ", win, ", max_time = ", dim(fishSimData()$sim1@n)[1])
            stop(paste("Time range out of bounds:", details))
          }

          # Build sims list and names
          sims <- list()
          names <- c()

          if (vis$show_sim1) {
            sims <- c(sims, list(fishSimData()$sim1))
            names <- c(names, "Strategy 1")
          }
          if (vis$show_sim2) {
            sims <- c(sims, list(fishSimData()$sim2))
            names <- c(names, "Strategy 2")
          }

          # Subset simulations by time range
          harvest_sub <- lapply(sims, function(sim) {
            mizerShiny:::subsetSimByTimeRange(sim, win)
          })

          mizerShiny:::plotDietCompare(
            harvest_sub,
            species = input$fish_name_select,
            sim_names = names
          )
        },
        last_plot_reactive = lastFishDietSinglePlot,
        condition = vis$show_sim1 || vis$show_sim2,
        context = "fishdietsingleplot"
      )
    })

    # Nutrition plot ----
    output$nutritionplot <- renderPlotly({
      req(fishSimData())
      chosen_year <- fish_win1()

      vis <- mizerShiny:::getSimVisibility(input$sim_choice)

      mizerShiny:::generatePlotWithErrorHandling(
        plot_fun = function() {
          mode <- mizerShiny:::getModeFromToggle(input$triplotToggleFish)

          if (vis$show_sim1 && vis$show_sim2) {
            ggplotly(
              mizerShiny:::plotNutritionChange2(
                fishSimData()$sim1,
                fishSimData()$sim2,
                fishSimData()$unharv,
                chosen_year,
                mode = mode
              )
            )
          } else if (vis$show_sim1) {
            ggplotly(
              mizerShiny:::plotNutritionChange(
                fishSimData()$sim1,
                fishSimData()$unharv,
                chosen_year,
                mode = mode
              )
            )
          } else {
            ggplotly(
              mizerShiny:::plotNutritionChange(
                fishSimData()$sim2,
                fishSimData()$unharv,
                chosen_year,
                mode = mode
              )
            )
          }
        },
        last_plot_reactive = lastNutritionPlot,
        condition = vis$show_sim1 || vis$show_sim2,
        context = "nutritionplot"
      )
    })
  })
}

