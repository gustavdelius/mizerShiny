# Fishery Strategy Module
# Handles the Fishery Strategy tab UI and server logic

fishery_strategy_ui <- function(id, fish_max_year, have_guild_file, have_nutrition_file, app_exists) {
  ns <- NS(id)
  
  grid_container(
    layout    = c("area1 area0"),
    row_sizes = c("1fr"),
    col_sizes = c("0.3fr", "1.7fr"),
    gap_size  = "10px",
    
    grid_card(
      area = "area1",
      card_body(
        style = "margin-top: -0.5rem",
        sliderInput(
          inputId = ns("fishyear"),
          label   = "Time Range",
          min     = 1,
          max     = fish_max_year,
          value   = 5,
          step    = 1,
          width   = "100%"
        ) |> tagAppendAttributes(id = "fishyyear"),
        div(id   = ns("yearAdjustButtons_fish"),
            style = "display:flex; justify-content:center; gap:10px;",
            actionButton(ns("decYear_fish"), "-1 year", class = "btn-small"),
            actionButton(ns("incYear_fish"), "+1 year", class = "btn-small")
        ),
        div(style = "margin: 4px 0  0; padding: 6px 8px 0 8px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
            div(style = "display: flex; align-items: center; gap: 8px;",
                HTML("<span style='font-weight:500; font-size: 0.9em; color: var(--bs-heading-color);'>Show:</span>"),
                radioButtons(
                  inputId = ns("sim_choice"),
                  label   = NULL,
                  choices = c("Sim 1" = "sim1", "Sim 2" = "sim2", "Both" = "both"),
                  selected = "sim1",
                  inline = TRUE
                )
            )
        ),
        tabsetPanel(
          tabPanel(
            title = "Sim 1",
            div(id = "fishery_sliders", uiOutput(ns("fishery_sliders_ui")))
          ),
          tabPanel(
            title = "Sim 2",
            div(id = "fishery_sliders", uiOutput(ns("fishery_sliders_ui2")))
          )
        )
      )
    ),
    
    grid_card(
      area = "area0",
      card_body(
        div(
          class = "plot-card",
          style = "flex: 4.5; height:50vh; display:flex; flex-direction:column; overflow: hidden; margin-top: -0.5rem",
          tabsetPanel(
            id = ns("fishy_plots"),
            tabPanel(
              title = "Biomass",
              div(style = "flex:1; display:flex;",
                  plotlyOutput(ns("fishspeciesPlot"), height = "100%", width = "100%")
              )
            ),
            tabPanel(
              title = "Yield Composition",
              div(style = "flex:1; display:flex;",
                  plotlyOutput(ns("yieldPlot"), height = "100%", width = "100%")
              )
            ),
            tabPanel(
              title = "Size",
              div(style = "flex:1; display:flex;",
                  plotlyOutput(ns("fishsizePlot"), height = "100%", width = "100%")
              )
            ),
            if (have_guild_file) {
              tabPanel(
                title = "Guild",
                div(style = "flex:1; display:flex;",
                    plotlyOutput(ns("fishguildPlot"), height = "100%", width = "100%")
                )
              )
            },
            tabPanel(
              title = "Spectra",
              div(style = "flex:1; display:flex;",
                  plotlyOutput(ns("spectrumPlot"), height = "100%", width = "100%")
              )
            ),
            tabPanel(
              title = "Diet",
              div(style = "height:50vh; display:flex;",
                  plotlyOutput(ns("fishdietsingleplot"), height = "100%", width = "100%")
              )
            ),
            if (have_nutrition_file) {
              tabPanel(
                title = "Nutrition",
                div(style = "flex:1; display:flex;",
                    plotlyOutput(ns("nutritionplot"), height = "100%", width = "100%")
                )
              )
            }
          )
        )
      ),
      
      card_body(
        style = "flex: auto",
        conditionalPanel(
          condition = paste0("input['", ns("fishy_plots"), "'] == 'Yield Composition'"),
          div(style = "margin-bottom:1.5rem;",
              mizerShiny:::legendUI(ns("infoButtonOrder"), legends$fishery_yield)
          ),
          div(style = "display: flex; align-items: center; gap: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
              HTML("<span style='margin-top:0px; margin-bottom:0.5rem; font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Yield Time Range</span>"),
              sliderInput(
                inputId = ns("fishyear2_yield"),
                label   = NULL,
                min     = 1,
                max     = fish_max_year,
                value   = c(1, 10),
                step    = 1,
                width   = "200px"
              ) |> tagAppendAttributes(id = "fishyyear_yield")
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("fishy_plots"), "'] == 'Biomass'"),
          div(style = "display: flex; align-items: center; gap: 15px; flex-wrap: wrap;",
              div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                  mizerShiny:::legendUI(ns("infoButtonOrder"), legends$fishery_species)
              ),
              div(style = "display: flex; align-items: center; gap: 10px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
                  HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Species Order:</span>"),
                  selectInput(
                    inputId = ns("species_order_fish"),
                    label   = NULL,
                    choices = c("Custom", "Size", "Guild"),
                    width = "120px"
                  ),
                  HTML(
                    "<button id='infoButtonOrder' class='btn btn-info btn-xs' type='button' data-bs-toggle='popover' title='' data-bs-content='Select how you want the species to be ordered on the axis. Options include &quot;Custom&quot;, &quot;Size&quot; and &quot;Guild&quot;. Click the &quot;customise&quot; button to change the custom order.'><strong>?</strong></button>"
                  ),
                  actionButton(
                    ns("customOrderInfo_fish"),
                    label = HTML("<strong>customise</strong>"),
                    class = "btn btn-info btn-xs no-focus-outline"
                  )
              ),
              div(style = "display: flex; align-items: center; gap: 10px; padding: 10px; background-color: #f3e5f5; border-radius: 5px; border: 1px solid #e1bee7;",
                  materialSwitch(
                    inputId = ns("triplotToggleFish"),
                    label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show intermediate years</span>"),
                    value   = TRUE,
                    status  = "info"
                  )
              )
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("fishy_plots"), "'] == 'Size'"),
          div(style = "display: flex; align-items: center; gap: 15px;",
              div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                  mizerShiny:::legendUI(ns("infoButtonOrder"), legends$fishery_size)
              ),
              div(style = "padding: 10px; background-color: #e8f5e8; border-radius: 5px; border: 1px solid #c8e6c9;",
                  materialSwitch(
                    inputId = ns("logToggle4"),
                    label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Log</span>"),
                    value   = TRUE,
                    status  = "info"
                  )
              )
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("fishy_plots"), "'] == 'Guild'"),
          div(style = "display: flex; align-items: center; gap: 15px;",
              div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                  mizerShiny:::legendUI(ns("infoButtonOrder"), legends$fishery_guild)
              ),
              div(style = "padding: 10px; background-color: #fff3e0; border-radius: 5px; border: 1px solid #ffcc80;",
                  materialSwitch(
                    inputId = ns("triguildToggleFish"),
                    label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show intermediate years</span>"),
                    value   = TRUE,
                    status  = "info"
                  )
              )
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("fishy_plots"), "'] == 'Spectra'"),
          div(style = "display: flex; align-items: center; gap: 15px;",
              div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                  mizerShiny:::legendUI(ns("infoButtonOrder"), legends$fishery_spectra)
              ),
              div(style = "padding: 10px; background-color: #fce4ec; border-radius: 5px; border: 1px solid #f8bbd9;",
                  materialSwitch(
                    inputId = ns("logToggle5"),
                    label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Log</span>"),
                    value   = TRUE,
                    status  = "info"
                  )
              )
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("fishy_plots"), "'] == 'Diet'"),
          div(style = "display: flex; align-items: center; gap: 15px;",
              div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                  mizerShiny:::legendUI(ns("infoButtonOrder"), legends$fishery_diet_single)
              ),
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
        conditionalPanel(
          condition = paste0("input['", ns("fishy_plots"), "'] == 'Nutrition'"),
          div(style = "display: flex; align-items: center; gap: 15px;",
              div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                  mizerShiny:::legendUI(ns("infoButtonOrder"), legends$nutrition)
              )
          )
        )
      )
    )
  )
}

fishery_strategy_server <- function(id, default_params, unfishedprojection,
                                   guildparams, ordered_species_reactive, species_list,
                                   fish_max_year) {
  moduleServer(id, function(input, output, session) {
    
    # Update species select input
    observe({
      updateSelectInput(session, "fish_name_select", choices = species_list())
    })
    
    # Dynamic fishery effort sliders for Sim 1
    output$fishery_sliders_ui <- renderUI({
      effort <- default_params@initial_effort
      gears <- unique(default_params@gear_params$gear)
      slider_list <- lapply(gears, function(gear) {
        sliderInput(
          inputId = session$ns(paste0("effort_", gear)),
          label = paste("Effort for", gear),
          min = 0,
          max = if(default_params@initial_effort[gear]==0){
            2
          }else(default_params@initial_effort[gear]*2),
          value = default_params@initial_effort[gear],
          step = 0.05,
          width = "100%"
        )
      })
      div(id = "fishery_sliders", slider_list)
    })
    
    # Dynamic fishery effort sliders for Sim 2
    output$fishery_sliders_ui2 <- renderUI({
      effort <- default_params@initial_effort
      gears <- unique(default_params@gear_params$gear)
      slider_list <- lapply(gears, function(gear) {
        sliderInput(
          inputId = session$ns(paste0("effort2_", gear)),
          label = paste("Effort for", gear),
          min = 0,
          max = if(default_params@initial_effort[gear]==0){
            2
          }else(default_params@initial_effort[gear]*2),
          value = default_params@initial_effort[gear],
          step = 0.05,
          width = "100%"
        )
      })
      div(id = "fishery_sliders", slider_list)
    })
    
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
    
    # Observer to automatically switch to "Both" when Sim 2 sliders are changed
    observeEvent({
      gears <- unique(default_params@gear_params$gear)
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
      sim1 = unfishedprojection,
      sim2 = NULL,
      unharv = unfishedprojection
    ))
    
    # Reactive observer that runs when time range changes
    observe({
      sims <- isolate(fishSimData())
      max_year <- dim(sims$sim1@n)[1] - 1
      if (input$fishyear <= max_year) return()
      
      pb <- shiny::Progress$new(); on.exit(pb$close())
      total_steps <- 3
      pb$set(message = "Running simulation …", value = 0)
      
      t_max <- input$fishyear - max_year + 5
      pb$inc(1/total_steps, "Projecting …")
      sim1 <- project(sims$sim1, t_max = t_max)
      
      pb$inc(1/total_steps, "Projecting …")
      sim2 <- if (!is.null(sims$sim2)) project(sims$sim2, t_max = t_max) else NULL
      
      pb$inc(1/total_steps, "Done")
      
      fishSimData(list(sim1 = sim1,
                      sim2 = sim2,
                      unharv = unfishedprojection))
    })
    
    # Reactive observer that runs simulation when inputs change
    observe({
      input$fishyear
      
      gears <- unique(default_params@gear_params$gear)
      effort1 <- makeEffort("effort_" , gears, default_params@initial_effort)
      effort2 <- makeEffort("effort2_", gears, default_params@initial_effort)
      
      max_year <- input$fishyear
      
      pb <- shiny::Progress$new(); on.exit(pb$close())
      total_steps <- 3
      pb$set(message = "Running fishery simulation …", value = 0)
      
      pb$inc(1/total_steps, "Projecting Sim 1 …")
      sim1 <- project(
        default_params, effort = effort1,
        t_max   = max_year * 2 + 2
      )
      
      pb$inc(1/total_steps, "Projecting Sim 2 …")
      sim2 <- project(
        default_params, effort = effort2,
        t_max   = max_year * 2 + 2
      )
      
      pb$inc(1/total_steps, "Done")
      
      fishSimData(list(sim1   = sim1,
                      sim2   = sim2,
                      unharv = unfishedprojection))
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
    lastYieldPlot             <- reactiveVal(NULL)
    lastFishSpeciesPlot       <- reactiveVal(NULL)
    lastFishSizePlot          <- reactiveVal(NULL)
    lastFishGuildPlot         <- reactiveVal(NULL)
    lastSpectrumPlot          <- reactiveVal(NULL)
    lastFishDietSinglePlot    <- reactiveVal(NULL)
    lastNutritionPlot         <- reactiveVal(NULL)
    
    # Plots
    output$yieldPlot <- renderPlotly({
      req(fishSimData())
      
      sims <- list()
      if (input$sim_choice == "sim1" || input$sim_choice == "both") {
        sims <- c(sims, list(fishSimData()$sim1))
      }
      if (input$sim_choice == "sim2" || input$sim_choice == "both") {
        sims <- c(sims, list(fishSimData()$sim2))
      }
      
      if (length(sims) == 0) {
        return(lastYieldPlot())
      }
      
      p <- tryCatch({
        generateYieldDashboard(
          NS_sim          = sims,
          highlight_times = input$fishyear2_yield,
          params          = default_params
        )
      },
      error = function(e) {
        lastYieldPlot()
      })
      
      lastYieldPlot(p)
      p
    })
    
    output$fishspeciesPlot <- renderPlotly({
      req(fishSimData())
      chosen_year <- fish_win1()
      
      show_sim1 <- input$sim_choice == "sim1" || input$sim_choice == "both"
      show_sim2 <- input$sim_choice == "sim2" || input$sim_choice == "both"
      
      if (!show_sim1 && !show_sim2) {
        return(lastFishSpeciesPlot())
      }
      
      p <- tryCatch({
        if (show_sim1 && show_sim2) {
          ggplotly(
            plotSpeciesWithTimeRange2(
              fishSimData()$sim1,
              fishSimData()$sim2,
              fishSimData()$unharv,
              chosen_year,
              mode = if (isTRUE(input$triplotToggleFish)) "triple" else "chosen"
            ) + scale_x_discrete(limits = ordered_species_reactive())
          )
        } else if (show_sim1) {
          modeFish <- if (isTRUE(input$triplotToggleFish)) "triple" else "chosen"
          ggplotly(
            plotSpeciesWithTimeRange(
              fishSimData()$sim1,
              fishSimData()$unharv,
              chosen_year,
              mode = modeFish
            ) +
              scale_x_discrete(limits = ordered_species_reactive())
          )
        } else {
          modeFish <- if (isTRUE(input$triplotToggleFish)) "triple" else "chosen"
          ggplotly(
            plotSpeciesWithTimeRange(
              fishSimData()$sim2,
              fishSimData()$unharv,
              chosen_year,
              mode = modeFish
            ) +
              scale_x_discrete(limits = ordered_species_reactive())
          )
        }
      },
      error = function(e) {
        lastFishSpeciesPlot()
      })
      
      lastFishSpeciesPlot(p)
      p
    })
    
    output$fishsizePlot <- renderPlotly({
      req(fishSimData())
      
      show_sim1 <- input$sim_choice == "sim1" || input$sim_choice == "both"
      show_sim2 <- input$sim_choice == "sim2" || input$sim_choice == "both"
      
      if (!show_sim1 && !show_sim2) {
        return(lastFishSizePlot())
      }
      
      p <- tryCatch({
        if (show_sim1 && show_sim2) {
          g <- plotSpectraRelative2(
            fishSimData()$sim1,
            fishSimData()$unharv,
            fishSimData()$sim2,
            fish_win1(),
            fish_win1()
          )
          if (!isTRUE(input$logToggle4)) {
            g <- g + scale_x_continuous()
          }
          ggplotly(g)
        } else if (show_sim1) {
          g <- plotSpectraRelative(
            fishSimData()$sim1,
            fishSimData()$unharv,
            fish_win1(),
            fish_win1()
          )
          if (!isTRUE(input$logToggle4)) {
            g <- g + scale_x_continuous()
          }
          ggplotly(g)
        } else {
          g <- plotSpectraRelative(
            fishSimData()$sim2,
            fishSimData()$unharv,
            fish_win1(),
            fish_win1()
          )
          if (!isTRUE(input$logToggle4)) {
            g <- g + scale_x_continuous()
          }
          ggplotly(g)
        }
      }, error = function(e) {
        lastFishSizePlot()
      })
      
      lastFishSizePlot(p)
      p
    })
    
    output$fishguildPlot <- renderPlotly({
      req(fishSimData())
      validate(need(!is.null(guildparams), "Guild data not available"))
      chosen_year <- fish_win1()
      
      show_sim1 <- input$sim_choice == "sim1" || input$sim_choice == "both"
      show_sim2 <- input$sim_choice == "sim2" || input$sim_choice == "both"
      
      if (!show_sim1 && !show_sim2) {
        return(lastFishGuildPlot())
      }
      
      modeGuild <- if (isTRUE(input$triguildToggleFish)) "triple" else "chosen"
      
      p <- tryCatch({
        if (show_sim1 && show_sim2) {
          ggplotly(
            guildplot_both(
              fishSimData()$sim1, fishSimData()$sim2, fishSimData()$unharv,
              chosen_year,
              guildparams, default_params,
              mode = modeGuild
            )
          )
        } else if (show_sim1) {
          ggplotly(
            guildplot(
              fishSimData()$sim1, fishSimData()$unharv,
              chosen_year,
              guildparams, default_params,
              mode = modeGuild
            )
          )
        } else {
          ggplotly(
            guildplot(
              fishSimData()$sim2, fishSimData()$unharv,
              chosen_year,
              guildparams, default_params,
              mode = modeGuild
            )
          )
        }
      },
      error = function(e) {
        lastFishGuildPlot()
      })
      
      lastFishGuildPlot(p)
      p
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
    
    output$spectrumPlot <- renderPlotly({
      p <- tryCatch({
        sim <- fishSimData(); req(sim)
        selected_year <- fish_win1()
        log_toggle <- input$logToggle5
        
        df1 <- mizer::plotSpectra(sim$sim1,
                           time_range  = selected_year,
                           return_data = TRUE) |>
          mutate(sim = "Sim 1")
        
        df2 <- mizer::plotSpectra(sim$sim2,
                           time_range  = selected_year,
                           return_data = TRUE) |>
          mutate(sim = "Sim 2")
        
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
        message("Spectrum build failed: ", e$message)
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
          message("Spectrum proxy update failed: ", e$message)
        })
      },
      ignoreInit = TRUE
    )
    
    output$fishdietsingleplot <- renderPlotly({
      req(fishSimData())
      
      win <- fish_win1()
      
      show_sim1 <- input$sim_choice == "sim1" || input$sim_choice == "both"
      show_sim2 <- input$sim_choice == "sim2" || input$sim_choice == "both"
      
      if (!show_sim1 && !show_sim2) {
        return(lastFishDietSinglePlot())
      }
      
      sims <- list()
      names <- c()
      
      if (show_sim1) {
        sims <- c(sims, list(fishSimData()$sim1))
        names <- c(names, "Sim 1")
      }
      if (show_sim2) {
        sims <- c(sims, list(fishSimData()$sim2))
        names <- c(names, "Sim 2")
      }
      
      p <- tryCatch({
        sim_dims <- dim(fishSimData()$sim1@n)
        if (win > sim_dims[1]) {
          message("DEBUG: Fishery time range out of bounds")
          message("DEBUG: win = ", win)
          message("DEBUG: sim_dims[1] = ", sim_dims[1])
          return(lastFishDietSinglePlot())
        }
        
        harvest_sub <- lapply(sims, function(p) {
          tryCatch({
            p@n       <- p@n      [win, , , drop = FALSE]
            p@n_pp    <- p@n_pp   [win, ,      drop = FALSE]
            p@n_other <- p@n_other[win, ,      drop = FALSE]
            p
          }, error = function(e) {
            message("DEBUG: Error in fishery diet plot subsetting: ", e$message)
            message("DEBUG: win = ", win)
            message("DEBUG: p@n dims = ", paste(dim(p@n), collapse = "x"))
            stop(e)
          })
        })
        
        plotDietCompare(harvest_sub, species = input$fish_name_select,
                        sim_names = names
        )
        
      },
      error = function(e) {
        message("DEBUG: Fishery diet plot error: ", e$message)
        lastFishDietSinglePlot()
      })
      
      lastFishDietSinglePlot(p)
      p
    })
    
    output$nutritionplot <- renderPlotly({
      req(fishSimData())
      
      win <- fish_win1()
      
      show_sim1 <- input$sim_choice == "sim1" || input$sim_choice == "both"
      show_sim2 <- input$sim_choice == "sim2" || input$sim_choice == "both"
      
      if (!show_sim1 && !show_sim2) {
        return(lastNutritionPlot())
      }
      
      sims <- list()
      
      if (show_sim1) {
        sims <- c(sims, list(fishSimData()$sim1))
      }
      if (show_sim2) {
        sims <- c(sims, list(fishSimData()$sim2))
      }
      
      p <- tryCatch({
        ggplotly(
          plotNutrition(sims, fishSimData()$unharv ,win)
        )
      },
      error = function(e) {
        lastNutritionPlot()
      })
      
      lastNutritionPlot(p)
      p
    })
  })
}

