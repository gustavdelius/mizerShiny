# Species Role Module
# Handles the Species Role tab UI and server logic

species_role_ui <- function(id, sp_max_year, have_guild_file, app_exists) {
  ns <- NS(id)
  
  grid_container(
    layout    = c("area1 area0"),
    row_sizes = c("1fr"),
    col_sizes = c("0.3fr", "1.7fr"),
    gap_size  = "10px",
    
    grid_card(
      area = "area1",
      card_body(
        selectInput(
          inputId = ns("species_name_select"),
          label   = "Select a Species:",
          choices = NULL
        ) |> tagAppendAttributes(id = "species_chose"),
        sliderInput(
          inputId = ns("species"),
          label   = HTML(
            "% Change in Biomass <button id='infoButtonSpecies' class='btn btn-info btn-xs' type='button' \
            data-bs-toggle='popover' title='' \
            data-bs-content='Slider value indicates the percentage change in starting biomass of the species. Example: to increase the starting population of a given species by 20%, set value on the slider to 20. To decrease by 20%, set value to -20.'>\
            <strong>?</strong></button>"
          ),
          min   = -100,
          max   = 100,
          value = 0,
          step  = 1,
          width = "100%"
        ) |> tagAppendAttributes(id = "species_slider"),
        sliderInput(
          inputId = ns("mortspecies"),
          label   = HTML(
            "% Change in Mortality<button id='infoButtonMort' class='btn btn-info btn-xs' type='button' \
            data-bs-toggle='popover' title='' \
            data-bs-content='Slider value indicates the change in mortality of a species. Example: to increase the mortality of a species by 10%, set the value of the slider to 10. This will change the mortality throughout the simulation to be 1% higher. If you want it to be a 1% decrease, set value to -1'>\
            <strong>?</strong></button>"
          ),
          min   = -25,
          max   = 25,
          value = 0,
          step  = 1,
          width = "100%"
        ) |> tagAppendAttributes(id = "mort_slider"),
        sliderInput(
          inputId = ns("year"),
          label   = "Time Range",
          min     = 1,
          max     = sp_max_year,
          value   = 5,
          step    = 1,
          width   = "100%"
        ) |> tagAppendAttributes(id = "yearspecies_slider"),
        div(id   = ns("yearAdjustButtons_bio"),
            style = "display:flex; justify-content:center; gap:10px;",
            actionButton(ns("decYear_bio"), "-1 year", class = "btn-small"),
            actionButton(ns("incYear_bio"), "+1 year", class = "btn-small")
        )
      )
    ),
    
    grid_card(
      area = "area0",
      
      card_body(
        class = "plot-card",
        style = "flex: 4; overflow: hidden; margin-top: -0.5rem",
        tabsetPanel(
          id = ns("plotTabs"),
          tabPanel(title = "Biomass", plotlyOutput(ns("speciesPlot"), height = "55vh")),
          tabPanel(title = "Size",     plotlyOutput(ns("sizePlot"),     height = "55vh")),
          if (have_guild_file) {
            tabPanel(title = "Guilds",   plotlyOutput(ns("guildPlot"),    height = "55vh"))
          },
          tabPanel(title = "Diet",
                   div(style = "height:50vh; display:flex;",
                       plotlyOutput(ns("dietplot"), height = "100%", width = "100%")
                   ))
        )
      ),
      
      card_body(
        style = "flex: 1.46;",
        
        conditionalPanel(
          condition = paste0("input['", ns("plotTabs"), "'] == 'Biomass'"),
          div(style = "display: flex; align-items: center; gap: 15px; flex-wrap: wrap;",
              div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                  mizerShiny:::legendUI(ns("infoButtonOrder"), legends$biomass_species)
              ),
              div(style = "display: flex; align-items: center; gap: 10px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
                  HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Species Order:</span>"),
                  selectInput(
                    inputId = ns("species_order_bio"),
                    label   = NULL,
                    choices = c("Custom", "Size", "Guild"),
                    width = "120px"
                  ),
                  HTML(
                    "<button id='infoButtonOrder' class='btn btn-info btn-xs' type='button' data-bs-toggle='popover' title='' data-bs-content='Select how you want the species to be ordered on the axis. Options include &quot;Custom&quot;, &quot;Size&quot; and &quot;Guild&quot;. Click the &quot;customise&quot; button to change the custom order.'><strong>?</strong></button>"
                  ),
                  actionButton(
                    ns("customOrderInfo_bio"),
                    label = HTML("<strong>customise</strong>"),
                    class = "btn btn-info btn-xs no-focus-outline"
                  )
              ),
              div(style = "display: flex; align-items: center; gap: 10px; padding: 10px; background-color: #f3e5f5; border-radius: 5px; border: 1px solid #e1bee7;",
                  materialSwitch(
                    inputId = ns("triplotToggle"),
                    label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show intermediate years</span>"),
                    value   = TRUE,
                    status  = "info"
                  )
              )
          )
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("plotTabs"), "'] == 'Size'"),
          div(style = "display: flex; align-items: center; gap: 15px;",
              div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                  mizerShiny:::legendUI(ns("infoButtonOrder"), legends$biomass_size)
              ),
              div(style = "padding: 10px; background-color: #e8f5e8; border-radius: 5px; border: 1px solid #c8e6c9;",
                  materialSwitch(
                    inputId = ns("logToggle"),
                    label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Log</span>"),
                    value   = TRUE,
                    status  = "info"
                  )
              )
          )
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("plotTabs"), "'] == 'Guilds'"),
          div(style = "display: flex; align-items: center; gap: 15px;",
              div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                  mizerShiny:::legendUI(ns("infoButtonOrder"), legends$biomass_guild)
              ),
              div(style = "padding: 10px; background-color: #fff3e0; border-radius: 5px; border: 1px solid #ffcc80;",
                  materialSwitch(
                    inputId = ns("triguildToggle"),
                    label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show intermediate years</span>"),
                    value   = TRUE,
                    status  = "info"
                  )
              )
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("plotTabs"), "'] == 'Diet'"),
          div(style = "display: flex; align-items: center; gap: 15px;",
              div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                  mizerShiny:::legendUI(ns("infoButtonDietBio"), legends$fishery_diet_single)
              ),
              div(style = "display: flex; align-items: center; gap: 10px; padding: 10px; background-color: #e0f2f1; border-radius: 5px; border: 1px solid #b2dfdb;",
                  HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Select a Species:</span>"),
                  selectInput(
                    inputId = ns("diet_species_select"),
                    label   = NULL,
                    choices = NULL,
                    width = "200px"
                  )
              )
          )
        )
      )
    )
  )
}

species_role_server <- function(id, default_params, unharvestedprojection, 
                                guildparams, ordered_species_reactive, species_list) {
  moduleServer(id, function(input, output, session) {
    
    # Setup year controls
    mizerShiny:::setupYearControls(
      input, session,
      sliderId = "year",
      minusId  = "decYear_bio",
      plusId   = "incYear_bio"
    )
    
    # Update species select inputs
    observe({
      updateSelectInput(session, "species_name_select", choices = species_list())
      updateSelectInput(session, "diet_species_select", choices = species_list())
    })
    
    # Initialize species selection when app starts
    observe({
      req(species_list())
      if (length(species_list()) > 0 && (is.null(input$species_name_select) || input$species_name_select == "")) {
        updateSelectInput(session, "species_name_select", selected = species_list()[1])
      }
    })
    
    # Reset sliders when species selection changes
    observe({
      req(input$species_name_select)
      updateSliderInput(session, "species", value = 0)
      updateSliderInput(session, "mortspecies", value = 0)
    })
    
    # Initialize bioSimData as a reactive value
    bioSimData <- reactiveVal(list(
      harvested = unharvestedprojection,
      unharvested = unharvestedprojection
    ))
    
    # Reactive observer that runs when time range changes
    # It checks whether the simulation needs to be extended
    observe({
      sims <- isolate(bioSimData())
      max_year <- dim(sims$harvested@n)[1] - 1
      if (input$year <= max_year) return()  # No need to re-run if within bounds
      
      pb <- shiny::Progress$new(); on.exit(pb$close())
      total_steps <- 3
      pb$set(message = "Running simulation …", value = 0)
      
      t_max <- input$year - max_year + 5  # Extend by 5 years
      pb$inc(1/total_steps, "Projecting …")
      unharvested <- mizerShiny:::runSimulationWithErrorHandling(
        function() project(sims$unharvested, t_max = t_max),
        context = "species_role_time_range_unharvested"
      )
      
      pb$inc(1/total_steps, "Projecting …")
      harvested <- mizerShiny:::runSimulationWithErrorHandling(
        function() project(sims$harvested, t_max = t_max),
        context = "species_role_time_range_harvested"
      )
      
      pb$inc(1/total_steps, "Done")
      
      # Update the reactive value
      bioSimData(list(harvested = harvested,
                      unharvested = unharvestedprojection))
    })
    
    # Reactive observer that runs simulation when inputs change
    observe({
      req(input$species_name_select)
      # Trigger on changes to these inputs
      input$species
      input$mortspecies
      input$species_name_select
      
      changed_params <- default_params
      
      pb <- shiny::Progress$new(); on.exit(pb$close())
      total_steps <- 4
      pb$set(message = "Running simulation …", value = 0)
      
      pb$inc(1/total_steps, "Adjusting biomass …")
      
      changed_params@initial_n[input$species_name_select, ] <-
        default_params@initial_n[input$species_name_select, ] * (1 + input$species / 100)
      
      pb$inc(1/total_steps, "Updating mortality …")
      extmort   <- getExtMort(default_params)
      totalmort <- getMort(default_params)
      
      extmort[input$species_name_select, ] <-
        extmort[input$species_name_select, ] +
        (input$mortspecies / 100) * totalmort[input$species_name_select, ]
      
      ext_mort(changed_params) <- extmort
      
      pb$inc(1/total_steps, "Projecting …")
      harvested <- mizerShiny:::runSimulationWithErrorHandling(
        function() project(
          changed_params,
          # We run the simulation for an extra 5 years so that the +1 button
          # does not immediately trigger a re-run.
          t_max  = isolate(input$year) + 5
        ),
        context = "species_role_simulation"
      )
      
      pb$inc(1/total_steps, "Done")
      
      # Update the reactive value
      bioSimData(list(harvested = harvested,
                      unharvested = unharvestedprojection))
    })
    
    # Store last successful plots for error recovery
    lastBioSpeciesPlot <- mizerShiny:::createLastPlotReactive()
    lastBioSizePlot    <- mizerShiny:::createLastPlotReactive()
    lastBioGuildPlot   <- mizerShiny:::createLastPlotReactive()
    lastDietPlot       <- mizerShiny:::createLastPlotReactive()
    
    # Output plots
    output$speciesPlot <- renderPlotly({
      req(bioSimData())
      chosen_year <- input$year
      
      mizerShiny:::generatePlotWithErrorHandling(
        plot_fun = function() {
          mode <- mizerShiny:::getModeFromToggle(input$triplotToggle)
          ggplotly(
            mizerShiny:::plotSpeciesWithTimeRange(
              bioSimData()$harvested,
              bioSimData()$unharvested,
              chosen_year,
              mode = mode
            ) +
              scale_x_discrete(limits = ordered_species_reactive())
          )
        },
        last_plot_reactive = lastBioSpeciesPlot,
        context = "speciesPlot"
      )
    })
    
    output$sizePlot <- renderPlotly({
      req(bioSimData())
      t1 <- max(input$year - 1, 1)
      t2 <- input$year + 1
      
      mizerShiny:::generatePlotWithErrorHandling(
        plot_fun = function() {
          g <- mizerShiny:::plotSpectraRelative(
            bioSimData()$harvested,
            bioSimData()$unharvested,
            t1, t2
          )
          if (!isTRUE(input$logToggle)) {
            g <- g + scale_x_continuous()
          }
          ggplotly(g)
        },
        last_plot_reactive = lastBioSizePlot,
        context = "sizePlot"
      )
    })
    
    output$guildPlot <- renderPlotly({
      req(bioSimData())
      validate(need(!is.null(guildparams), "Guild data not available"))
      chosen_year <- input$year
      
      mizerShiny:::generatePlotWithErrorHandling(
        plot_fun = function() {
          mode <- mizerShiny:::getModeFromToggle(input$triguildToggle)
          ggplotly(
            mizerShiny:::guildplot(
              bioSimData()$harvested, bioSimData()$unharvested,
              chosen_year,
              guildparams, default_params,
              mode = mode
            )
          )
        },
        last_plot_reactive = lastBioGuildPlot,
        context = "guildPlot"
      )
    })
    
    output$dietplot <- renderPlotly({
      req(bioSimData())
      win <- list(start = max(input$year - 1, 1), end = input$year + 1)
      sims <- list(bioSimData()$harvested, bioSimData()$unharvested)
      
      mizerShiny:::generatePlotWithErrorHandling(
        plot_fun = function() {
          # Add bounds checking for time range
          if (!mizerShiny:::checkTimeRangeBounds(bioSimData()$harvested, win)) {
            details <- paste0("win$start = ", win$start, ", win$end = ", win$end,
                            ", max_time = ", dim(bioSimData()$harvested@n)[1])
            stop(paste("Time range out of bounds:", details))
          }
          
          # Subset simulations by time range
          harvest_sub <- lapply(sims, function(sim) {
            mizerShiny:::subsetSimByTimeRange(sim, win)
          })
          
          mizerShiny:::plotDietCompare(
            harvest_sub,
            species   = input$diet_species_select,
            sim_names = c("Your Sim", "Base Sim")
          )
        },
        last_plot_reactive = lastDietPlot,
        context = "dietplot"
      )
    })
  })
}

