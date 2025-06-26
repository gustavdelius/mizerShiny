library(mizerShiny)
library(shiny)
library(mizer)
library(ggplot2)
library(bslib)
library(plotly)
library(ggplot2)
library(gridlayout)
library(thematic)
library(tidyverse)
library(forcats)
library(shinyBS)
library(rintrojs)
library(patchwork)
library(here)
library(sortable)
library(shinyjs)
library(shinyWidgets)
library(dplyr)

default_params <- getShinyOption("default_params")
if (is.null(default_params)) default_params <- mizerShiny::default_params

unharvestedprojection <- project(default_params, t_max = 12)
unfishedprojection <- project(default_params, t_max = 12)

#Find years for the Time Range
sp_max_year   <- max(16, floor((dim(unharvestedprojection@n)[1] - 2) / 2))
fish_max_year <- max(16, floor((dim(unfishedprojection@n)[1] - 2) / 2))


# Load guild & nutrition data ----

# Functions to help load in files
app_path <- function(...) {
  p <- system.file("app", ..., package = "mizerShiny")
  if (p == "") p <- file.path("inst", "app", ...)
  p
}
app_exists <- function(...) file.exists(app_path(...))

guild_file <- app_path("Including", "guilds_information", "checkGuilds",
                       "guildparams_preprocessed.Rdata")
have_guild_file <- file.exists(guild_file)
if (have_guild_file) load(guild_file, envir = .GlobalEnv)

nut_file <- app_path("Including", "Nutrition", "checkNutrition", "nutrition.csv")
have_nutrition_file <- file.exists(nut_file)
#Loading in text for legends and the code to generate them
source(app_path("www", "legendsTXT.R"), local = TRUE)
source(app_path("Functions", "legendUI.R"), local = TRUE)

# Server ----
server <- function(input, output, session) {

  # Global error handler for debugging
  options(error = function() {
    message("DEBUG: Global error caught!")
    message("DEBUG: Error message: ", geterrmessage())
    message("DEBUG: Call stack:")
    print(sys.calls())
  })

  # loading the introduction/guide ----
  source("tutorial_steps/get_intro_steps.R")
  observeEvent(input$start_tutorial, {
    steps <- get_intro_steps(input)

    if (length(steps) > 0) {
      introjs(session, options = list(steps = steps))
    }
  })

  #changing the species options to dynamically change depending on the model
  species_list <- reactive({
    species <- setdiff(unique(default_params@species_params$species),
            ("Resource"))
    species
  })
  species_input_ids <- c(
    "species_name_select",
    "fish_name_select",
    "diet_species_select"
  )
  observe({
    lapply(species_input_ids, function(id) {
      updateSelectInput(session, id, choices = species_list())
    })
  })

  # Initialize species selection when app starts
  observe({
    req(species_list())
    if (length(species_list()) > 0 && (is.null(input$species_name_select) || input$species_name_select == "")) {
      updateSelectInput(session, "species_name_select", selected = species_list()[1])
    }
  })

  #changing the fishery options to dynamically change depending on the model
  output$fishery_sliders_ui <- renderUI({
    effort <- default_params@initial_effort
    gears <- unique(default_params@gear_params$gear)
    slider_list <- lapply(gears, function(gear) {
      sliderInput(
        inputId = paste0("effort_", gear),  # e.g., "effort_total"
        label = paste("Effort for", gear),
        min = 0,
        #if its 0, it needs a different way.
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
  # for both sections
  output$fishery_sliders_ui2 <- renderUI({
    effort <- default_params@initial_effort
    gears <- unique(default_params@gear_params$gear)
    slider_list <- lapply(gears, function(gear) {
      sliderInput(
        inputId = paste0("effort2_", gear),  # e.g., "effort_total"
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

  #This section contains all the functions to be used
  source("Functions/plotSpectraRelative.R")
  source("Functions/percentdiff.R")
  source("Functions/plotSpeciesWithTimeRange.R")
  source("Functions/guildplot.R")
  source("Functions/comparedietmatrix.R")
  source("Functions/create_species_level_plot.R")
  source("Functions/yieldplot.R")
  source("Functions/plotDietwComparison.R")
  source("Functions/plotNutrition.R")
  source("Functions/yearControls.R")

  source("Functions/2sims/plotSpeciesWithTimeRange2.R")
  source("Functions/2sims/create_species_level_plot2.R")
  source("Functions/2sims/plotSpectraRelative2.R")
  source("Functions/2sims/guildplot2.R")
  source("Functions/2sims/plotSpectra2.R")

  #Having a custom order on the X axis
  contexts <- c("bio", "fish")
  custom_species_order <- reactiveVal(NULL)
  species_order_choice <- reactiveVal("Custom")

  #add the ui to load when custom is pressed
  lapply(contexts, function(ctx) {
    observeEvent(input[[paste0("customOrderInfo_", ctx)]], {
      showModal(modalDialog(
        title = "Customise species order",
        rank_list(
          input_id = paste0("custom_order_rank_", ctx),
          text     = "Drag the species into the order you want:",
          labels   = species_list()
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(paste0("save_custom_order_", ctx), "Save order")
        ),
        easyClose = TRUE
      ))
    })
  })
  #save this order and change the menu so Custom is first
  lapply(contexts, function(ctx) {
    observeEvent(input[[paste0("save_custom_order_", ctx)]], {
      custom_species_order(input[[paste0("custom_order_rank_", ctx)]])
      for (x in contexts) {
        updateSelectInput(session, paste0("species_order_", x), selected = "Custom")
      }
      removeModal()
    })
  })
  #now the custom order thats been saved is made to be the species order
  lapply(contexts, function(ctx) {
    observeEvent(input[[paste0("species_order_", ctx)]], {
      species_order_choice(input[[paste0("species_order_", ctx)]])
    }, ignoreInit = TRUE)
  })
  #sets the order chosen by the user
  ordered_species <- reactive({
    choice <- species_order_choice()

    if (choice == "Guild") {
      if (exists("guildparams", inherits = FALSE)) {
        guild_order <- guildparams |>
          ## keep only the size-class with the largest maxw for each species
          group_by(Species, Feeding.guild) |>
          slice_max(maxw, n = 1, with_ties = FALSE) |>
          ungroup() |>
          ## order by the guilds' first appearance, then pull the Species names
          arrange(factor(Feeding.guild,
                         levels = unique(Feeding.guild))) |>
          pull(Species) |>
          unique()

        ## return just the species that are actually in the current model
        intersect(guild_order, default_params@species_params$species)

      } else { #if theres no guild infomration, this is the order if they choose guild
        as.data.frame(default_params@species_params$species) |>
          setNames("sp") |>
          filter(sp != "Resource") |>
          pull(sp)
}
    } else if (choice == "Size") {
      default_params@species_params %>%
        filter(species != "Resource") %>%
        arrange(w_mat) %>% pull(species)

    } else {
      # This handles "Custom" case - either use custom order or fall back to default order
      ord <- custom_species_order()
      if (length(ord)) ord else
        as.data.frame(default_params@species_params$species) %>%
        setNames("sp") %>% filter(sp != "Resource") %>% pull(sp)
    }
  })

  #will generate the +1/-1 and reset time buttons, function loaded from another file
  setupYearControls(
    input, session,
    sliderId = "year",
    minusId  = "decYear_bio",
    plusId   = "incYear_bio"
  )


# Species role  ----

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
    unharvested <- project(sims$unharvested, t_max = t_max)

    pb$inc(1/total_steps, "Projecting …")
    harvested <- project(sims$harvested, t_max = t_max)

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
      default_params@initial_n[input$species_name_select, ] * input$species

    pb$inc(1/total_steps, "Updating mortality …")
    extmort   <- getExtMort(default_params)
    totalmort <- getMort(default_params)

    extmort[input$species_name_select, ] <-
      extmort[input$species_name_select, ] +
      input$mortspecies * totalmort[input$species_name_select, ]

    ext_mort(changed_params) <- extmort

    pb$inc(1/total_steps, "Projecting …")
    harvested <- project(
      changed_params,
      # We run the simulation for an extra 5 years so that the +1 button
      # does not immediately trigger a re-run.
      t_max  = isolate(input$year) + 5
    )

    pb$inc(1/total_steps, "Done")

    # Update the reactive value
    bioSimData(list(harvested = harvested,
                    unharvested = unharvestedprojection))
  })

  #so if something breaks an error wont pop up, it will load the previous
  lastBioSpeciesPlot <- reactiveVal(NULL)
  lastBioSizePlot    <- reactiveVal(NULL)
  lastBioGuildPlot   <- reactiveVal(NULL)
  lastDietPlot     <- reactiveVal(NULL)

  #start of the output plots
  #they all generally follow the same order, so I have just commented this one.
  output$speciesPlot <- renderPlotly({
    #make sure we have model data and set the years
    req(bioSimData())
    chosen_year <- input$year
    #does user want 3 times plotted or one
    modeChoice <- if (isTRUE(input$triplotToggle)) "triple" else "chosen"

    #plot using the fucntion, but if its an error, load the previous plot
    p <- tryCatch({
      ggplotly(
        plotSpeciesWithTimeRange(
          bioSimData()$harvested,
          bioSimData()$unharvested,
          chosen_year,
          mode = modeChoice
        ) +
          scale_x_discrete(limits = ordered_species())
      )
    }, error = function(e) lastBioSpeciesPlot())

    lastBioSpeciesPlot(p)
    p
  })

  output$sizePlot <- renderPlotly({
    req(bioSimData())
    t1 <- max(input$year - 1, 1);  t2 <- input$year + 1

    p <- tryCatch({
      g <- plotSpectraRelative(
        bioSimData()$harvested,
        bioSimData()$unharvested,
        t1, t2
      )
      if (!isTRUE(input$logToggle))
        g <- g + scale_x_continuous()

      ggplotly(g)
    }, error = function(e) lastBioSizePlot())

    lastBioSizePlot(p); p
  })

  output$guildPlot <- renderPlotly({
    req(bioSimData())
    chosen_year <- input$year

    modeGuild <- if (isTRUE(input$triguildToggle)) "triple" else "chosen"

    p <- tryCatch({
      ggplotly(
        guildplot(
          bioSimData()$harvested, bioSimData()$unharvested,
          chosen_year,
          guildparams, default_params,
          mode = modeGuild
        )
      )
    }, error = function(e) lastBioGuildPlot())

    lastBioGuildPlot(p); p
  })


  output$dietplot <- renderPlotly({
    req(bioSimData())
    win <- list(start = max(input$year - 1, 1), end = input$year + 1)
    sims <- list(bioSimData()$harvested, bioSimData()$unharvested)

    p <- tryCatch({
      # Add bounds checking for time range
      sim_dims <- dim(bioSimData()$harvested@n)
      if (win$start > sim_dims[1] || win$end > sim_dims[1]) {
        message("DEBUG: Time range out of bounds")
        message("DEBUG: win$start = ", win$start, ", win$end = ", win$end)
        message("DEBUG: sim_dims[1] = ", sim_dims[1])
        return(lastDietPlot())
      }

    harvest_sub <- lapply(sims, function(p) {
      tryCatch({
        p@n       <- p@n      [win$start:win$end, , , drop = FALSE]
        p@n_pp    <- p@n_pp   [win$start:win$end, ,      drop = FALSE]
        p@n_other <- p@n_other[win$start:win$end, ,      drop = FALSE]
        p
      }, error = function(e) {
        message("DEBUG: Error in diet plot subsetting: ", e$message)
        message("DEBUG: win$start = ", win$start, ", win$end = ", win$end)
        message("DEBUG: p@n dims = ", paste(dim(p@n), collapse = "x"))
        stop(e)
      })
    })


        plotDietCompare(
          harvest_sub,
          species   = input$diet_species_select,
          sim_names = c("Your Sim", "Base Sim")
        )
      },
      error = function(e) {
        # on error, return the last successful diet plot
        message("DEBUG: Diet plot error: ", e$message)
        lastDietPlot()
      }
    )
    # store and return
    lastDietPlot(p)
    p
  })

# Fishery Strategy --------------------------------------------------------

  #changing the timerange to subset on the plot  for yield
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

  #Sim 1
  setupYearControls(
    input, session,
    sliderId  = "fishyear",
    minusId   = "decYear_fish1",
    plusId    = "incYear_fish1"
  )

  #Sim 2
  setupYearControls(
    input, session,
    sliderId  = "fishyear2",
    minusId   = "decYear_fish2",
    plusId    = "incYear_fish2"
  )


#Helper function to reactively make new effort vector depending on the input, for any mizer object.
  makeEffort <- function(prefix, gears, base_effort) {
    ef <- base_effort
    for (g in gears) {
      id <- paste0(prefix, g)
      if (!is.null(input[[id]]))
        ef[g] <- input[[id]]
    }
    ef
  }

  # Initialize fishSimData as a reactive value
  fishSimData <- reactiveVal(list(
    sim1 = unfishedprojection,
    sim2 = NULL,
    unharv = unfishedprojection
  ))

  # Reactive observer that runs when time range changes
  # It checks whether the simulation needs to be extended
  observe({
    sims <- isolate(fishSimData())
    max_year <- dim(sims$sim1@n)[1] - 1
    if (input$fishyear <= max_year) return()  # No need to re-run if within bounds

    pb <- shiny::Progress$new(); on.exit(pb$close())
    total_steps <- 3
    pb$set(message = "Running simulation …", value = 0)

    t_max <- input$fishyear - max_year + 5  # Extend by 5 years
    pb$inc(1/total_steps, "Projecting …")
    sim1 <- project(sims$sim1, t_max = t_max)

    pb$inc(1/total_steps, "Projecting …")
    sim2 <- if (!is.null(sims$sim2)) project(sims$sim2, t_max = t_max) else NULL

    pb$inc(1/total_steps, "Done")

    # Update the reactive value
    fishSimData(list(sim1 = sim1,
                    sim2 = sim2,
                    unharv = unfishedprojection))
  })

  # Reactive observer that runs simulation when inputs change
  observe({
    # Trigger on changes to these inputs
    input$fishyear
    
    # Get all effort sliders for both simulations
    gears <- unique(default_params@gear_params$gear)
    effort1 <- makeEffort("effort_" , gears, default_params@initial_effort)
    effort2 <- makeEffort("effort2_", gears, default_params@initial_effort)

    # long enough for the time range
    max_year <- input$fishyear          # ≥ 0

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

    # Update the reactive value
    fishSimData(list(sim1   = sim1,
                    sim2   = sim2,
                    unharv = unfishedprojection))
  })

  # Single time control for both simulations
  setupYearControls(
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

  #Plots
  output$yieldPlot <- renderPlotly({
    req(fishSimData())

    sims <- list()
    if (input$sim_choice == "sim1" || input$sim_choice == "both") {
      sims <- c(sims, list(fishSimData()$sim1))
    }
    if (input$sim_choice == "sim2" || input$sim_choice == "both") {
      sims <- c(sims, list(fishSimData()$sim2))
    }
    
    # If no simulations are selected, return the last successful plot
    if (length(sims) == 0) {
      return(lastYieldPlot())
    }

    p <- tryCatch({
      generateYieldDashboard(
        NS_sim          = sims,
        highlight_times = input$fishyear2_yield
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

    # Check which simulations to show
    show_sim1 <- input$sim_choice == "sim1" || input$sim_choice == "both"
    show_sim2 <- input$sim_choice == "sim2" || input$sim_choice == "both"
    
    # If no simulations are selected, return the last successful plot
    if (!show_sim1 && !show_sim2) {
      return(lastFishSpeciesPlot())
    }

    p <- tryCatch({
      if (show_sim1 && show_sim2) {
        # Both simulations
        ggplotly(
          plotSpeciesWithTimeRange2(
            fishSimData()$sim1,
            fishSimData()$sim2,
            fishSimData()$unharv,
            chosen_year,
            mode = if (isTRUE(input$triplotToggleFish)) "triple" else "chosen"
          ) + scale_x_discrete(limits = ordered_species())
        )
      } else if (show_sim1) {
        # Only Sim 1
        modeFish <- if (isTRUE(input$triplotToggleFish)) "triple" else "chosen"
        ggplotly(
          plotSpeciesWithTimeRange(
            fishSimData()$sim1,
            fishSimData()$unharv,
            chosen_year,
            mode = modeFish
          ) +
            scale_x_discrete(limits = ordered_species())
        )
      } else {
        # Only Sim 2
        modeFish <- if (isTRUE(input$triplotToggleFish)) "triple" else "chosen"
        ggplotly(
          plotSpeciesWithTimeRange(
            fishSimData()$sim2,
            fishSimData()$unharv,
            chosen_year,
            mode = modeFish
          ) +
            scale_x_discrete(limits = ordered_species())
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

    # Check which simulations to show
    show_sim1 <- input$sim_choice == "sim1" || input$sim_choice == "both"
    show_sim2 <- input$sim_choice == "sim2" || input$sim_choice == "both"
    
    # If no simulations are selected, return the last successful plot
    if (!show_sim1 && !show_sim2) {
      return(lastFishSizePlot())
    }

    p <- tryCatch({
      if (show_sim1 && show_sim2) {
        # Both simulations
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
        # Only Sim 1
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
        # Only Sim 2
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
    chosen_year <- fish_win1()

    # Check which simulations to show
    show_sim1 <- input$sim_choice == "sim1" || input$sim_choice == "both"
    show_sim2 <- input$sim_choice == "sim2" || input$sim_choice == "both"
    
    # If no simulations are selected, return the last successful plot
    if (!show_sim1 && !show_sim2) {
      return(lastFishGuildPlot())
    }

    modeGuild <- if (isTRUE(input$triguildToggleFish)) "triple" else "chosen"

    p <- tryCatch({
      if (show_sim1 && show_sim2) {
        # Both simulations
        ggplotly(
          guildplot_both(
            fishSimData()$sim1, fishSimData()$sim2, fishSimData()$unharv,
            chosen_year,
            guildparams, default_params,
            mode = modeGuild
          )
        )
      } else if (show_sim1) {
        # Only Sim 1
        ggplotly(
          guildplot(
            fishSimData()$sim1, fishSimData()$unharv,
            chosen_year,
            guildparams, default_params,
            mode = modeGuild
          )
        )
      } else {
        # Only Sim 2
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

  #So that you can repress the button and rerun
  observeEvent(c(input$goButton2, input$goButton22), {
    built(FALSE)
  }, ignoreInit = TRUE)

  # So that it saves the last plot, and remembers that the plot is already built (so just change data not plot object)
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

  #helper to get the data
  getSpectraData <- function(sim, win) {
    df <- plotSpectra(sim$sim1,
                      time_range  = win$start:win$end,
                      return_data = TRUE)
    split(df, df$Species)
  }

  output$spectrumPlot <- renderPlotly({
    p <- tryCatch({

      sim <- isolate(fishSimData()); req(sim)
      win <- isolate(fish_win1())
      
      # Check which simulations to show
      show_sim1 <- input$sim_choice == "sim1" || input$sim_choice == "both"
      show_sim2 <- input$sim_choice == "sim2" || input$sim_choice == "both"
      
      # If no simulations are selected, return the last successful plot
      if (!show_sim1 && !show_sim2) {
        return(lastSpectrumPlot())
      }
      
      # Prepare data for selected simulations
      df1 <- NULL
      df2 <- NULL
      
      if (show_sim1) {
        df1 <- plotSpectra(sim$sim1,
                           time_range  = win$start:win$end,
                           return_data = TRUE) %>%
          mutate(sim = "Sim 1")
      }
      
      if (show_sim2) {
        df2 <- plotSpectra(sim$sim2,
                           time_range  = win$start:win$end,
                           return_data = TRUE) %>%
          mutate(sim = "Sim 2")
      }

      # Combine species from both simulations
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
        
        if (show_sim1 && !is.null(df1)) {
          sub1 <- df1 %>% filter(Species == sp)
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
        
        if (show_sim2 && !is.null(df2)) {
          sub2 <- df2 %>% filter(Species == sp)
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

      axType <- if (isTRUE(input$logToggle5)) "log" else "linear"
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

  #Change the time plotted without having to replot everything (thereby saving the legends chosen)
  observeEvent(
    fish_win1(),
    {
      req(built())

      tryCatch({
        win <- fish_win1()
        sim <- fishSimData()
        
        # Check which simulations to show
        show_sim1 <- input$sim_choice == "sim1" || input$sim_choice == "both"
        show_sim2 <- input$sim_choice == "sim2" || input$sim_choice == "both"

        # ---- pull fresh spectra -------------------------------------------------
        spec1 <- list()
        spec2 <- list()
        
        if (show_sim1) {
          df1 <- plotSpectra(sim$sim1,
                             time_range  = win$start:win$end,
                             return_data = TRUE)
          spec1 <- split(df1, df1$Species)
        }

        if (show_sim2) {
          df2 <- plotSpectra(sim$sim2,
                             time_range  = win$start:win$end,
                             return_data = TRUE)
          spec2 <- split(df2, df2$Species)
        }

        species <- sort(unique(c(names(spec1), names(spec2))))
        px <- plotlyProxy("spectrumPlot", session)

        i <- 0L
        for (sp in species) {
          if (show_sim1 && sp %in% names(spec1)) {
            sub1 <- spec1[[sp]]
            plotlyProxyInvoke(
              px, "restyle",
              list(x = list(sub1$w),
                   y = list(sub1$value)),
              list(i))
            i <- i + 1L
          }
          if (show_sim2 && sp %in% names(spec2)) {
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

  #Toggle X axis to log or not for Spectrum
  observeEvent(
    input$logToggle5,
    {
      req(built())

      tryCatch({
        axType <- if (isTRUE(input$logToggle5)) "log" else "linear"
        plotlyProxyInvoke(
          plotlyProxy("spectrumPlot", session),
          "relayout",
          list(xaxis = list(type = axType),
               yaxis = list(type = "linear")))
      }, error = function(e) {
        message("Axis relayout failed: ", e$message)
      })
    },
    ignoreInit = TRUE
  )

  output$fishdietsingleplot <- renderPlotly({
    req(fishSimData())

    win <- fish_win1()

    # Check which simulations to show
    show_sim1 <- input$sim_choice == "sim1" || input$sim_choice == "both"
    show_sim2 <- input$sim_choice == "sim2" || input$sim_choice == "both"
    
    # If no simulations are selected, return the last successful plot
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
      # Add bounds checking for time range
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

    # Check which simulations to show
    show_sim1 <- input$sim_choice == "sim1" || input$sim_choice == "both"
    show_sim2 <- input$sim_choice == "sim2" || input$sim_choice == "both"
    
    # If no simulations are selected, return the last successful plot
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

}




# NOTE - FOR UI, all of the code has tagAppendAttributes, which makes it
# confusing, but it is necessary as you have to label the sections of the code to
# be able to put it into the tutorial of the app.

ui <- fluidPage(
  rintrojs::introjsUI(),
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
    tags$script(HTML("
    document.addEventListener('shiny:connected', function() {
      document
        .querySelectorAll('[data-bs-toggle=\"popover\"]')
        .forEach(function(el) {
          new bootstrap.Popover(el, {
            container: 'body',
            html: true,
            sanitize: false
          });
        });
      });
    ")),
    tags$script(src = "app.js")
  ),
  bslib::page_navbar(
    id = "bigtabpanel",
    title = tagList(
      img(src = "mizer.png", height = "75px",
          style = "vertical-align: middle; margin-right: 15px; margin-bottom: 5px; margin-top: 5px;"),
      "mizerShiny"
    ),
    selected    = "Species Role",
    navbar_options = bslib::navbar_options(collapsible = TRUE),
    theme       = bs_theme(bootswatch = "cerulean"),

    # Fishery Strategy tab ----
    tabPanel(
      title = "Fishery Strategy",
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
              inputId = "fishyear",
              label   = "Time Range",
              min     = 1,
              max     = fish_max_year,
              value   = 5,
              step    = 1,
              width   = "100%"
            ) %>% tagAppendAttributes(id = "fishyyear"),
            div(id   = "yearAdjustButtons_fish",
                style = "display:flex; justify-content:center; gap:10px;",
                actionButton("decYear_fish", "-1 year", class = "btn-small"),
                actionButton("incYear_fish", "+1 year", class = "btn-small")
            ),
            tabsetPanel(
              tabPanel(
                title = "Sim 1",
                div(id = "fishery_sliders", uiOutput("fishery_sliders_ui"))
              ),
              tabPanel(
                title = "Sim 2",
                div(id = "fishery_sliders", uiOutput("fishery_sliders_ui2"))
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
                id = "fishy_plots",
                tabPanel(
                  title = "Species",
                  div(style = "flex:1; display:flex;",
                      plotlyOutput("fishspeciesPlot", height = "100%", width = "100%")
                  )
                ),
                tabPanel(
                  title = "Yield",
                  div(style = "flex:1; display:flex;",
                      plotlyOutput("yieldPlot", height = "100%", width = "100%")
                  )
                ),
                tabPanel(
                  title = "Size",
                  div(style = "flex:1; display:flex;",
                      plotlyOutput("fishsizePlot", height = "100%", width = "100%")
                  )
                ),
                if (app_exists("Including", "guilds_information", "checkGuilds", "guildparams_preprocessed.Rdata")) {
                  tabPanel(
                    title = "Guild",
                    div(style = "flex:1; display:flex;",
                        plotlyOutput("fishguildPlot", height = "100%", width = "100%")
                    )
                  )
                },
                tabPanel(
                  title = "Spectra",
                  div(style = "flex:1; display:flex;",
                      plotlyOutput("spectrumPlot", height = "100%", width = "100%")
                  )
                ),
                tabPanel(
                  title = "Diet",
                  div(style = "height:50vh; display:flex;",
                      plotlyOutput("fishdietsingleplot", height = "100%", width = "100%")
                  )
                ),
                if (app_exists("Including", "Nutrition", "checkNutrition", "nutrition.csv")) {
                  tabPanel(
                    title = "Nutrition",
                    div(style = "flex:1; display:flex;",
                        plotlyOutput("nutritionplot", height = "100%", width = "100%")
                    )
                  )
                }
              )
            )
          ),

          card_body(
            style = "flex: auto",
            conditionalPanel(
              condition = "input.fishy_plots == 'Yield'",
              div(style = "margin-bottom:1.5rem;",
                  legendUI("infoButtonOrder", legends$fishery_yield)
              ),
              div(style = "display: flex; align-items: center; gap: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
                  HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show:</span>"),
                  radioButtons(
                    inputId = "sim_choice",
                    label   = NULL,
                    choices = c("Sim 1" = "sim1", "Sim 2" = "sim2", "Both" = "both"),
                    selected = "sim1",
                    inline = TRUE
                  )
              ),
              div(style = "display: flex; align-items: center; gap: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                  HTML("<span style='margin-top:0px; margin-bottom:0.5rem; font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Yield Time Range</span>"),
                  sliderInput(
                    inputId = "fishyear2_yield",
                    label   = NULL,
                    min     = 1,
                    max     = fish_max_year,
                    value   = c(1, 10),
                    step    = 1,
                    width   = "200px"
                  ) %>% tagAppendAttributes(id = "fishyyear_yield")
              )
            ),
            conditionalPanel(
              condition = "input.fishy_plots == 'Species'",
              div(style = "display: flex; align-items: center; gap: 15px; flex-wrap: wrap;",
                  div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                      legendUI("infoButtonOrder", legends$fishery_species)
                  ),
                  div(style = "display: flex; align-items: center; gap: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
                      HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show:</span>"),
                      radioButtons(
                        inputId = "sim_choice",
                        label   = NULL,
                        choices = c("Sim 1" = "sim1", "Sim 2" = "sim2", "Both" = "both"),
                        selected = "sim1",
                        inline = TRUE
                      )
                  ),
                  div(style = "display: flex; align-items: center; gap: 10px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
                      HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Species Order:</span>"),
                      selectInput(
                        inputId = "species_order_fish",
                        label   = NULL,
                        choices = c("Custom", "Size", "Guild"),
                        width = "120px"
                      ),
                      HTML(
                        "<button id='infoButtonOrder' class='btn btn-info btn-xs' type='button' data-bs-toggle='popover' title='' data-bs-content='Select how you want the species to be ordered on the axis. Options include &quot;Custom&quot;, &quot;Size&quot; and &quot;Guild&quot;. Click the &quot;customise&quot; button to change the custom order.'><strong>?</strong></button>"
                      ),
                      actionButton(
                        "customOrderInfo_fish",
                        label = HTML("<strong>customise</strong>"),
                        class = "btn btn-info btn-xs no-focus-outline"
                      )
                  ),
                  div(style = "display: flex; align-items: center; gap: 10px; padding: 10px; background-color: #f3e5f5; border-radius: 5px; border: 1px solid #e1bee7;",
                      materialSwitch(
                        inputId = "triplotToggleFish",
                        label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show intermediate years</span>"),
                        value   = TRUE,
                        status  = "info"
                      )
                  )
              )
            ),
            conditionalPanel(
              condition = "input.fishy_plots == 'Size'",
              div(style = "display: flex; align-items: center; gap: 15px;",
                  div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                      legendUI("infoButtonOrder", legends$fishery_size)
                  ),
                  div(style = "display: flex; align-items: center; gap: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
                      HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show:</span>"),
                      radioButtons(
                        inputId = "sim_choice",
                        label   = NULL,
                        choices = c("Sim 1" = "sim1", "Sim 2" = "sim2", "Both" = "both"),
                        selected = "sim1",
                        inline = TRUE
                      )
                  ),
                  div(style = "padding: 10px; background-color: #e8f5e8; border-radius: 5px; border: 1px solid #c8e6c9;",
                      materialSwitch(
                        inputId = "logToggle4",
                        label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Log</span>"),
                        value   = TRUE,
                        status  = "info"
                      )
                  )
              )
            ),
            conditionalPanel(
              condition = "input.fishy_plots == 'Guild'",
              div(style = "display: flex; align-items: center; gap: 15px;",
                  div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                      legendUI("infoButtonOrder", legends$fishery_guild)
                  ),
                  div(style = "display: flex; align-items: center; gap: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
                      HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show:</span>"),
                      radioButtons(
                        inputId = "sim_choice",
                        label   = NULL,
                        choices = c("Sim 1" = "sim1", "Sim 2" = "sim2", "Both" = "both"),
                        selected = "sim1",
                        inline = TRUE
                      )
                  ),
                  div(style = "padding: 10px; background-color: #fff3e0; border-radius: 5px; border: 1px solid #ffcc80;",
                      materialSwitch(
                        inputId = "triguildToggleFish",
                        label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show intermediate years</span>"),
                        value   = TRUE,
                        status  = "info"
                      )
                  )
              )
            ),
            conditionalPanel(
              condition = "input.fishy_plots == 'Spectra'",
              div(style = "display: flex; align-items: center; gap: 15px;",
                  div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                      legendUI("infoButtonOrder", legends$fishery_spectra)
                  ),
                  div(style = "display: flex; align-items: center; gap: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
                      HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show:</span>"),
                      radioButtons(
                        inputId = "sim_choice",
                        label   = NULL,
                        choices = c("Sim 1" = "sim1", "Sim 2" = "sim2", "Both" = "both"),
                        selected = "sim1",
                        inline = TRUE
                      )
                  ),
                  div(style = "padding: 10px; background-color: #fce4ec; border-radius: 5px; border: 1px solid #f8bbd9;",
                      materialSwitch(
                        inputId = "logToggle5",
                        label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Log</span>"),
                        value   = TRUE,
                        status  = "info"
                      )
                  )
              )
            ),
            conditionalPanel(
              condition = "input.fishy_plots == 'Diet'",
              div(style = "display: flex; align-items: center; gap: 15px;",
                  div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                      legendUI("infoButtonOrder", legends$fishery_diet_single)
                  ),
                  div(style = "display: flex; align-items: center; gap: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
                      HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show:</span>"),
                      radioButtons(
                        inputId = "sim_choice",
                        label   = NULL,
                        choices = c("Sim 1" = "sim1", "Sim 2" = "sim2", "Both" = "both"),
                        selected = "sim1",
                        inline = TRUE
                      )
                  ),
                  div(style = "display: flex; align-items: center; gap: 10px; padding: 10px; background-color: #e0f2f1; border-radius: 5px; border: 1px solid #b2dfdb;",
                      HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Select a Species:</span>"),
                      selectInput(
                        inputId = "fish_name_select",
                        label   = NULL,
                        choices = NULL,
                        width = "200px"
                      )
                  )
              )
            ),
            conditionalPanel(
              condition = "input.fishy_plots == 'Nutrition'",
              div(style = "display: flex; align-items: center; gap: 15px;",
                  div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                      legendUI("infoButtonOrder", legends$nutrition)
                  ),
                  div(style = "display: flex; align-items: center; gap: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
                      HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show:</span>"),
                      radioButtons(
                        inputId = "sim_choice",
                        label   = NULL,
                        choices = c("Sim 1" = "sim1", "Sim 2" = "sim2", "Both" = "both"),
                        selected = "sim1",
                        inline = TRUE
                      )
                  )
              )
            )
          )
        )
      )
    ),

    # Species Role tab ----
    tabPanel(
      title = "Species Role",

      grid_container(
        layout    = c("area1 area0"),
        row_sizes = c("1fr"),
        col_sizes = c("0.3fr", "1.7fr"),
        gap_size  = "10px",

        grid_card(
          area = "area1",
          card_body(
            selectInput(
              inputId = "species_name_select",
              label   = "Select a Species:",
              choices = NULL
            ) %>% tagAppendAttributes(id = "species_chose"),
            sliderInput(
              inputId = "species",
              label   = HTML(
                "Starting Biomass <button id='infoButtonSpecies' class='btn btn-info btn-xs' type='button' \
                data-bs-toggle='popover' title='' \
                data-bs-content='Slider value indicates the starting biomass of the species. Example: to increase the starting population of a given species by 20%, set value on the slider to 1.2. To decrease by 20%, set value to 0.8.'>\
                <strong>?</strong></button>"
              ),
              min   = 0,
              max   = 2,
              value = 1,
              step  = 0.01,
              width = "100%"
            ) %>% tagAppendAttributes(id = "species_slider"),
            sliderInput(
              inputId = "mortspecies",
              label   = HTML(
                "Mortality Change <button id='infoButtonMort' class='btn btn-info btn-xs' type='button' \
                data-bs-toggle='popover' title='' \
                data-bs-content='Slider value indicates the change in mortality of a species. Example: to increase the mortality of a species by 1%, set the value of the slider to 0.01. This will change the mortality throughout the simulation to be 1% higher. If you want it to be a 1% decrease, set value to -0.01'>\
                <strong>?</strong></button>"
              ),
              min   = -0.25,
              max   = 0.25,
              value = 0,
              step  = 0.001,
              width = "100%"
            ) %>% tagAppendAttributes(id = "mort_slider"),
            sliderInput(
              inputId = "year",
              label   = "Time Range",
              min     = 1,
              max     = sp_max_year,
              value   = 5,
              step    = 1,
              width   = "100%"
            ) %>% tagAppendAttributes(id = "yearspecies_slider"),
            div(id   = "yearAdjustButtons_bio",
                style = "display:flex; justify-content:center; gap:10px;",
                actionButton("decYear_bio", "-1 year", class = "btn-small"),
                actionButton("incYear_bio", "+1 year", class = "btn-small")
            )
          )
        ),

        grid_card(
          area = "area0",

          card_body(                       # added class
            class = "plot-card",
            style = "flex: 4; overflow: hidden; margin-top: -0.5rem",
            tabsetPanel(
              id = "plotTabs",
              tabPanel(title = "Species", plotlyOutput("speciesPlot", height = "55vh")),
              tabPanel(title = "Size",     plotlyOutput("sizePlot",     height = "55vh")),
              if (app_exists("Including", "guilds_information", "checkGuilds",
                             "guildparams_preprocessed.Rdata")) {
                tabPanel(title = "Guilds",   plotlyOutput("guildPlot",    height = "55vh"))},
              tabPanel(title = "Diet",
                       div(style = "height:50vh; display:flex;",
                           plotlyOutput("dietplot", height = "100%", width = "100%")
                       ))
            )
          ),

          card_body(
            style = "flex: 1.46;",

            conditionalPanel(
              condition = "input.plotTabs == 'Species'",
              div(style = "display: flex; align-items: center; gap: 15px; flex-wrap: wrap;",
                  div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                      legendUI("infoButtonOrder", legends$biomass_species)
                  ),
                  div(style = "display: flex; align-items: center; gap: 10px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
                      HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Species Order:</span>"),
                      selectInput(
                        inputId = "species_order_bio",
                        label   = NULL,
                        choices = c("Custom", "Size", "Guild"),
                        width = "120px"
                      ),
                      HTML(
                        "<button id='infoButtonOrder' class='btn btn-info btn-xs' type='button' data-bs-toggle='popover' title='' data-bs-content='Select how you want the species to be ordered on the axis. Options include &quot;Custom&quot;, &quot;Size&quot; and &quot;Guild&quot;. Click the &quot;customise&quot; button to change the custom order.'><strong>?</strong></button>"
                      ),
                      actionButton(
                        "customOrderInfo_bio",
                        label = HTML("<strong>customise</strong>"),
                        class = "btn btn-info btn-xs no-focus-outline"
                      )
                  ),
                  div(style = "display: flex; align-items: center; gap: 10px; padding: 10px; background-color: #f3e5f5; border-radius: 5px; border: 1px solid #e1bee7;",
                      materialSwitch(
                        inputId = "triplotToggle",
                        label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show intermediate years</span>"),
                        value   = TRUE,
                        status  = "info"
                      )
                  )
              )
            ),

            conditionalPanel(
              condition = "input.plotTabs == 'Size'",
              div(style = "display: flex; align-items: center; gap: 15px;",
                  div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                      legendUI("infoButtonOrder", legends$biomass_size)
                  ),
                  div(style = "padding: 10px; background-color: #e8f5e8; border-radius: 5px; border: 1px solid #c8e6c9;",
                      materialSwitch(
                        inputId = "logToggle",
                        label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Log</span>"),
                        value   = TRUE,
                        status  = "info"
                      )
                  )
              )
            ),

            conditionalPanel(
              condition = "input.plotTabs == 'Guilds'",
              div(style = "display: flex; align-items: center; gap: 15px;",
                  div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                      legendUI("infoButtonOrder", legends$biomass_guild)
                  ),
                  div(style = "padding: 10px; background-color: #fff3e0; border-radius: 5px; border: 1px solid #ffcc80;",
                      materialSwitch(
                        inputId = "triguildToggle",
                        label   = HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show intermediate years</span>"),
                        value   = TRUE,
                        status  = "info"
                      )
                  )
              )
            ),
            conditionalPanel(
              condition = "input.plotTabs == 'Diet'",
              div(style = "display: flex; align-items: center; gap: 15px;",
                  div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                      legendUI("infoButtonDietBio", legends$fishery_diet_single)
                  ),
                  div(style = "display: flex; align-items: center; gap: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
                      HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Show:</span>"),
                      radioButtons(
                        inputId = "sim_choice",
                        label   = NULL,
                        choices = c("Sim 1" = "sim1", "Sim 2" = "sim2", "Both" = "both"),
                        selected = "sim1",
                        inline = TRUE
                      )
                  ),
                  div(style = "display: flex; align-items: center; gap: 10px; padding: 10px; background-color: #e0f2f1; border-radius: 5px; border: 1px solid #b2dfdb;",
                      HTML("<span style='font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Select a Species:</span>"),
                      selectInput(
                        inputId = "diet_species_select",
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
    ),

    # Page guide ----
    bslib::nav_spacer(),
    bslib::nav_item(
      actionButton("start_tutorial", "Page Guide", class = "btn btn-primary",
                   style = "margin-right: 20px; padding: 5px 5px;")
    )
  )
)

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

