# Core Shiny and UI packages
library(shiny)
library(bslib)
library(gridlayout)
library(shinyjs)
library(shinyWidgets)
library(rintrojs)
library(sortable)

# Data manipulation and visualization
library(dplyr)
library(ggplot2)
library(plotly)

# Mizer ecosystem package
library(mizer)

default_params <- getShinyOption("default_params")
if (is.null(default_params)) {
  default_params <- readRDS("default_params.rds")
}

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
if (have_guild_file) {
  guild_env <- new.env(parent = emptyenv())
  load(guild_file, envir = guild_env)
  if (exists("guildparams", envir = guild_env, inherits = FALSE)) {
    guildparams <- get("guildparams", envir = guild_env)
  }
}

nut_file <- app_path("Including", "Nutrition", "checkNutrition", "nutrition.csv")
have_nutrition_file <- file.exists(nut_file)
#Loading in text for legends and the code to generate them
source(app_path("www", "legendsTXT.R"), local = TRUE)
source(app_path("Functions", "legendUI.R"), local = TRUE)

# Load modules
source(app_path("modules", "species_role_module.R"), local = TRUE)
source(app_path("modules", "fishery_strategy_module.R"), local = TRUE)

# Make helper functions available to modules (source at top-level)
source(app_path("Functions", "yearControls.R"), local = TRUE)
source(app_path("Functions", "plotSpectraRelative.R"), local = TRUE)
source(app_path("Functions", "percentdiff.R"), local = TRUE)
source(app_path("Functions", "plotSpeciesWithTimeRange.R"), local = TRUE)
source(app_path("Functions", "guildplot.R"), local = TRUE)
source(app_path("Functions", "comparedietmatrix.R"), local = TRUE)
source(app_path("Functions", "create_species_level_plot.R"), local = TRUE)
source(app_path("Functions", "yieldplot.R"), local = TRUE)
source(app_path("Functions", "plotDietwComparison.R"), local = TRUE)
source(app_path("Functions", "plotNutrition.R"), local = TRUE)
source(app_path("Functions", "species_plot_utils.R"), local = TRUE)
source(app_path("Functions", "2sims", "plotSpeciesWithTimeRange2.R"), local = TRUE)
source(app_path("Functions", "2sims", "create_species_level_plot2.R"), local = TRUE)
source(app_path("Functions", "2sims", "plotSpectraRelative2.R"), local = TRUE)
source(app_path("Functions", "2sims", "guildplot2.R"), local = TRUE)
source(app_path("Functions", "2sims", "plotSpectra2.R"), local = TRUE)

# Server ----
server <- function(input, output, session) {

  # loading the introduction/guide ----
  source(app_path("tutorial_steps", "get_intro_steps.R"), local = TRUE)
  observeEvent(input$start_tutorial, {
    steps <- get_intro_steps(input)

    if (length(steps) > 0) {
      introjs(session, options = list(steps = steps))
    }
  })

  # Shared reactive for species list - used by modules
  species_list <- reactive({
    species <- setdiff(unique(default_params@species_params$species),
            ("Resource"))
    species
  })

  # Helper functions are sourced at top-level for module access

  source(app_path("Functions", "2sims", "plotSpeciesWithTimeRange2.R"), local = TRUE)
  source(app_path("Functions", "2sims", "create_species_level_plot2.R"), local = TRUE)
  source(app_path("Functions", "2sims", "plotSpectraRelative2.R"), local = TRUE)
  source(app_path("Functions", "2sims", "guildplot2.R"), local = TRUE)
  source(app_path("Functions", "2sims", "plotSpectra2.R"), local = TRUE)

  #Having a custom order on the X axis
  # Map module names to context suffixes
  module_contexts <- list(species = "bio", fishery = "fish")
  custom_species_order <- reactiveVal(NULL)
  species_order_choice <- reactiveVal("Custom")

  #add the ui to load when custom is pressed - watch for module namespaced inputs
  lapply(names(module_contexts), function(mod_id) {
    ctx <- module_contexts[[mod_id]]
    observeEvent(input[[paste0(mod_id, "-customOrderInfo_", ctx)]], {
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
  lapply(names(module_contexts), function(mod_id) {
    ctx <- module_contexts[[mod_id]]
    observeEvent(input[[paste0("save_custom_order_", ctx)]], {
      custom_species_order(input[[paste0("custom_order_rank_", ctx)]])
      # Update both module select inputs
      for (mod in names(module_contexts)) {
        updateSelectInput(session, paste0(mod, "-species_order_", module_contexts[[mod]]), selected = "Custom")
      }
      removeModal()
    })
  })
  #now the custom order thats been saved is made to be the species order - watch module inputs
  lapply(names(module_contexts), function(mod_id) {
    ctx <- module_contexts[[mod_id]]
    observeEvent(input[[paste0(mod_id, "-species_order_", ctx)]], {
      species_order_choice(input[[paste0(mod_id, "-species_order_", ctx)]])
    }, ignoreInit = TRUE)
  })
  #sets the order chosen by the user
  ordered_species <- reactive({
    mizerShiny:::compute_ordered_species(
      default_params = default_params,
      guildparams    = if (exists("guildparams", inherits = TRUE)) guildparams else NULL,
      choice         = species_order_choice(),
      custom_order   = custom_species_order()
    )
  })

  # Initialize modules ----
  # Pass shared reactive for species ordering to modules
  ordered_species_reactive <- ordered_species
  
  # Handle guildparams - may not exist (check parent environments too)
  guildparams_for_modules <- if (exists("guildparams", inherits = TRUE)) guildparams else NULL
  
  # Species Role module
  species_role_server(
    "species",
    default_params = default_params,
    unharvestedprojection = unharvestedprojection,
    guildparams = guildparams_for_modules,
    ordered_species_reactive = ordered_species_reactive,
    species_list = species_list
  )
  
  # Fishery Strategy module
  fishery_strategy_server(
    "fishery",
    default_params = default_params,
    unfishedprojection = unfishedprojection,
    guildparams = guildparams_for_modules,
    ordered_species_reactive = ordered_species_reactive,
    species_list = species_list,
    fish_max_year = fish_max_year
  )

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
    selected    = "Fishery Strategy",
    navbar_options = bslib::navbar_options(collapsible = TRUE),
    theme       = bs_theme(bootswatch = "cerulean"),

    # Fishery Strategy tab ----
    tabPanel(
      title = "Fishery Strategy",
      fishery_strategy_ui("fishery", fish_max_year, have_guild_file, have_nutrition_file, app_exists)
    ),

    # Species Role tab ----
    tabPanel(
      title = "Species Role",
      species_role_ui("species", sp_max_year, have_guild_file, app_exists)
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

