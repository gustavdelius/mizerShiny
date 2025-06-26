get_intro_steps <- function(input) {
  # Ensure required inputs are available
  req(input$bigtabpanel)
  
  intro_steps <- list()  # Initialize an empty list
  
  if (input$bigtabpanel == "Species Role") {
    source("tutorial_steps/single_species_biomass.R", local = TRUE)
  } else if (input$bigtabpanel == "Fishery Strategy") {
    source("tutorial_steps/fishery_strategy.R", local = TRUE)
  }
  
  # The sourced file should create the variable 'intro_steps'
  return(intro_steps)
}
