intro_steps <- list(
  list(element = "#species_chose", title = "Species Selector",
       intro = "Here is where you choose the species you want to modify."),
  list(element = "#species_slider", title = "Biomass Slider",
       intro = "This slider will change the starting biomass of the
                 species in the model. Changing the starting biomass intends to
                 show the trophic role of different species within the ecosystem,
                 as the knock on effect on the other species can be seen
                 in the plots. Hover over the information button to learn more."),
  list(element = "#mort_slider", title = "Mortality Slider",
       intro = "The Mortality slider changes the fitness of the species by
                changing the mortality rate of a given species across their
                entire size range."),
  list(element = "#yearspecies_slider", title = "Time Range Slider",
       intro = "Changing the value of this slider will change the
                 year that is plotted. It is useful to look at different time scales,
                 as the impact of the imposed change may differ and it will show the
                 oscillatory change in fish populations."),
  list(element = "#yearAdjustButtons_bio", title = "Time Controls",
       intro = "Use the +1/-1 buttons to quickly adjust the time range."),
  list(element = "#species_order_bio_box", title = "Species Order",
       intro = "On graphs that show changes in each of the species within the model,
            changing the order that these species are presented may allow for easier observation of
            any general patterns. You can change and customise the order using the options here."),
  list(element = "#plotTabs", title="Plots", intro = "Here is where you can navigate between different plots.
       The plots are interactive. You can look at specific areas of the plot by selecting with your mouse (then double click to reset),
       remove plot items by clicking them on the legend (double click to remove everything but that one), hover over plot information to get
       more specific values and save the plot by using the picture icon in the top right of the plot."),
  list(element = "#plotTabs .nav-link[data-value='Biomass']", title = "Biomass",
       intro = "The first plot presents the percentage change in the biomass of each species.
       This percentage change is relative to the original steady state.
       Each species has 3 bars that indicate the percentage change at
       specific time points: quarter of the selected year, half of the selected year, and the selected year.
      " ),
  list(element = "#plotTabs .nav-link[data-value='Biomass']",title = "Biomass continued",
       intro = "Plotting all 3 time points
      aids in understanding how the ecosystem changes evolve over time, showing the progression from early to late stages of the simulation.
      " ),
  list(element = "#triMode",title = "Change Time Plotted",
       intro = "When changing the time with the +1/-1 buttons, it is difficult to observe changes when 3 time points are plotted. Use this toggle to change
       from 3 time points plotted to only the selected year plotted.
      " ),
  list(element = "#plotTabs .nav-link[data-value='Size']",title = "Size",
       intro = "The next plot shows the relative change in the size spectrum on a community level. Plotting the size spectrum informs
      the viewer of the change in community composition, more specifically, how the distribution of fish size has changed."
  ),
  list(element = "#plotTabs .nav-link[data-value='Guilds']",title = "Guilds",
       intro = "The plot here showcases the change in the guilds within the species, which are fish in the ecosystem that share a distinct
       feeding pattern in relation to other fish. Observing this plot helps to understand how the trophic dynamics of the ecosystem are changing.
      Additionally, the guilds are plotted as 3 bars representing biomass at quarter, half and full of the selected year."
  ),
  list(element = "#plotTabs .nav-link[data-value='Diet']",title = "Diet",
       intro = "This plot shows what a species of fish is eating across its size range. The majority of
       a species diet will be the 'Resource' - the 'Resource' is meant to simulate the plankton in a marine ecosystem and acts as the bottom
       trophic level, providing energy for the modelled species. The point of this tab is so that you can find the reasons behind the ecosystem
       changes that you have observed, it makes the trophic links between species clear, more specifically, what eats what."
  )

)
