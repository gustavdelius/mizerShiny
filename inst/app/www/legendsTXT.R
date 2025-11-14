legends <- list(
  role_biomass =
    "Change in biomass of each species at specific time intervals. The X axis shows \
the species and the Y axis is the percentage change in its biomass compared \
with the base simulation. Each species has three bars representing biomass at \
quarter, half and full of the selected year. Red/blue indicate direction; bright &rarr; dark shows \
quarter/half/full time intervals.",
  role_size = # Will also be used for fishery_size
    "Change in the community size spectrum in comparison to the base \
simulation. The community size spectrum is the biomass of all species in the \
ecosystem at a given size. The blue line is the changed simulation; the dashed \
line is the base simulation. Use the <em>Log</em> switch to change the \
X&nbsp;axis from log-scale to linear.",
  role_guild = # Will also be used for fishery_guild
    "Percentage change in biomass by feeding guild relative to the baseline strategy. Feeding guilds
group fish by diet and life stage.\
 X axis is the guild, Y is the % change. Shading distinguishes earlier → later time intervals when “Intermediate years” is on. ",
  role_diet = # Will also be used for fishery_diet
      "Diet composition of a selected fish across its size range. X axis is the \
    size of fish, Y axis is the proportion of diet of a prey species. Colour \
    indicates prey species.",
  fishery_nutrition =
      "Relative change in nutrition compared with the baseline fishing strategy. \
X axis contains the nutrient, Y axis is the % change in comparison with the \
  baseline fishing strategy. Colour indicates whether change is positive or \
  negative. Nutrient amount is calculated from biomass of species caught.",
  fishery_yield_composition =
    "Dashboard of yield for the chosen fishery strategy within the \
selected time range. <strong>Species Change</strong> – yield of individual \
species over time. <strong>Gear Change</strong> – yield of each gear over \
time. <strong>Yield Composition</strong> – total yield for each gear within \
the time range. <strong>Total&nbsp;Yield&nbsp;Per&nbsp;Sim</strong> – species \
composition of total yield (bar height&nbsp;=&nbsp;total yield). \
<strong>Pie Chart</strong> – species composition shown as a pie chart. If <strong>two \
simulations</strong> are plotted, the second uses dashed lines (line graphs) and its \
own bar and pie plots.",
  fishery_spectra =
    "Biomass density across size for each species. Use the <em>Log</em> switch \
to toggle the X&nbsp;axis between log and linear scales. Click the legend \
to select and remove species from the plot. Resource is an energy input into \
the simulation that models plankton."
)

# --- Toggle help  ---
legends$role_show_intermediate_years <-
    "When on, each chart shows three time intervals for every species/guild: \
¼ and ½ of the span from the model start to the selected end year, plus the final year. \
Turn off to show only the final year."

legends$role_log <-
    "Switch the body-size axis between logarithmic and linear scaling."
legends$role_species_select <-
    "Choose the species whose biomass and mortality sliders act on, and is plotted."
legends$role_time_range <-
    "Set the end of the simulation period used in plots. Drag the slider to choose the end year; use “–1 year” / “+1 year” to step by one year."

legends$fishery_show_intermediate_years <-
    "When on, each bar chart shows three time intervals for every species/guild: \
¼ and ½ of the span from the model start to the selected end year, plus the end-year value. \
Turn off to show only the end-year value."

legends$fishery_log <-
    "Switch the body-size axis between logarithmic and linear scaling."
legends$fishery_time_range <-
    "Set the simulation period. Drag the slider to choose the end year; use “–1 year” / “+1 year” to step by one year."
legends$fishery_slider_tabs <-
    "Choose which strategy’s sliders you are editing (Strategy 1 or Strategy 2). \
Changes only affect the selected strategy."
legends$fishery_sim_choice <-
    "Choose which strategies to display: Strategy 1, Strategy 2, or both. \
When both are shown, plots are split by strategy."

legends$fishery_multispecies <-
    "Toggle multispecies interactions (predation). \
On = interacting community (species affect one another). \
Off = non‑interacting (each species projects independently)."

legends$tab_fishery_strategy <-
    "Explore how different fishing strategies change the modelled community over time. ."
legends$tab_species_role <-
    "Explore how species contribute to the ecosystem by studying the effect of changing their initial abundance or their mortality rate."
legends$page_guide_button <-
    "Launch a short, page-specific walkthrough explaining the main controls and plots."

legends$fishery_biomass <-
    "Total biomass by species for the selected fishing strategy (default is baseline). X: species; Y: biomass. Shading distinguishes earlier → later time intervals."

legends$fishery_biomass_change <-
    "Percentage change in species biomass under the selected fishing strategy \
relative to the baseline strategy at selected time intervals (¼, ½, end of period). \
 X: species; Y: % change. Red = decrease; blue = increase; shading indicates earlier → later time intervals.
Shading indicates earlier→later time intervals when “Intermediate years” is on."

legends$fishery_yield <-
    "Total yield by species for the selected fishing strategy (default is baseline), broken down by fishing gear. The bars show yields under the selected strategy; the baseline yield is not shown here. X: species; Y: yield. Shading distinguishes earlier → later time intervals when “Intermediate years” is on."

legends$fishery_yield_change <-
    "Yield % change by species relative to the baseline strategy at selected time intervals (¼, ½, final year). X: species; Y: % change. Red = decrease; blue = increase; shading indicates earlier → later time intervals when “Intermediate years” is on."

legends$fishery_length <-
    "Size (length) composition of the catch. Area under the curve reflects the total yield."

# Duplicate role legends for fishery plots
legends$fishery_guild   <- legends$role_guild
legends$fishery_size    <- legends$role_size
legends$fishery_diet    <- legends$role_diet

legends$species_order_help <-
    "Select how you want the species to be ordered on the axis. Options include &quot;Custom&quot; and &quot;Size&quot;. Click the &quot;customise&quot; button to change the custom order."

legends$role_biomass_slider <-
    "Sets the % change to the starting biomass of the selected species. \
For example, +20 increases the starting biomass by 20%."

legends$role_mortality_slider <-
    "Sets the % change to natural mortality for the selected species across all sizes. \
For example, +1 makes mortality 1% higher; −1 makes it 1% lower."
