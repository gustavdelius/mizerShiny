legends <- list(
  role_biomass =
    "Change in biomass of each species at specific time points. The X axis shows \
the species and the Y axis is the percentage change in its biomass compared \
with the base simulation. Each species has three bars representing biomass at \
quarter, half and full of the selected year. Red/blue indicate direction; bright &rarr; dark shows \
quarter/half/full time points.",
  role_size = # Will also be used for fishery_size
    "Change in the community size spectrum in comparison to the base \
simulation. The community size spectrum is the biomass of all species in the \
ecosystem at a given size. The blue line is the changed simulation; the dashed \
line is the base simulation. Use the <em>Log</em> switch to change the \
X&nbsp;axis from log-scale to linear.",
  role_guild = # Will also be used for fishery_guild
    "Change in feeding guilds across the entire community compared with the \
base model. X axis is the guild, Y is the % change. Each group of three\
bars represents biomass at quarter, half and full of the selected year \
(bright &rarr; dark). Feeding guilds \
group fish by diet and life stage.",
  role_diet = # Will also be used for fishery_diet
      "Diet composition of a selected fish across its size range. X axis is the \
    size of fish, Y axis is the proportion of diet of a prey species. Colour \
    indicates prey species.",
  fishery_biomass = "To be written.",
  fishery_biomass_change = "To be written.",
  fishery_yield = "To be written.",
  fishery_yield_change = "To be written.",
  fishery_nutrition =
      "Relative change in nutrition compared with the current fishing strategy. \
X axis contains the nutrient, Y axis is the % change in comparison with the \
  current fishing strategy. Colour indicates whether change is positive or \
  negative. Nutrient amount is calculated from biomass of species caught.",
  fishery_length = "To be written.",
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
    "When on, bars also show earlier responses at ¼ and ½ of the selected period, alongside the value at its end."
legends$role_log <-
    "Switches the body-size axis to logarithmic. Useful when spectra span orders of magnitude."

legends$fishery_show_intermediate_years <-
    "When on, bars also show earlier responses at ¼ and ½ of the selected period, alongside the value at its end."
legends$fishery_log <-
    "Switches the body-size axis to logarithmic for spectra. Turn off for a linear size axis."

legends$fishery_biomass <-
    "Absolute biomass by species for the selected fishing strategy (default is baseline) at selected time points. X: species; Y: biomass. Shading distinguishes earlier → later time points."

legends$fishery_biomass_change <-
    "Biomass % change by species relative to the baseline strategy at selected time points (¼, ½, end of period). X: species; Y: % change. Red = decrease; blue = increase; shading indicates earlier → later time points."

legends$fishery_yield <-
    "Absolute yield by species for the selected fishing strategy (default is baseline) at selected time points. X: species; Y: yield. Shading distinguishes earlier → later time points."

legends$fishery_yield_change <-
    "Yield % change by species relative to the baseline strategy at selected time points (¼, ½, end of period), broken down by fishing gear. X: species; Y: % change. Red = decrease; blue = increase; shading indicates earlier → later time points."

legends$fishery_length <-
    "Length (size) composition of the catch. Select species/gear and compare current vs baseline where available. Compositions are not re-normalised; total area varies with yield."

# Duplicate role legends for fishery plots
legends$fishery_guild   <- legends$role_guild
legends$fishery_size    <- legends$role_size
legends$fishery_diet    <- legends$role_diet
