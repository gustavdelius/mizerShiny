intro_steps <- list(

    # ============================
    # Guide overview
    # ============================
    list(
        element = ".baseline-definition, [id$='baselineDefinition'], [id$='whatIsBaseline']",
        title   = "What is this app for?",
        intro   = "Explore how different fishing strategies change the modelled community over time.
Investigate one strategy or compare two side-by-side, and see the effects of changing fishing effort on biomass, yield, size composition of landings, nutrition of landings, and feeding guild structures."
    ),

    # (optional next click if you want the baseline explained separately)
    list(
        element = ".baseline-definition, [id$='baselineDefinition'], [id$='whatIsBaseline']",
        title   = "The baseline strategy",
        intro   = "The baseline against which relative changes are compared to is the uploaded mizer model called with mizerShiny(), or the default Celtic Sea model if no model is provided.
All percentage changes in the app are calculated relative to this baseline, not to the initial yield after you move the effort sliders. You can return to the baseline strategy by setting all effort sliders to 1 or pressing Reset."
    ),

    # ============================
    # STRATEGY SELECTION / EDITING
    # ============================
    list(
        # Tab pill for Strategy 1 (link element created by tabPanel(title = "Strategy 1", ...))
        element = "a.nav-link[data-value='Strategy 1'], a[data-value='Strategy 1']",
        title   = "Strategy 1",
        intro   = "Use this tab to configure the effort of each gear in Strategy 1, using the sliders below."
    ),
    list(
        # Tab pill for Strategy 2
        element = "a.nav-link[data-value='Strategy 2'], a[data-value='Strategy 2']",
        title   = "Strategy 2",
        intro   = "Configure a second strategy so you can compare Strategy 1 and Strategy 2."
    ),
    list(
        # Radio group: Show Strategy 1 / Strategy 2 / Both
        element = "[id$='sim_choice']",
        title   = "Show which strategies?",
        intro   = "Choose to display Strategy 1, Strategy 2, or both together."
    ),

    # ============================
    # MULTISPECIES & TIME CONTROLS
    # ============================
    list(
        # materialSwitch – label is a visible, stable anchor
        element = "[for$='multispeciesToggle'], [id$='multispeciesToggle']",
        title   = "Multispecies effects",
        intro   = "Use to enable or disable predation between (and within) each species.
        This allows you to examine the differences between a set of single-species models, and a fully interacting multispecies model."
    ),
    list(
        element = "[id$='fishyyear']",
        title   = "Time range",
        intro   = "Sets the number of years to project the chosen strategy/strategies forward. Under the baseline scenario biomasses and yields will not change as the system is an unchanging (steady) state. If “Show intermediate years” is on, plots also include the ¼ and ½ intervals from model start to the projected year."
    ),

#    list(
#        element = "[id$='decYear_fish']",
#        title   = "-1 year",
#        intro   = "Step the selected end year backward by one for finer control."
#    ),
#    list(
#        element = "[id$='incYear_fish']",
#        title   = "+1 year",
#        intro   = "Step the selected end year forward by one for finer control."
#    ),

    # ============================
    # PLOT TABS (as visible in your UI)
    # ============================
    list(
        element = "a.nav-link[data-value='Biomass'], a[data-value='Biomass']",
        title   = "Biomass",
        intro   = "This tab plots the total biomass of each species at the initial, intermediate (if shown), and final years for the selected fishing strategy/strategies. Baseline and final year biomasses are plotted."
    ),
    list(
        element = "a.nav-link[data-value='Biomass % Change'], a[data-value='Biomass % Change']",
        title   = "Biomass % change",
        intro   = "Change in biomass relative to the baseline. Useful when changes in biomass are difficult to observe due to axis scaling."
    ),
    list(
        element = "a.nav-link[data-value='Yield'], a[data-value='Yield']",
        title   = "Yield",
        intro   = "This tab plots the total yield of each species at the initial, intermediate (if shown), and final years for the selected fishing strategy/strategies. The first bar is the yield at the start of the scenario after changing effort, not the baseline yield. Colour denotes gear category."
    ),
    list(
        element = "a.nav-link[data-value='Yield % Change'], a[data-value='Yield % Change']",
        title   = "Yield % change",
        intro   = "Change in yield relative to the baseline. Bars show how much higher or lower simulated yield is at the plotted time intervals compared with the baseline. Useful when changes in yield are difficult to observe due to axis scaling."
    ),
    list(
        element = "a.nav-link[data-value='Nutrition'], a.nav-link[data-value='Nutrition change'], a[data-value='Nutrition'], a[data-value='Nutrition change']",
        title   = "Nutrition",
        intro   = "This tab plots the % change in the nutrient content of the yield compared to the baseline."
    ),
    list(
        element = "a.nav-link[data-value='Length'], a[data-value='Length']",
        title   = "Catch size composition",
        intro   = "This tab plots the size distribution of each landed species . If Stategy 1 and/or 2 are defined, their distributions are also plotted and scale with the total yield."
    ),
    list(
        element = "a.nav-link[data-value='Guild'], a[data-value='Guild']",
        title   = "Feeding guilds",
        intro   = "This tab visualises the relative biomass change in each of four feeding guilds, defined by both species and size. For example, small hake tend to be planktivores, large hake tend to be piscivores"
    ),

    # ============================
    # PAGE FOOTER CONTROL
    # ============================
    list(
        # For materialSwitch, the input (checkbox) is visible and reliable in your build.
        # Using the input id itself gives the guide a solid anchor next to the toggle.
        element = "input[type='checkbox'][id$='triplotToggleFish'], [for$='triplotToggleFish']",
        title   = "Show intermediate years",
        intro   = "Add ¼ and ½ time intervals (between the baseline and the final year) to show the trajectory of the community, not just the final year result. Under the baseline strategy, Biomass and Yield will be the same height at each time point, "
    )

)
