intro_steps <- list(

    # ============================
    # STRATEGY SELECTION / EDITING
    # ============================
    list(
        # Tab pill for Strategy 1 (link element created by tabPanel(title = "Strategy 1", ...))
        element = "a.nav-link[data-value='Strategy 1'], a[data-value='Strategy 1']",
        title   = "Strategy 1",
        intro   = "Use this tab to configure and view Strategy 1 (effort sliders below)."
    ),
    list(
        # Tab pill for Strategy 2
        element = "a.nav-link[data-value='Strategy 2'], a[data-value='Strategy 2']",
        title   = "Strategy 2",
        intro   = "Configure a second strategy so you can compare Strategy 1 and Strategy 2, or examine a second strategy in isolation without altering the first."
    ),
    list(
        # Radio group: Show Strategy 1 / Strategy 2 / Both
        element = "[id$='sim_choice']",
        title   = "Show which strategies?",
        intro   = "Choose to display Strategy 1, Strategy 2, or Both together."
    ),

    # ============================
    # MULTISPECIES & TIME CONTROLS
    # ============================
    list(
        # materialSwitch – label is a visible, stable anchor
        element = "[for$='multispeciesToggle'], [id$='multispeciesToggle']",
        title   = "Multispecies effects",
        intro   = "On = interacting community (predation & competition feedbacks). Off = non‑interacting species. Toggling re‑runs the baseline and both strategies for the selected time range."
    ),
    list(
        element = "[id$='fishyyear']",
        title   = "Time range",
        intro   = "Sets the end year used in plots. If “Show intermediate years” is on, charts also include the ¼ and ½ points from model start to this year."
    ),
    list(
        element = "[id$='incYear_fish']",
        title   = "+1 year",
        intro   = "Step the selected end year forward by one."
    ),

    # ============================
    # PLOT TABS (as visible in your UI)
    # ============================
    list(
        element = "a.nav-link[data-value='Biomass'], a[data-value='Biomass']",
        title   = "Biomass",
        intro   = "Species biomass under the selected fishing strategy. Use the legend to focus on species; adjust the time range to see how outcomes evolve."
    ),
    list(
        element = "a.nav-link[data-value='Biomass % Change'], a[data-value='Biomass % Change']",
        title   = "Biomass % change",
        intro   = "Change relative to the baseline (uploaded model). When “Show intermediate years” is on, shading shows earlier→later time points."
    ),
    list(
        element = "a.nav-link[data-value='Yield'], a[data-value='Yield']",
        title   = "Yield",
        intro   = "Catch by species and gear under the selected strategy. Use Gear and Species filters to drill down."
    ),
    list(
        element = "a.nav-link[data-value='Yield % Change'], a[data-value='Yield % Change']",
        title   = "Yield % change",
        intro   = "Change in catch relative to the baseline (uploaded model). Positive = more catch; negative = less."
    ),
    list(
        element = "a.nav-link[data-value='Nutrition'], a.nav-link[data-value='Nutrition change'], a[data-value='Nutrition'], a[data-value='Nutrition change']",
        title   = "Nutrition",
        intro   = "Change in nutrient availability from the catch (e.g., protein, omega‑3) relative to the baseline."
    ),
    list(
        element = "a.nav-link[data-value='Length'], a[data-value='Length']",
        title   = "Catch size composition",
        intro   = "How the size distribution of the catch shifts under the strategy. Total area reflects total yield."
    ),
    list(
        element = "a.nav-link[data-value='Guild'], a[data-value='Guild']",
        title   = "Feeding guilds",
        intro   = "Aggregate responses by feeding guild to see community‑level trade‑offs."
    ),

    # ============================
    # PAGE FOOTER CONTROL
    # ============================
    list(
        # For materialSwitch, the input (checkbox) is visible and reliable in your build.
        # Using the input id itself gives the guide a solid anchor next to the toggle.
        element = "input[type='checkbox'][id$='fishy_intermediate_toggle'], [for$='fishy_intermediate_toggle']",
        title   = "Show intermediate years",
        intro   = "Add ¼ and ½ time points (between model start and the selected end year) to show the trajectory, not just the end result. Under the baseline strategy, Biomass and Yield will be the same height at each time point"
    ),

    # ============================
    # BASELINE
    # ============================
    list(
        element = ".baseline-definition, [id$='baselineDefinition'], [id$='whatIsBaseline']",
        title   = "What is the baseline?",
        intro   = "Baseline = the default parameter with all gears at initial effort (i.e. effort = 1). All % changes are calculated relative to this baseline."
    )
)
