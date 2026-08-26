# mizerShiny 0.3.0

## Celtic Sea Model & Strategy Enhancements

* **Celtic Sea model**: Switched default model from North Sea to Celtic Sea (`default_params`, `default_sim`, `default_guildparams`, `default_nutrition`).
* **Fishery Strategy module**:
  * Added dynamic effort sliders for Strategy 1 and Strategy 2 with individual "Reset" buttons.
  * Added interactive toggle for multispecies trophic interaction effects.
  * Added nutrition tab to calculate and plot nutrient yields per tonne with informative tooltips and legends.
  * Added "Length" tab with `plotYieldVsSize()` and `plotlyYieldVsSize()` for catch size distribution analysis.
  * Added `mizerShinyAllTabs()` to launch the app with all legacy and extended tabs.
* **UI & Help System**:
  * Interactive page tour guide using `rintrojs`.
  * Added explanatory popovers and tooltips for all controls, tabs, and plots.
  * Added custom and flexible species ordering controls (Custom, Size, Guild).
  * Harmonized color palettes and layout styling across all tabs.


# mizerShiny 0.2.0

## Architecture & Refactoring

* **Modularization**: Refactored Shiny app into modular architecture (`species_role_module` and `fishery_strategy_module`).
* **Dependency cleanup**: Pruned unused package dependencies (`forcats`, `here`, `patchwork`, `shinyBS`, `thematic`).
* **Modern R syntax**: Adopted native R pipe operator (`|>`) throughout the codebase.
* **Projection efficiency**: Optimized simulation stepping to extend projections on-demand rather than re-running from scratch.


# mizerShiny 0.1.1

## Improvements & Bug Fixes

* Improved plot layout and styling on different screen resolutions.
* Updated documentation and vignette guides.


# mizerShiny 0.1.0

## Initial Release

* Initial release of `mizerShiny`, an interactive Shiny web application for exploring and simulating `mizer` size-spectrum ecosystem models.
* Features single-species perturbation analysis (biomass and mortality adjustments) and multi-species fishery comparisons.
* Visualizations including biomass time series, community size spectra, feeding guild distributions, and diet interaction matrices.
* Interactive plots built on `ggplot2` and `plotly`.
