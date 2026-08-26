# mizerShiny 0.3.3

## Upgrade to mizer 3.3.0

* **mizer compatibility**: Upgraded `mizerShiny` to be fully compatible with `mizer (>= 3.3.0)`.
* **Bundled datasets**: Re-saved and upgraded `default_params.rda` and `default_sim.rda` using `validParams()` and `validSim()` to populate all new mizer 3.3.0 S4 slots (such as `@second_order_w`) and species parameter table columns (`is_background`).
* **Spectra & guild plots**: Updated `guildplot()`, `guildplot_both()`, `plotSpectraRelative()`, `plotSpectraRelative2()`, and `plotSpectra2()` to handle mizer 3.3.0 density output columns (`Biomass density` / `Number density`) returned by `plotSpectra(..., return_data = TRUE)`.
* **API modernization**:
  * Replaced deprecated `setInitialValues()` calls with `mizer::finalParams()`.
  * Replaced direct S4 slot manipulations (`@species_params`, `@gear_params`, `@initial_n`, `@initial_effort`, `@linecolour`, `@w`, `@dw`) with canonical S3 accessors (`species_params()`, `gear_params()`, `initialN()`, `initial_effort()`, `getColours()`, `w()`, `dw()`, `ext_mort()`).
* **Bug fixes**:
  * Fixed diet comparison calculation in `comparedietmatrix()` to evaluate against `harvestedprojection` instead of `sim_0`.
  * Exported and consolidated `app_path()` helper in `R/ui_helpers.R` for consistent file resolution.
  * Added documentation for shipped datasets in `R/data.R`.
  * Updated test suite and visual snapshots for mizer 3.3.0.


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
