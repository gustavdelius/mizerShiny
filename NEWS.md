# mizerShiny 0.3.1

* Upgraded package to be fully compatible with `mizer (>= 3.3.0)`.
* Refreshed bundled datasets `default_params.rda` and `default_sim.rda` to valid mizer 3.3.0 S4 structures.
* Updated spectra and guild plots to support mizer 3.3.0 density output columns (`Biomass density` and `Number density`).
* Replaced deprecated `setInitialValues()` calls with `finalParams()`.
* Modernized slot accesses to canonical S3 accessor generics (`species_params()`, `gear_params()`, `initialN()`, `initial_effort()`, `w()`, `dw()`, `ext_mort()`, `getColours()`).
* Fixed diet comparison matrix computation in `comparedietmatrix()`.
* Exported and consolidated `app_path()` helper for consistent path resolution in tests and UI.
