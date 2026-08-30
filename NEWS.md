# mizerShiny 0.4.0

- Require mizer 3.4.0 and upgrade the bundled parameter and simulation data to
  mizer's S3 object representation.
- Replace deprecated `setInitialValues()` calls with `finalParams()`.
- Make catch-size densities and their normalisation follow the model's active
  size quadrature, including second-order bin averaging.
- Use mizer accessors for model changes and simulation time-window extraction.
- Remove local weight-length defaults in favour of mizer 3.4's validated
  species parameters.
