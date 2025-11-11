#' Make model non-interacting
#'
#' Adds the predation mortality to the external mortality and the predation
#' encounter to the external encounter and then sets the interaction matrix to
#' zero and switches off the interaction with the resource, so that the model
#' becomes non-interacting.
#'
#' @param params A MizerParams object
#' @return The modified MizerParams object
#' @keywords internal
make_noninteracting <- function(params) {

    # Put predation mortality into external mortality
    ext_mort(params) <- ext_mort(params) + getPredMort(params)

    # Put predation encounter into external encounter
    # We make the assumption that all of the encounter rate is from
    # predation and ext_encounter. We need to do this because we do not have
    # a way to calculate the encounter specifically from predation. There is
    # no getPredEncounter() function.
    ext_encounter(params) <- getEncounter(params)

    # Set the interaction matrix to zero
    interaction_matrix(params)[] <- 0
    species_params(params)$interaction_resource <- 0

    return(params)
}
