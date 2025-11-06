#' Compute ordered species list for axis
#'
#' Returns an ordered vector of species names based on a selected strategy:
#' custom order, size-based, or guild-based (falling back to default species list
#' if no `guildparams` provided). Always excludes the "Resource" row.
#'
#' @param default_params Mizer params object containing species_params
#' @param guildparams Optional data.frame with columns Species, Feeding.guild, maxw
#' @param choice One of "Custom", "Size", "Guild"
#' @param custom_order Optional character vector of species for custom choice
#' @return Character vector of ordered species (excluding "Resource")
#' @keywords internal
compute_ordered_species <- function(default_params, guildparams = NULL,
                                    choice = c("Custom", "Size", "Guild"),
                                    custom_order = NULL) {
  choice <- match.arg(choice)

  if (identical(choice, "Guild")) {
    if (!is.null(guildparams)) {
      guild_order <- guildparams |>
        dplyr::group_by(.data$Species, .data$Feeding.guild) |>
        dplyr::slice_max(.data$maxw, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::arrange(factor(.data$Feeding.guild,
                               levels = unique(.data$Feeding.guild))) |>
        dplyr::pull(.data$Species) |>
        unique()
      return(intersect(guild_order, default_params@species_params$species))
    } else {
      return(as.data.frame(default_params@species_params$species) |>
               stats::setNames("sp") |>
               dplyr::filter(.data$sp != "Resource") |>
               dplyr::pull(.data$sp))
    }
  }

  if (identical(choice, "Size")) {
    return(default_params@species_params |>
             dplyr::filter(.data$species != "Resource") |>
             dplyr::arrange(.data$w_mat) |>
             dplyr::pull(.data$species))
  }

  # Custom or default fallback
  if (!is.null(custom_order) && length(custom_order)) {
    return(custom_order)
  }
  as.data.frame(default_params@species_params$species) |>
    stats::setNames("sp") |>
    dplyr::filter(.data$sp != "Resource") |>
    dplyr::pull(.data$sp)
}


