#' Plot the size distribution of the catch
#'
#' Plots the normalised number density of the catch for a species as a function
#' of either length or weight.
#' Superimposes a plot of the number density of all individuals of the
#' species.
#' @param sim A [mizer::MizerSim] object or a list of such objects.
#' @param species The name of the predator species for which to plot the
#'   mortality.
#' @param gear Optional. The name of a gear. If supplied, only the yield from
#'   this gear will be displayed.
#' @param x_var Determines whether to show the size distribution of the catch as
#'   a function of weight ("Weight") or as a function of length ("Length").
#'   Default is "Weight".
#' @param return_data A boolean value that determines whether the formatted data
#'   used for the plot is returned instead of the plot itself. Default value is
#'   FALSE
#' @return A ggplot2 object, unless `return_data = TRUE`, in which case a
#' data frame with the four
#' variables 'w' or 'l' (depending on `x_var`), 'Catch density', 'Type', 'Species
#' and the second slot is a data frame with the four variables 'w_mat',
#' 'Species', 'y_coord', 'Type' (to plot vertical lines).
#' @export
#' @family plotting functions
#' @seealso [plotting_functions]
plotYieldVsSize <- function(sim, species = NULL, gear = NULL,
                           x_var = c("Weight", "Length"),
                           return_data = FALSE) {
    # Allow both a single sim and a list of sims; coerce to list for processing
    if (inherits(sim, "MizerSim")) {
        sim_list <- list(sim)
        names(sim_list) <- "Current"
    } else if (is.list(sim) &&
               all(vapply(sim, inherits, logical(1), what = "MizerSim"))) {
        sim_list <- sim
        if (is.null(names(sim_list))) {
            names(sim_list) <- paste0("Strategy ", seq_along(sim_list))
            names(sim_list)[1] <- "Current"
        } else if (is.na(names(sim_list)[1]) || names(sim_list)[1] == "") {
            names(sim_list)[1] <- "Current"
        }
    } else {
        stop("Argument 'sim' must be a MizerSim or a list of MizerSim objects.")
    }

    # Baseline parameters from the first sim
    params_base <- mizer::finalParams(sim_list[[1]])

    x_var <- match.arg(x_var)

    if (!is.null(gear)) {
        if (!is.character(gear) || length(gear) != 1) {
            stop("Argument 'gear' must be a single gear name.", call. = FALSE)
        }
        if (!(gear %in% mizer::gear_params(params_base)$gear)) {
            stop("The gear ", gear, " does not exist.")
        }
    }

    sp_base <- mizer::species_params(params_base)
    species_names <- mizer::valid_species_arg(params_base, species)

    get_f_mort <- function(params) {
        if (is.null(gear)) {
            return(mizer::getFMort(params))
        }
        mizer::getFMortGear(params)[gear, , , drop = TRUE]
    }

    # Compute baseline totals with the quadrature scheme used by the model.
    baseline_total <- mizer::sizeIntegral(
        params_base,
        weighting = get_f_mort(params_base),
        min_w = sp_base$w_mat / 100,
        max_w = sp_base$w_max
    )

    # Build combined plot data across sims and species, normalised by baseline totals
    plot_dat <- vector("list", length(sim_list) * length(species_names))
    row <- 1L
    for (iSim in seq_along(sim_list)) {
        sim_i <- sim_list[[iSim]]
        params_i <- mizer::finalParams(sim_i)
        sp_i <- mizer::species_params(params_i)
        f_mort <- get_f_mort(params_i)
        size_window <- mizer::get_size_range_array(
            params_i,
            min_w = sp_i$w_mat / 100,
            max_w = sp_i$w_max
        )
        catch_weight <- mizer::bin_average_weight(
            f_mort * size_window,
            params_i
        )
        range_weight <- mizer::bin_average_weight(size_window * 1, params_i)
        catch_density <- mizer::initialN(params_i) * catch_weight

        for (s_name in species_names) {
            iSpecies <- match(s_name, sp_i$species)
            if (is.na(iSpecies)) {
                stop("Species '", s_name, "' is missing from simulation ",
                     iSim, ".", call. = FALSE)
            }
            a <- sp_i$a[[iSpecies]]
            b <- sp_i$b[[iSpecies]]

            w_sel <- which(range_weight[iSpecies, ] > 0)
            w <- mizer::w(params_i)[w_sel]
            l <- (w / a) ^ (1 / b)
            catch_w <- catch_density[iSpecies, w_sel]

            # Normalise by baseline total of the corresponding species
            total_ref <- baseline_total[[s_name]]
            if (!is.finite(total_ref) || total_ref == 0) {
                # Avoid division by zero; keep zeros
                catch_w <- catch_w * 0
            } else {
                catch_w <- catch_w / total_ref
            }
            # The catch density in l gets an extra factor of dw/dl
            catch_l <- catch_w * b * w / l

            plot_dat[[row]] <- data.frame(
                w = w,
                l = l,
                catch_w = unname(catch_w),
                catch_l = unname(catch_l),
                Strategy = names(sim_list)[iSim],
                Species = factor(s_name, levels = sp_base$species)
            )
            row <- row + 1L
        }
    }
    plot_dat <- do.call(rbind, plot_dat)

    if (x_var == "Weight") {

        # remove length-related columns
        plot_dat <- plot_dat[, -c(2, 4)]
        colnames(plot_dat)[2] <- "Catch density"

        if (return_data) return(plot_dat)

        pl <- ggplot2::ggplot(plot_dat, ggplot2::aes(x = w, y = `Catch density`,
                                                     colour = Strategy)) +
            ggplot2::geom_line() +
            ggplot2::facet_wrap(~Species, scales = "free") +
            ggplot2::labs(x = "Size [g]",
                          y = "Normalised number density [1/g]",
                          colour = "Simulation")

    } else {

        # remove weight-related columns
        plot_dat <- plot_dat[,-c(1,3)]
        colnames(plot_dat)[2] <- "Catch density"

        if (return_data) return(plot_dat)

        pl <- ggplot2::ggplot(plot_dat, ggplot2::aes(x = l, y = `Catch density`,
                                                     colour = Strategy)) +
            ggplot2::geom_line() +
            ggplot2::facet_wrap(~Species, scales = "free") +
            ggplot2::labs(x = "Size [cm]",
                          y = "Number density",
                          colour = "Simulation")

    }

    pl <- pl + ggplot2::theme_minimal(base_size = 16) +
        ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
            panel.spacing.y = grid::unit(1.5, "lines")
        )

    if (!is.null(gear)) {
        pl <- pl + ggplot2::ggtitle(paste("Gear:", gear))
    }
    return(pl)
}

#' @rdname plotYieldVsSize
#' @export
plotlyYieldVsSize <- function(object,
                              species = NULL,
                              gear = NULL,
                              x_var = c("Weight", "Length"),
                              ...) {
  plotly::ggplotly(
    plotYieldVsSize(
      sim = object,
      species = species,
      gear = gear,
      x_var = x_var,
      ...
    ),
    tooltip = c("Catch density", "Strategy", "w", "l")
  )
}
