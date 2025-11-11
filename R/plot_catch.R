#' Plot the size distribution of the catch
#'
#' Plots the normalised number density of the catch for a species as a function
#' of either length or weight.
#' Superimposes a plot of the number density of all individuals of the
#' species.
#' @param sim An object of class \linkS4class{MizerSim}
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
    if (methods::is(sim, "MizerSim")) {
        sim_list <- list(sim)
        names(sim_list) <- "Current"
    } else if (is.list(sim)) {
        sim_list <- sim
        if (is.null(names(sim_list))) {
            names(sim_list) <- paste0("Strategy ", seq_along(sim_list))
        }
        # Ensure the first entry is labelled "Current" for consistency in plotting
        names(sim_list)[1] <- ifelse(is.na(names(sim_list)[1]) || names(sim_list)[1] == "", "Current", names(sim_list)[1])
    } else {
        stop("Argument 'sim' must be a MizerSim or a list of MizerSim objects.")
    }

    # Baseline parameters from the first sim
    params_base <- sim_list[[1]]@params
    params_base <- setInitialValues(params_base, sim_list[[1]])

    x_var = match.arg(x_var)

    if (!is.null(gear)) {
        assert_that(is.character(gear),
                    length(gear) == 1)
        if (!(gear %in% params_base@gear_params$gear)) {
            stop("The gear ", gear, " does not exist.")
        }
    }

    SpIdx <- factor(params_base@species_params$species,
                    levels = params_base@species_params$species)
    species <- valid_species_arg(params_base, species)
    species <- which(params_base@species_params$species %in% species)

    # Ensure defaults exist on baseline for length-weight conversion; will be re-applied per sim
    params_base <- set_species_param_default(params_base, "a", 0.006)
    params_base <- set_species_param_default(params_base, "b", 3)

    # Compute baseline totals per species for normalisation
    baseline_total <- numeric(length(species))
    names(baseline_total) <- params_base@species_params$species[species]
    for (idx in seq_along(species)) {
        iSpecies <- species[[idx]]
        a_b <- params_base@species_params[iSpecies, "a"]
        b_b <- params_base@species_params[iSpecies, "b"]

        w_min_idx <- sum(params_base@w < (params_base@species_params$w_mat[[iSpecies]] / 100))
        w_max_idx <- sum(params_base@w <= params_base@species_params$w_max[[iSpecies]])
        w_sel <- seq(w_min_idx, w_max_idx, by = 1)

        if (is.null(gear)) {
            f_mort_b <- getFMort(params_base)[iSpecies, w_sel]
        } else {
            f_mort_b <- getFMortGear(params_base)[gear, iSpecies, w_sel]
        }
        catch_w_b <- f_mort_b * params_base@initial_n[iSpecies, w_sel]
        baseline_total[[idx]] <- sum(catch_w_b * params_base@dw[w_sel])
    }

    # Build combined plot data across sims and species, normalised by baseline totals
    plot_dat <- NULL
    for (iSim in seq_along(sim_list)) {
        sim_i <- sim_list[[iSim]]
        params_i <- sim_i@params
        params_i <- setInitialValues(params_i, sim_i)
        params_i <- set_species_param_default(params_i, "a", 0.006)
        params_i <- set_species_param_default(params_i, "b", 3)

        for (idx in seq_along(species)) {
            iSpecies <- species[[idx]]
            s_name <- params_base@species_params$species[[iSpecies]]
            a <- params_i@species_params[iSpecies, "a"]
            b <- params_i@species_params[iSpecies, "b"]

            w_min_idx <- sum(params_i@w < (params_i@species_params$w_mat[[iSpecies]] / 100))
            w_max_idx <- sum(params_i@w <= params_i@species_params$w_max[[iSpecies]])

            w_sel <- seq(w_min_idx, w_max_idx, by = 1)
            w <- params_i@w[w_sel]
            l <- (params_i@w[w_sel] / a) ^ (1 / b)

            if (is.null(gear)) {
                f_mort <- getFMort(params_i)[iSpecies, w_sel]
            } else {
                f_mort <- getFMortGear(params_i)[gear, iSpecies, w_sel]
            }
            catch_w <- f_mort * params_i@initial_n[iSpecies, w_sel]

            # Normalise by baseline total of the corresponding species
            total_ref <- baseline_total[[idx]]
            if (total_ref == 0) {
                # Avoid division by zero; keep zeros
                catch_w <- catch_w * 0
            } else {
                catch_w <- catch_w / total_ref
            }
            # The catch density in l gets an extra factor of dw/dl
            catch_l <- catch_w * b * w / l

            df <- data.frame(w = w,
                             l = l,
                             catch_w = catch_w,
                             catch_l = catch_l,
                             Strategy = names(sim_list)[iSim],
                             Species = SpIdx[which(s_name == SpIdx)])
            plot_dat <- rbind(plot_dat, df)
        }
    }

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
        pl <- pl + ggtitle(paste("Gear:", gear))
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
  argg <- c(as.list(environment()), list(...))
  ggplotly(do.call("plotYieldVsSize", argg),
           tooltip = c("Catch density", "Strategy", "w", "l"))
}
