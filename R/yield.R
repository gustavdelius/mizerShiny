# Yield dashboard helper used in Shiny app

#' Generate a multi-panel yield dashboard as plotly subplots
#'
#' Builds species-level, gear-level, composition and total-yield plots for one
#' or two simulations, optionally focusing on a highlighted time window.
#'
#' @param NS_sim List of one or two mizer projections
#' @param highlight_times Optional numeric length-2 vector for x-axis zoom
#' @param params Optional MizerParams to derive species colours
#' @return A plotly subplot object
#' @keywords internal
generateYieldDashboard <- function(NS_sim, highlight_times = NULL, params = NULL) {
  nSim <- length(NS_sim)
  yieldList     <- lapply(NS_sim, getYield)
  yieldGearList <- lapply(NS_sim, getYieldGear)

  if (!is.null(highlight_times) && length(highlight_times) == 2) {
    slice_time <- function(arr, keep_idx) {
      d <- length(dim(arr))
      if (d == 2) arr[keep_idx, , drop = FALSE] else if (d == 3) arr[keep_idx, , , drop = FALSE]
    }
    time_idx <- which(dimnames(yieldList[[1]])[[1]] >= highlight_times[1] &
                        dimnames(yieldList[[1]])[[1]] <= highlight_times[2])
    yieldList     <- lapply(yieldList,     slice_time, keep_idx = time_idx)
    yieldGearList <- lapply(yieldGearList, slice_time, keep_idx = time_idx)
  }

  sp_all <- dplyr::bind_rows(lapply(seq_along(yieldGearList), function(i) {
    reshape2::melt(yieldGearList[[i]]) |>
      dplyr::group_by(sp, time) |>
      dplyr::summarise(value = sum(value, na.rm=TRUE), .groups="drop") |>
      dplyr::mutate(sim = paste0("Strategy ", i))
  }))

  species_colors <- NULL
  if (!is.null(params) && !is.null(params@linecolour)) {
    species_colors <- params@linecolour[unique(sp_all$sp)]
    missing_colors <- is.na(species_colors)
    if (any(missing_colors)) {
      extra_colors <- grDevices::rainbow(sum(missing_colors))
      names(extra_colors) <- unique(sp_all$sp)[missing_colors]
      species_colors[missing_colors] <- extra_colors
    }
  }

  spline_plotly <- (
    ggplot2::ggplot(sp_all, ggplot2::aes(time, value, color = sp, linetype = sim)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::labs(x = "Time", y = "Yield") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1)) +
      (if (!is.null(species_colors)) ggplot2::scale_color_manual(values = species_colors) else NULL)
  ) |> plotly::ggplotly()
  if (!is.null(highlight_times) && length(highlight_times) == 2) {
    spline_plotly <- spline_plotly |> plotly::layout(xaxis = list(range = highlight_times))
  }

  gear_all <- dplyr::bind_rows(lapply(seq_along(yieldGearList), function(i) {
    reshape2::melt(yieldGearList[[i]]) |>
      dplyr::group_by(gear, time) |>
      dplyr::summarise(value = sum(value, na.rm=TRUE), .groups="drop") |>
      dplyr::mutate(sim = paste0("Strategy ", i))
  }))
  gearline_plotly <- (
    ggplot2::ggplot(gear_all, ggplot2::aes(time, value, color = gear, linetype = sim)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_line(data = gear_all |>
                           dplyr::group_by(time,sim) |>
                           dplyr::summarise(total = sum(value), .groups = "drop"),
                         ggplot2::aes(time, total, linetype = sim), colour = "grey") +
      ggplot2::labs(x = "Time", y = "Yield") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1))
  ) |> plotly::ggplotly()
  if (!is.null(highlight_times) && length(highlight_times) == 2) {
    gearline_plotly <- gearline_plotly |> plotly::layout(xaxis = list(range = highlight_times))
  }

  compo_all <- dplyr::bind_rows(lapply(seq_along(yieldGearList), function(i) {
    mat <- apply(yieldGearList[[i]], c(2,3), sum)
    reshape2::melt(mat, varnames = c("Gear","Species"), value.name = "Yield") |>
      dplyr::mutate(sim = paste0("Strategy ", i))
  }))
  compo_summ <- compo_all |>
    dplyr::group_by(Species, sim) |>
    dplyr::summarise(total = sum(Yield), .groups="drop")

  compo_plotly <- (
    ggplot2::ggplot(compo_summ, ggplot2::aes(x = Species, y = total, fill = Species, alpha = sim)) +
      ggplot2::geom_bar(stat = "identity", width = 0.5,
                         position = ggplot2::position_dodge2(width = 1.0, preserve = "single", padding = 0)) +
      ggplot2::scale_alpha_manual(values = c("Strategy 1" = 1, "Strategy 2" = 0.4)) +
      ggplot2::labs(x = "Species", y = "Yield", fill = "Species", alpha = "Strategy") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      (if (!is.null(species_colors)) ggplot2::scale_fill_manual(values = species_colors) else NULL)
  ) |>
    plotly::ggplotly() |>
    plotly::layout(barmode = "group", bargap = 0.01)

  singular_all <- dplyr::bind_rows(lapply(seq_along(yieldList), function(i) {
    tot <- apply(yieldGearList[[i]], 2, sum)
    data.frame(sim = paste0("Strategy ", i), Species = dimnames(yieldList[[i]])[[2]], Yield = tot)
  }))
  singular_plotly <- (
    ggplot2::ggplot(singular_all, ggplot2::aes(x = sim, y = Yield, fill = Species)) +
      ggplot2::geom_col() +
      ggplot2::labs(x = "Strategy", y = "Total Yield", fill = "Species") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1)) +
      (if (!is.null(species_colors)) ggplot2::scale_fill_manual(values = species_colors) else NULL)
  ) |> plotly::ggplotly()

  pieList <- lapply(seq_along(yieldList), function(i) {
    df <- data.frame(Category = dimnames(yieldList[[i]])[[2]],
                     Value    = apply(yieldGearList[[i]], 2, sum))
    headRoom <- 0.12
    dom <- if (nSim == 1) list(x = c(0.70, 1.00), y = c(0, 1.00 - headRoom))
           else if (i == 1) list(x = c(0.70, 1.00), y = c(0.55, 1.00 - headRoom/2))
           else              list(x = c(0.70, 1.00), y = c(0.00, 0.45 - headRoom/2))

    pie_colors <- NULL
    if (!is.null(species_colors)) {
      pie_colors <- species_colors[df$Category]
      missing_pie_colors <- is.na(pie_colors)
      if (any(missing_pie_colors)) {
        extra_pie_colors <- grDevices::rainbow(sum(missing_pie_colors))
        names(extra_pie_colors) <- df$Category[missing_pie_colors]
        pie_colors[missing_pie_colors] <- extra_pie_colors
      }
    }
    plotly::plot_ly(df, labels = ~Category, values = ~Value, type = "pie", domain = dom,
                    marker = list(colors = pie_colors)) |>
      plotly::layout(legend = list(title = list(text = "Species")))
  })

  left_col <- plotly::subplot(
    plotly::subplot(spline_plotly, gearline_plotly, nrows = 1, shareY = FALSE, titleY = TRUE, margin = 0.05),
    plotly::subplot(compo_plotly, singular_plotly, nrows = 1, margin = 0.05),
    nrows = 2, margin = 0.05
  )
  right_col <- if (nSim == 1) pieList[[1]] else plotly::subplot(pieList[[1]], pieList[[2]], nrows = 2,
                                                                heights = c(0.45, 0.45), margin = 0.05)

  annotations <- list(
    list(text = "Species Change",      x = 0.085,  y = 1.06, xref = "paper", yref = "paper", showarrow = FALSE),
    list(text = "Gear Change",         x = 0.512, y = 1.06, xref = "paper", yref = "paper", showarrow = FALSE),
    list(text = "Yield Composition",   x = 0.075,  y = 0.48, xref = "paper", yref = "paper", showarrow = FALSE),
    list(text = "Total Yield", x = 0.512, y = 0.48, xref = "paper", yref = "paper", showarrow = FALSE)
  )
  fig <- plotly::subplot(left_col, right_col, nrows = 1, widths = c(0.7, 0.3), margin = 0.05) |>
    plotly::layout(annotations = annotations)

  domains <- purrr::keep(fig$x$data, ~ .$type == "pie") |> purrr::map(~ .$domain)
  labs <- purrr::imap(domains, function(dom, idx) {
    if (nSim == 1) { x_off <- 0.05; y_off <- 0.06 }
    else if (idx == 1) { x_off <- 0.05; y_off <- 0.08 } else { x_off <- 0.05; y_off <- 0.04 }
    list(text = paste0("Strategy ", idx, " composition"), x = mean(dom$x) + x_off, y = dom$y[2] + y_off,
         xref = "paper", yref = "paper", showarrow = FALSE, font = list(size = 14))
  })
  fig |> plotly::layout(annotations = labs)
}


