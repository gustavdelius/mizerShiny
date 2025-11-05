plotDietCompare <- function(objects, species = NULL, sim_names = NULL) {

  diet_long <- function(obj, idx, species) {
    d <- getDiet(obj@params,
                 n       = apply(obj@n,      2:3, mean),
                 n_pp    = apply(obj@n_pp,   2,   mean),
                 n_other = apply(obj@n_other,2,   mean))
    d <- d[dimnames(d)[[1]] %in% species, , , drop = FALSE]
    names(dimnames(d)) <- c("Predator", "w", "Prey")

    df <- reshape2::melt(d, value.name = "Proportion")
    df <- subset(df, Proportion > 0.001)
    df$w   <- as.numeric(as.character(df$w))
    df$Sim <- if (!is.null(sim_names) && length(sim_names) >= idx)
      sim_names[idx] else paste0("Sim ", idx)
    df
  }

  plot_dat <- dplyr::bind_rows(
    lapply(seq_along(objects), function(i) diet_long(objects[[i]], i, species))
  )
  if (nrow(plot_dat) == 0) return(NULL)

  params      <- objects[[1]]@params
  # Use a fixed global order for prey based on params@linecolour to keep colors consistent
  prey_levels <- names(params@linecolour)
  plot_dat$Prey <- factor(plot_dat$Prey, levels = prey_levels)

  col_vec <- params@linecolour
  if (any(is.na(col_vec))) {
    extra <- grDevices::rainbow(sum(is.na(col_vec)))
    names(extra) <- prey_levels[is.na(col_vec)]
    col_vec[is.na(col_vec)] <- extra
  }
  # Ensure vector order aligns with factor levels
  col_vec <- col_vec[prey_levels]

  sims   <- unique(plot_dat$Sim)
  full_panels <- lapply(sims, function(sim_lab) {
    sub <- plot_dat[plot_dat$Sim == sim_lab, ]
    miss <- setdiff(prey_levels, sub$Prey)
    if (length(miss)) {
      dummy <- data.frame(
        Predator   = species[1],
        w          = min(sub$w),
        Prey       = factor(miss, levels = prey_levels),
        Proportion = 0,
        Sim        = sim_lab
      )
      sub <- rbind(sub, dummy)
    }
    sub[order(sub$Prey, sub$w), ]
  })

  panels <- lapply(seq_along(sims), function(i) {
    sub <- full_panels[[i]]
    # Build panel by adding one trace per prey with fixed colours
    plt <- plotly::plot_ly()
    for (prey in prey_levels) {
      prey_col <- unname(col_vec[prey])
      prey_sub <- sub[sub$Prey == prey, , drop = FALSE]
      if (nrow(prey_sub) == 0) {
        # ensure presence in legend with a zero area at min w if missing
        if (nrow(sub) > 0) {
          prey_sub <- data.frame(
            w = min(sub$w),
            Proportion = 0,
            Prey = factor(prey, levels = prey_levels)
          )
        } else {
          next
        }
      }
      plt <- plotly::add_trace(
        plt,
        data        = prey_sub,
        x           = ~w,
        y           = ~Proportion,
        name        = prey,
        type        = "scatter",
        mode        = "lines",
        stackgroup  = "one",
        fill        = "tonexty",
        legendgroup = prey,
        showlegend  = (i == 1),
        line        = list(color = prey_col, width = 0),
        fillcolor   = prey_col
      )
    }
    plt |>
      plotly::layout(
        xaxis = list(
          type        = "log",
          title       = list(
            text     = if (i == length(sims)) "Size (g)" else "",
            standoff = 20
          ),
          ticklen     = 10,
          automargin  = TRUE,
          showgrid    = TRUE,
          zeroline    = FALSE
        ),
        yaxis = list(
          title      = list(
            text     = "Proportion",
            standoff = 20
          ),
          ticklen = 10, tickcolor = "transparent",
          automargin = TRUE,
          showgrid   = TRUE,
          zeroline   = FALSE
        ),
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  })

  fig <- plotly::subplot(
    panels,
    nrows  = length(panels),
    shareX = TRUE,
    shareY = TRUE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.02
  )

  n <- length(sims)
  annotation_list <- lapply(seq_along(sims), function(i) {
    top <- if (i == 1) 0.99 else 0.47
    list(
      x        = 0.5,
      y        = top,
      xref     = "paper",
      yref     = "paper",
      text     = sims[i],
      showarrow= FALSE,
      xanchor  = "center",
      yanchor  = "bottom",
      font     = list(size = 14)
    )
  })

  fig |>
    plotly::layout(
      annotations = annotation_list,
      legend      = list(title = list(text = "Prey")),
      margin      = list(t = 30)  # increased bottom margin for x-axis labels
    )
}
