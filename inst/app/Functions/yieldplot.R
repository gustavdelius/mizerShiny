generateYieldDashboard <- function(NS_sim,
                               highlight_times = NULL,
                               params = NULL) {
  nSim <- length(NS_sim)

  yieldList     <- lapply(NS_sim, getYield)
  yieldGearList <- lapply(NS_sim, getYieldGear)

  if (!is.null(highlight_times) && length(highlight_times) == 2) {

    t_min <- highlight_times[1]
    t_max <- highlight_times[2]

    slice_time <- function(arr, keep_idx) {
      d <- length(dim(arr))
      if (d == 2) {
        arr[keep_idx, , drop = FALSE]
      } else if (d == 3) {
        arr[keep_idx, , , drop = FALSE]
      }

    }

    time_idx <- which(
      dimnames(yieldList[[1]])[[1]] >= highlight_times[1] &
        dimnames(yieldList[[1]])[[1]] <= highlight_times[2]
    )

    yieldList     <- lapply(yieldList,     slice_time, keep_idx = time_idx)
    yieldGearList <- lapply(yieldGearList, slice_time, keep_idx = time_idx)
  }

  sp_all <- bind_rows(lapply(seq_along(yieldGearList), function(i) {
    melt(yieldGearList[[i]]) %>%
      group_by(sp, time) %>%
      summarise(value = sum(value, na.rm=TRUE), .groups="drop") %>%
      mutate(sim = paste0("Sim ", i))
  }))
  
  # Use linecolour from params for consistent species colors
  if (!is.null(params) && !is.null(params@linecolour)) {
    species_colors <- params@linecolour[unique(sp_all$sp)]
    # Handle any missing colors
    missing_colors <- is.na(species_colors)
    if (any(missing_colors)) {
      extra_colors <- grDevices::rainbow(sum(missing_colors))
      names(extra_colors) <- unique(sp_all$sp)[missing_colors]
      species_colors[missing_colors] <- extra_colors
    }
  } else {
    species_colors <- NULL
  }
  
  spline_plotly <- (
    ggplot(sp_all, aes(time, value, color = sp, linetype = sim)) +
      geom_line(linewidth = 1) +
      labs(x = "Time", y = "Yield") +
      theme_minimal() +
      theme(axis.text.x = element_text(hjust = 1)) +
      {if (!is.null(species_colors)) scale_color_manual(values = species_colors) else NULL}
  ) %>%
    ggplotly() %>%
    { if (!is.null(highlight_times) && length(highlight_times) == 2)
      layout(., xaxis = list(range = highlight_times))
      else
        .
    }

  # 2) Yield over time by gear
  gear_all <- bind_rows(lapply(seq_along(yieldGearList), function(i) {
    melt(yieldGearList[[i]]) %>%
      group_by(gear, time) %>%
      summarise(value = sum(value, na.rm=TRUE), .groups="drop") %>%
      mutate(sim = paste0("Sim ", i))
  }))
  gearline_plotly <- (
    ggplot(gear_all, aes(time, value, color = gear, linetype = sim)) +
      geom_line(linewidth = 1) +
      geom_line(
        data = gear_all %>% group_by(time,sim) %>% summarise(total = sum(value)),
        aes(time, total, linetype = sim), colour = "grey"
      ) +
      labs(x = "Time", y = "Yield") +
      theme_minimal() +
      theme(axis.text.x = element_text(hjust = 1))
  ) %>%
    ggplotly() %>%
    { if (!is.null(highlight_times) && length(highlight_times) == 2)
      layout(., xaxis = list(range = highlight_times))
      else
        .
    }

  # 3) Bar plots - Yield Composition by Species (not gear)
  compo_all <- bind_rows(lapply(seq_along(yieldGearList), function(i) {
    mat <- apply(yieldGearList[[i]], c(2,3), sum)
    melt(mat, varnames = c("Gear","Species"), value.name = "Yield") %>%
      mutate(sim = paste0("Sim ", i))
  }))
  compo_summ <- compo_all %>%
    group_by(Species, sim) %>%
    summarise(total = sum(Yield), .groups="drop")

  compo_plotly <- (
    ggplot(compo_summ, aes(x = Species, y = total, fill = Species, alpha = sim)) +
      geom_bar(
        stat     = "identity",
        width    = 0.5,
        position = position_dodge2(width = 1.0, preserve = "single", padding = 0)
      ) +
      scale_alpha_manual(values = c("Sim 1" = 1, "Sim 2" = 0.4)) +
      labs(x = "Species", y = "Yield", fill = "Species", alpha = "Simulation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      {if (!is.null(species_colors)) scale_fill_manual(values = species_colors) else NULL}
  ) %>%
    ggplotly() %>%
    layout(barmode = "group", bargap = 0.01)

  # Total Yield per Sim - by Species
  singular_all <- bind_rows(lapply(seq_along(yieldList), function(i) {
    tot <- apply(yieldGearList[[i]], 2, sum)
    data.frame(sim = paste0("Sim ", i),
               Species = dimnames(yieldList[[i]])[[2]],
               Yield = tot)
  }))
  singular_plotly <- (
    ggplot(singular_all, aes(x = sim, y = Yield, fill = Species)) +
      geom_col() +
      labs(x = "Simulation", y = "Total Yield", fill = "Species") +
      theme_minimal() +
      theme(axis.text.x = element_text(hjust = 1)) +
      {if (!is.null(species_colors)) scale_fill_manual(values = species_colors) else NULL}
  ) %>%
    ggplotly()

  # 4) Pie charts - by Species
  pieList <- lapply(seq_along(yieldList), function(i) {
    df <- data.frame(
      Category = dimnames(yieldList[[i]])[[2]],
      Value    = apply(yieldGearList[[i]], 2, sum)
    )
    headRoom <- 0.12
    dom <- if (nSim == 1) {
      list(x = c(0.70, 1.00), y = c(0,          1.00 - headRoom))
    } else if (i == 1) {
      list(x = c(0.70, 1.00), y = c(0.55,       1.00 - headRoom/2))
    } else {
      list(x = c(0.70, 1.00), y = c(0.00,       0.45 - headRoom/2))
    }
    
    # Apply consistent colors to pie chart
    if (!is.null(species_colors)) {
      pie_colors <- species_colors[df$Category]
      # Handle any missing colors
      missing_pie_colors <- is.na(pie_colors)
      if (any(missing_pie_colors)) {
        extra_pie_colors <- grDevices::rainbow(sum(missing_pie_colors))
        names(extra_pie_colors) <- df$Category[missing_pie_colors]
        pie_colors[missing_pie_colors] <- extra_pie_colors
      }
    } else {
      pie_colors <- NULL
    }
    
    plot_ly(
      df,
      labels = ~Category,
      values = ~Value,
      type   = "pie",
      domain = dom,
      marker = list(colors = pie_colors)
    ) %>%
      layout(legend = list(title = list(text = "Species")))
  })

  # 5) Arrange subplots
  left_col <- subplot(
    subplot(spline_plotly, gearline_plotly,
            nrows = 1, shareY = FALSE, titleY = TRUE, margin = 0.05),
    subplot(compo_plotly, singular_plotly,
            nrows = 1, margin = 0.05),
    nrows = 2, margin = 0.05
  )
  right_col <- if (nSim == 1) {
    pieList[[1]]
  } else {
    subplot(pieList[[1]], pieList[[2]],
            nrows   = 2,
            heights = c(0.45, 0.45),
            margin  = 0.05)
  }

  # 6) Annotations for the 4-panel titles
  annotations <- list(
    list(text = "Species Change",      x = 0.085,  y = 1.06, xref = "paper", yref = "paper", showarrow = FALSE),
    list(text = "Gear Change",         x = 0.512, y = 1.06, xref = "paper", yref = "paper", showarrow = FALSE),
    list(text = "Yield Composition",   x = 0.075,  y = 0.48, xref = "paper", yref = "paper", showarrow = FALSE),
    list(text = "Total Yield per Sim", x = 0.512, y = 0.48, xref = "paper", yref = "paper", showarrow = FALSE)
  )
  fig <- subplot(left_col, right_col,
                 nrows  = 1,
                 widths = c(0.7, 0.3),
                 margin = 0.05) %>%
    layout(annotations = annotations)

  # 7) Final per-pie annotations
  domains <- purrr::keep(fig$x$data, ~ .$type == "pie") %>%
    purrr::map(~ .$domain)

  labs <- purrr::imap(domains, function(dom, idx) {
    if (nSim == 1) {
      x_off <- 0.05
      y_off <- 0.06
    } else {
      if (idx == 1) {
        x_off <- 0.05
        y_off <- 0.08
      } else {
        x_off <- 0.05
        y_off <- 0.04
      }
    }
    list(
      text      = paste0("Sim ", idx, " composition"),
      x         = mean(dom$x) + x_off,
      y         = dom$y[2]       + y_off,
      xref      = "paper",
      yref      = "paper",
      showarrow = FALSE,
      font      = list(size = 14)
    )
  })

  fig <- fig %>% layout(annotations = labs)

  fig
}




