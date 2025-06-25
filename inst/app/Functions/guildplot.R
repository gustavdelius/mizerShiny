guildplot <- function(harvestedprojection, unharvestedprojection,
                      chosenyear,
                      guildparams, celticsim,
                      mode = c("chosen", "triple")) {

  mode <- match.arg(mode)

  # Calculate three specific time points
  quarter_year <- max(1, ceiling(chosenyear * 0.25))
  half_year <- max(1, ceiling(chosenyear * 0.5))
  full_year <- chosenyear

  # Get spectra data at the full year (chosen time point)
  harvested_full    <- plotSpectra(harvestedprojection,
                              time_range = full_year,
                              return_data = TRUE)
  unharvested_full  <- plotSpectra(unharvestedprojection,
                              time_range = full_year,
                              return_data = TRUE)

  if (mode == "triple") {
    # Get spectra data at quarter and half years
    harvested_quarter   <- plotSpectra(harvestedprojection,
                                    time_range = quarter_year,
                                    return_data = TRUE)
    harvested_half      <- plotSpectra(harvestedprojection,
                                    time_range = half_year,
                                    return_data = TRUE)
    unharvested_quarter <- plotSpectra(unharvestedprojection,
                                    time_range = quarter_year,
                                    return_data = TRUE)
    unharvested_half    <- plotSpectra(unharvestedprojection,
                                    time_range = half_year,
                                    return_data = TRUE)
  }

  process_guilds <- function(mizerprojection) {

    assign_guild <- function(dat, rules) {
      dat <- dat |> dplyr::mutate(Guild = NA_character_)
      for (i in seq_len(nrow(rules))) {
        dat <- dat |>
          dplyr::mutate(
            Guild = dplyr::case_when(
              w < 0.05                                      ~ "Plank",
              is.na(Guild) &
                w >= rules$minw[i] & w < rules$maxw[i]      ~ rules$Feeding.guild[i],
              TRUE                                          ~ Guild
            )
          )
      }
      dat
    }

    mizerprojection |>
      dplyr::group_by(Species) |>
      dplyr::group_modify(\(.x, .y){
        rules <- guildparams |>
          dplyr::filter(Species == unique(.x$Legend))
        if (nrow(rules) == 0) .x else assign_guild(.x, rules)
      }) |>
      dplyr::ungroup() |>
      tidyr::drop_na(Guild) |>
      dplyr::group_by(Guild) |>
      dplyr::summarise(value = mean(value), .groups = "drop")
  }

  guilds_full    <- process_guilds(harvested_full)    |> dplyr::mutate(time = "full")
  unguilds_full  <- process_guilds(unharvested_full)  |> dplyr::mutate(time = "full")

  if (mode == "triple") {
    guilds_quarter <- process_guilds(harvested_quarter)   |> dplyr::mutate(time = "quarter")
    guilds_half    <- process_guilds(harvested_half)      |> dplyr::mutate(time = "half")
    unguilds_quarter <- process_guilds(unharvested_quarter) |> dplyr::mutate(time = "quarter")
    unguilds_half    <- process_guilds(unharvested_half)    |> dplyr::mutate(time = "half")

    harv_all   <- dplyr::bind_rows(guilds_full, guilds_quarter, guilds_half)
    unharv_all <- dplyr::bind_rows(unguilds_full, unguilds_quarter, unguilds_half)
  } else {                       # mode == "chosen"
    harv_all   <- guilds_full
    unharv_all <- unguilds_full
  }

  joinedguilds <- harv_all |>
    dplyr::full_join(unharv_all, by = c("Guild","time")) |>
    dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
    dplyr::select(Guild, time, percentage_diff)

  joinedguilds$time <- factor(
    joinedguilds$time,
    levels = if (mode == "chosen") "full" else c("quarter","half","full")
  )
  joinedguilds$fill_group <- interaction(joinedguilds$percentage_diff >= 0,
                                         joinedguilds$time)

  joinedguilds$Class <- factor(
    joinedguilds$fill_group,
    levels = c("FALSE.quarter", "TRUE.quarter", "FALSE.half", "TRUE.half", "FALSE.full", "TRUE.full"),
    labels = c("Quarter, Negative", "Quarter, Positive", "Half, Negative", "Half, Positive", "Full, Negative", "Full, Positive")
  )

  joinedguilds$Percentage <- joinedguilds$percentage_diff

  ## ---- plot -----------------------------------------------------------------
  ggplot(joinedguilds, aes(Guild, Percentage, fill = Class)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 0, colour = "grey", linetype = "dashed", linewidth = 0.5) +
    scale_fill_manual(
      values = c(
        "Quarter, Negative"  = "#F2A488",
        "Quarter, Positive"  = "#2FA4E799",
        "Half, Negative" = "#E98C6B",
        "Half, Positive" = "#2FA4E7cc",
        "Full, Negative"   = "#E76F51",
        "Full, Positive"   = "#2FA4E7"
      ),
      drop = FALSE
    ) +
    labs(x = "Guild", y = "Percentage Change") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5),
      axis.text.y = element_text(size = 14),
      legend.position = "none",
      axis.title = element_text(size = 16)
    )

}

##HOW TO LOAD PUT IN THE GUILDS STUFF
#This loads and formats in the data for the guilds
#
# guildinfo <- read.table("Guilds information/guild_cleaned.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
# fish_names <- read.table("Guilds information/fishinfo.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
#
# guildparams <- celticsim@species_params%>%
#   select(species, a, b)%>%
#   rename(Common_Name=species)%>%
#   #this code has correctly formatted the species params
#   inner_join(fish_names, by=c("Common_Name"))%>%
#   #we have now joined species params to a table containing the scientific names
#   rename(Species=Scientific_Name)%>%
#   inner_join(
#     guildinfo%>%
#       filter(Species %in% fish_names$Scientific_Name),
#     by="Species")%>%
#   #we have now joined the rows with the same scientific names - so
#   #we have joined the a and b values to the given species
#   #this is converting from length to weight
#   mutate(maxw=a*Max.cm^b,
#          minw=a*Min.cm^b)%>%
#   select(Common_Name, maxw, minw, Feeding.guild)%>%
#   rename(Species=Common_Name)

#So what the code does above is take the table of the guild information,
#from the pilot assessment by Murray Thompson, then you give it a table
#containing the species common + scientific names, and this is all then
#joined together so you have the a and b values next to given species,
#so therefore we are able to convert from the length measurements to weight,
#which can then be used in mizer to filter into the correct guilds.


