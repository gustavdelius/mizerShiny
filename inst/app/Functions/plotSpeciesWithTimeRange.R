# Source the shared utility function
source("Functions/species_plot_utils.R")

plotSpeciesWithTimeRange <- function(harvestedprojection, unharvestedprojection, chosenyear,
                                     mode = c("triple", "chosen")) {

  mode <- match.arg(mode)
  percentage_diff <- process_sim_shared(harvestedprojection, unharvestedprojection, chosenyear, mode)
  percentage_diff$Percentage <- percentage_diff$percentage_diff
  percentage_diff$Class <- percentage_diff$fill_group

  ggplot(percentage_diff, aes(x = Species, y = Percentage, fill = Class)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 0, color = "grey", linetype = "dashed", linewidth = 0.5)+
    labs(x = "Species", y = "% Change") +
    scale_fill_manual(values = c(
      "Quarter, Negative" = "#F2A488",
      "Quarter, Positive" = "#2FA4E799",
      "Half, Negative" = "#E98C6B",
      "Half, Positive" = "#2FA4E7cc",
      "Full, Negative" = "#E76F51",
      "Full, Positive" = "#2FA4E7"
    )) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
      axis.text.y = element_text(size = 14),
      legend.position = "none",
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    )
}
