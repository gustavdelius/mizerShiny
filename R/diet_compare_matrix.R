comparedietmatrix <- function(unharvestedprojection, harvestedprojection, timerange){
  dietunharv <- getDiet(unharvestedprojection@params,
                        n = apply(unharvestedprojection@n[timerange,,], c(2, 3), mean),
                        n_pp = apply(unharvestedprojection@n_pp[timerange,], 2, mean),
                        n_other = apply(unharvestedprojection@n_other[timerange,], 2, mean),
                        proportion = TRUE) |>
    as.table()|>
    as.data.frame()|>
    dplyr::group_by(predator, prey)|>
    dplyr::summarise(Proportion=mean(Freq), .groups = "drop")

  dietharv <- getDiet(unharvestedprojection@params,
                      n = apply(unharvestedprojection@n[timerange,,], c(2, 3), mean),
                      n_pp = apply(unharvestedprojection@n_pp[timerange,], 2, mean),
                      n_other = apply(unharvestedprojection@n_other[timerange,], 2, mean),
                      proportion = TRUE) |>
    as.table()|>
    as.data.frame()|>
    dplyr::group_by(predator, prey)|>
    dplyr::summarise(Proportion=mean(Freq), .groups = "drop")

  joindiet <- dplyr::left_join(dietharv, dietunharv, by = c("prey", "predator"))|>
    dplyr::mutate(Difference = ((Proportion.x - Proportion.y) / Proportion.y) * 100) |>
    dplyr::select(predator, prey, Difference)|>
    dplyr::filter(!predator %in% ("Resource"), !prey %in% ("Resource"))

  ggplot2::ggplot(joindiet, ggplot2::aes(x = predator, y = prey, fill = Difference)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2() +
    ggplot2::labs(x = "Predator", y = "Prey", fill = "Difference") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,size = 14),
                   axis.text.y = ggplot2::element_text(size = 14),
                   axis.title.x = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 16))
}


