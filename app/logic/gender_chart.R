box::use(
  tidyr[pivot_wider],
  dplyr[...],
  ggplot2[...],
  ggimage[...],
  forcats[fct_rev]
)

##### icons path
female_icon_path <- 'app/static/img/female.png'
male_icon_path <- 'app/static/img/male.png'


#' @export
TRANSFORM_DATA <- function(data) {

  res_ <- data |>
    dplyr::select(fisher_id, fisher_gender) |>
    dplyr::group_by(fisher_gender) |>
    dplyr::summarise(
      total_num = n_distinct(fisher_id)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      total_num = total_num / sum(total_num, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      image = dplyr::case_when(
        fisher_gender == 'female' ~ female_icon_path,
        fisher_gender == 'male' ~ male_icon_path,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(
      fisher_gender %in% c('female', 'male')
    )

  return(res_)

}

#' @export
TRANSFORM_DATA_BUYER <- function(data) {

  res_ <- data |>
    dplyr::select(buyer_id, buyer_gender) |>
    dplyr::group_by(buyer_gender) |>
    dplyr::summarise(
      total_num = n_distinct(buyer_id)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      total_num = total_num / sum(total_num, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      image = dplyr::case_when(
        buyer_gender == 'female' ~ female_icon_path,
        buyer_gender == 'male' ~ male_icon_path,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(
      buyer_gender %in% c('female', 'male')
    )

  return(res_)

}

#' @export
draw_total_fishers_number_plot <- function(data) {

  chart <- ggplot(
    data,
    aes(x = forcats::fct_rev(fisher_gender), y = 1, group = fisher_gender)
  ) +
    geom_image(
      aes(
        image = image,
        color = factor(fisher_gender),
        size = factor(fisher_gender)
      )
    ) +
    geom_text(
      aes(
        fontface = 2,
        label = scales::percent(total_num, accuracy = NULL),
        y = -1,
        color = factor(fisher_gender)
      ),
      size = 14
    ) +
    scale_color_manual(values = c("male" = "#07b6d5", "female" = "#fd7da6")) +
    scale_size_manual(values = c("male" = 0.4, "female" = 0.65)) +
    theme_minimal() +
    labs(
      x = NULL,
      y = NULL
    ) +
    ylim(-1, 3) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      # Remove all lines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
    )


  return(chart)
}


#' @export
draw_total_buyers_number_plot <- function(data) {

  chart <- ggplot(
    data,
    aes(x = forcats::fct_rev(buyer_gender), y = 1, group = buyer_gender)
  ) +
    geom_image(
      aes(
        image = image,
        color = factor(buyer_gender),
        size = factor(buyer_gender)
      )
    ) +
    geom_text(
      aes(
        fontface = 2,
        label = scales::percent(total_num, accuracy = NULL),
        y = -1,
        color = factor(buyer_gender)
      ),
      size = 14
    ) +
    scale_color_manual(values = c("male" = "#07b6d5", "female" = "#fd7da6")) +
    scale_size_manual(values = c("male" = 0.4, "female" = 0.65)) +
    theme_minimal() +
    labs(
      x = NULL,
      y = NULL
    ) +
    ylim(-1, 3) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      # Remove all lines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
    )

  return(chart)
}

