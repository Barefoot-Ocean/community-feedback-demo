box::use(
  tidyr[pivot_wider],
  dplyr[...],
  ggplot2[...],
  ggimage[...],
  forcats[fct_rev],
  lubridate[...],
  utils[...],
  cowplot[...]
)

box::use(
  app/logic/get_top_5_function[plot_top_5_species]
)

box::use(
  app/logic/utils
)

### Species Lelve Data ---------------------
#' @export
GET_TOP_FIVE_SPECIES_BY_LENGTH_NAME_CHART_DF <- function(data) {
  res_ <- data |>
    select(mean_length_cm, scientific_species, local_name) |>
    group_by(scientific_species) |>
    # summarise(
    #   median_length = mean(mean_length_cm, na.rm = TRUE)
    # ) |>
    mutate(
      median_length = mean(mean_length_cm, na.rm = TRUE)
    ) |>
    ungroup() |>
    filter(!is.na(median_length)) |>
    group_by(scientific_species) |>
    slice(1) |>
    ungroup() |>
    arrange(desc(median_length)) |>
    head(5) |>
    collect() |>
    select(scientific_species, local_name)

  return(res_)
}

#' @export
GET_TOP_FIVE_SPECIES_BY_LENGTH_NAME <- function(data) {
  res_ <- data |>
    select(mean_length_cm, scientific_species) |>
    group_by(scientific_species) |>
    summarise(
      median_length = mean(mean_length_cm, na.rm = TRUE)
    ) |>
    ungroup() |>
    filter(!is.na(median_length)) |>
    arrange(desc(median_length)) |>
    head(5) |>
    collect() |>
    pull(scientific_species)

  return(res_)
}

#' @export
TRANSFORM_DATA <- function(data) {

  SPECIES_LIST <- GET_TOP_FIVE_SPECIES_BY_LENGTH_NAME(data)

  date_range_tbl <- data |>
    summarise(
      min_date = min(date_landed, na.rm = TRUE),
      max_date = max(date_landed, na.rm = TRUE)
    ) |>
    collect()


  time_unit <- utils$get_time_unit(
    start_date = date_range_tbl$min_date,
    end_date = date_range_tbl$max_date
  )
  if (time_unit == "year") {
    time_unit <- "month"
  }

  res_ <- data |>
    select(date_landed, mean_length_cm, scientific_species) |>
    filter(scientific_species %in% SPECIES_LIST) |>
    arrange(date_landed) |>
    mutate(
      # date_from = (
      #   paste0(
      #     # day(date_landed), ' ',
      #     month(date_landed, label = TRUE, abbr = TRUE), ' ',
      #     year(date_landed)
      #   )
      # ),
      date_from = case_when(
        time_unit == "year" ~ paste0(year(date_landed)),
        time_unit == "month" ~ paste0(month(date_landed, label = TRUE, abbr = TRUE), ' ', year(date_landed)),
        time_unit == "day" ~ paste0(date_landed),
        TRUE ~ paste0(date_landed)
      ),
      mean_length_cm = as.numeric(mean_length_cm)
    ) |>
    select(date_from, mean_length_cm, scientific_species) |>
    # mutate(date_from = factor(date_from, levels = unique(date_from))) |>
    group_by(date_from, scientific_species) |>
    summarise(
      median_length = mean(mean_length_cm, na.rm = TRUE)
    ) |>
    ungroup() |>
    collect() |>
    mutate(date_from = factor(date_from, levels = unique(date_from))) |>
    arrange(desc(median_length))

  return(res_)

}

#' @export
TOP_FIVE_LEN_TIMELINE_PLOT <- function(data) {

  TRANSFORM_DATA(data) |>
    ggplot(aes(x = date_from, y = scientific_species, color = scientific_species, fill = scientific_species)) +
      geom_line(aes(y = median_length, group = scientific_species), size = 2) +
      geom_point(aes(y = median_length), size = 3) +
      scale_color_manual(values = c("#00cacf", "#806ebe", "#73b66c", "#ff9671", "#f9f871")) +
      theme_minimal() +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45),
        axis.text = element_text(size = 20)
      )

}

#' @export
TOP_FIVE_LEN_IMAGE_PLOT <- function(data, AVAILABLE_ART_FISH_IMG_TBL) {

  # SPECIES_LIST <- GET_TOP_FIVE_SPECIES_BY_LENGTH_NAME(data)
  SPECIES_LIST <- GET_TOP_FIVE_SPECIES_BY_LENGTH_NAME_CHART_DF(data)

  SPECIES_LIST |>
    # as_tibble() |>
    # rename(scientific_species = value) |>
    left_join(
      AVAILABLE_ART_FISH_IMG_TBL |> select(-local_name),
      by = c(
        'scientific_species' = 'scientific_species'
      )
    ) |>
    mutate(
      img_path = if_else(is.na(img_path), 'app/static/img/fish_images/default_fish_img.png', img_path),
      img_lineart_path = if_else(is.na(img_lineart_path), 'app/static/img/fish_images/default_fish_lineart_img.png', img_lineart_path)
    ) |>
    distinct() |>
    group_by(scientific_species) |>
    slice(1) |>
    ungroup() |>
    mutate(order_num = as.character(row_number())) |>
    ggplot(
      aes(x = "1", y = forcats::fct_rev(order_num), group = order_num)
    ) +
    geom_image(
      aes(
        image = img_path,
        color = factor(order_num)
      ),
      alpha = 0.75,
      size = 0.25,
      # color = c("#204e78", "#5693bf", "#ff7638", "#d53208", "#fff697")
    ) +
    geom_image(
      aes(
        image = img_lineart_path
      ),
      size = 0.25,
      alpha = 0.75,
      color = '#000000'
    ) +
    geom_text(
      aes(fontface = 2, label = local_name),
      vjust = -3.75,
      size = 5,
      color = '#000000'
    ) +
    scale_color_manual(values = c("1" = "#00cacf", "2" = "#806ebe", "3" = "#73b66c", "4" = "#ff9671", "5" = "#f9f871")) +
    theme_minimal() +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank() #remove minor gridlines,
    )

}

#' @export
DRAW_TOP_LEN_PLOT <- function(data, AVAILABLE_ART_FISH_IMG_TBL) {
  res_plot_ <- plot_grid(
    NULL,
    NULL,
    TOP_FIVE_LEN_TIMELINE_PLOT(data),
    TOP_FIVE_LEN_IMAGE_PLOT(data, AVAILABLE_ART_FISH_IMG_TBL),
    ncol = 2,
    align = "hv",
    rel_widths = c(3, 3),
    rel_heights = c(0, 6)
  )

  return(res_plot_)
}

#' @export
AVERAGE_LENGTH_DATA_TOP_FIVE_TBL <- function(data) {
  res_ <- data |>
    distinct() |>
    select(-id) |>
    head(6) |>
    mutate(scientific_species = factor(scientific_species, levels = unique(scientific_species)))

  return(res_)

}

AVERAGE_LENGTH_DATA_TOP_FIVE_DATE_TBL <- function(data) {

  res_ <- data |>
    head(5) |>
    mutate(
      rank = 11 - row_number()
    ) |>
    select(date_from, rank) |>
    union(
      data$date_from |>
        unique() |>
        as_tibble() |>
        rename(date_from = value) |>
        filter(!date_from %in% data$date_from) |>
        mutate(rank = 2)
    )

  return(res_)
}


#' @export
xplot <- function(data) {
  chart <- ggplot(
    data,
    aes(x = date_from, y = rank, fill = rank)
  ) +
    geom_col() +
    theme_minimal() +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme(
      # axis.text.x = element_blank(),
      # axis.ticks.x = element_blank(),
      axis.text.x = element_text(size = 20),
      axis.text.x = element_text(angle = 45),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )

  return(chart)
}

#' @export
yplot <- function(data) {
  chart <- ggplot(
    data,
    aes(y = forcats::fct_rev(date_from), x = rank, fill = rank)
  ) +
    geom_col() +
    theme_minimal() +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme(
      # axis.text.x = element_blank(),
      # axis.ticks.x = element_blank(),
      # axis.text.x = element_text(angle = 45),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none"
    )

  return(chart)
}

#' @export
draw_average_length_data_top_five_gridplot <- function(data, AVAILABLE_ART_FISH_IMG_TBL) {

  # AVERAGE_LENGTH_DATA_TOP_FIVE_FISH_CHART <- draw_average_length_data_top_five_plot(data)
  AVERAGE_LENGTH_DATA_TOP_FIVE_FISH_CHART <- plot_top_5_species(
    data = data,
    column = 'median_kg_trip',
    pal = c("#2c3e50", "#495d72", "#667b8d", "#8399a7", "#a0b6c5", '#9ca8b1'),
    text_color = "#ffffff",
    AVAILABLE_ART_FISH_IMG_TBL = AVAILABLE_ART_FISH_IMG_TBL,
    number_label = TRUE
  )

  xplot_ <- xplot(
    AVERAGE_LENGTH_DATA_TOP_FIVE_DATE_TBL(
      AVERAGE_LENGTH_DATA_TOP_FIVE_TBL(
        TRANSFORM_DATA(data)
      )
    )
  )
  yplot_ <- yplot(
    AVERAGE_LENGTH_DATA_TOP_FIVE_DATE_TBL(
      AVERAGE_LENGTH_DATA_TOP_FIVE_TBL(
        TRANSFORM_DATA(data)
      )
    )
  )

  # Arranging the plot using cowplot
  res_plot_ <- plot_grid(
    xplot_,
    NULL,
    AVERAGE_LENGTH_DATA_TOP_FIVE_FISH_CHART,
    yplot_,
    ncol = 2,
    align = "hv",
    rel_widths = c(3, 2),
    rel_heights = c(2, 3)
  )

  return(res_plot_)
}




### Family Level Data ---------------------
#' @export
GET_TOP_FIVE_SPECIES_BY_LENGTH_NAME__FAMILY_LEVEL <- function(data) {
  res_ <- data |>
    select(mean_length_cm, scientific_family) |>
    group_by(scientific_family) |>
    summarise(
      median_length = mean(mean_length_cm, na.rm = TRUE)
    ) |>
    ungroup() |>
    filter(!is.na(median_length)) |>
    arrange(desc(median_length)) |>
    head(5) |>
    collect() |>
    pull(scientific_family)

  return(res_)
}

#' @export
TRANSFORM_DATA__FAMILY_LEVEL <- function(data) {

  SPECIES_LIST <- GET_TOP_FIVE_SPECIES_BY_LENGTH_NAME__FAMILY_LEVEL(data)

  date_range_tbl <- data |>
    summarise(
      min_date = min(date_landed, na.rm = TRUE),
      max_date = max(date_landed, na.rm = TRUE)
    ) |>
    collect()


  time_unit <- utils$get_time_unit(
    start_date = date_range_tbl$min_date,
    end_date = date_range_tbl$max_date
  )
  # !dev temp solution to show more dots on the chart
  if (time_unit == "year") {
    time_unit <- "month"
  }

  res_ <- data |>
    select(date_landed, mean_length_cm, scientific_family) |>
    filter(scientific_family %in% SPECIES_LIST) |>
    arrange(date_landed) |>
    mutate(
      date_from = case_when(
        time_unit == "year" ~ paste0(year(date_landed)),
        time_unit == "month" ~ paste0(month(date_landed, label = TRUE, abbr = TRUE), ' ', year(date_landed)),
        time_unit == "day" ~ paste0(date_landed),
        TRUE ~ paste0(date_landed)
      ),
      mean_length_cm = as.numeric(mean_length_cm)
    ) |>
    select(date_from, mean_length_cm, scientific_family) |>
    group_by(date_from, scientific_family) |>
    summarise(
      median_length = mean(mean_length_cm, na.rm = TRUE)
    ) |>
    ungroup() |>
    collect() |>
    mutate(date_from = factor(date_from, levels = unique(date_from))) |>
    arrange(desc(median_length))

  return(res_)

}

#' @export
TOP_FIVE_LEN_TIMELINE_PLOT__FAMILY_LEVEL <- function(data) {

  TRANSFORM_DATA__FAMILY_LEVEL(data) |>
    ggplot(aes(x = date_from, y = scientific_family, color = scientific_family, fill = scientific_family)) +
    geom_line(aes(y = median_length, group = scientific_family), size = 2) +
    geom_point(aes(y = median_length), size = 3) +
    scale_color_manual(values = c("#00cacf", "#806ebe", "#73b66c", "#ff9671", "#f9f871")) +
    theme_minimal() +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45),
      axis.text = element_text(size = 20)
    )

}

#' @export
TOP_FIVE_LEN_IMAGE_PLOT__FAMILY_LEVEL <- function(data, AVAILABLE_ART_FISH_IMG_TBL) {

  SPECIES_LIST <- GET_TOP_FIVE_SPECIES_BY_LENGTH_NAME__FAMILY_LEVEL(data)

  SPECIES_LIST |>
    as_tibble() |>
    rename(scientific_family = value) |>
    left_join(
      AVAILABLE_ART_FISH_IMG_TBL,
      by = c(
        'scientific_family' = 'scientific_family'
      )
    ) |>
    mutate(
      img_path = if_else(is.na(img_path), 'app/static/img/fish_images/default_fish_img.png', img_path),
      img_lineart_path = if_else(is.na(img_lineart_path), 'app/static/img/fish_images/default_fish_lineart_img.png', img_lineart_path)
    ) |>
    distinct() |>
    group_by(scientific_family) |>
    slice(1) |>
    ungroup() |>
    mutate(order_num = as.character(row_number())) |>
    ggplot(
      aes(x = "1", y = forcats::fct_rev(order_num), group = order_num)
    ) +
    geom_image(
      aes(
        image = img_path,
        color = factor(order_num)
      ),
      size = 0.25,
      alpha = 0.75
      # color = c("#204e78", "#5693bf", "#ff7638", "#d53208", "#fff697")
    ) +
    geom_image(
      aes(
        image = img_lineart_path
      ),
      size = 0.25,
      alpha = 0.75,
      color = '#000000'
    ) +
    geom_text(
      aes(fontface = 2, label = scientific_family),
      vjust = -3.75,
      size = 5,
      color = '#000000'
    ) +
    scale_color_manual(values = c("1" = "#00cacf", "2" = "#806ebe", "3" = "#73b66c", "4" = "#ff9671", "5" = "#f9f871")) +
    theme_minimal() +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank() #remove minor gridlines,
    )

}

#' @export
DRAW_TOP_LEN_PLOT__FAMILY_LEVEL <- function(data, AVAILABLE_ART_FISH_IMG_TBL) {
  res_plot_ <- plot_grid(
    NULL,
    NULL,
    TOP_FIVE_LEN_TIMELINE_PLOT__FAMILY_LEVEL(data),
    TOP_FIVE_LEN_IMAGE_PLOT__FAMILY_LEVEL(data, AVAILABLE_ART_FISH_IMG_TBL),
    ncol = 2,
    align = "hv",
    rel_widths = c(4, 2),
    rel_heights = c(0, 6)
  )

  return(res_plot_)
}

#' @export
AVERAGE_LENGTH_DATA_TOP_FIVE_TBL__FAMILY_LEVEL <- function(data) {
  res_ <- data |>
    distinct() |>
    select(-id) |>
    head(6) |>
    mutate(scientific_family = factor(scientific_family, levels = unique(scientific_family)))

  return(res_)

}

AVERAGE_LENGTH_DATA_TOP_FIVE_DATE_TBL__FAMILY_LEVEL <- function(data) {

  res_ <- data |>
    head(5) |>
    mutate(
      rank = 11 - row_number()
    ) |>
    select(date_from, rank) |>
    union(
      data$date_from |>
        unique() |>
        as_tibble() |>
        rename(date_from = value) |>
        filter(!date_from %in% data$date_from) |>
        mutate(rank = 2)
    )

  return(res_)
}


#' @export
xplot__FAMILY_LEVEL <- function(data) {
  chart <- ggplot(
    data,
    aes(x = date_from, y = rank, fill = rank)
  ) +
    geom_col() +
    theme_minimal() +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme(
      # axis.text.x = element_blank(),
      # axis.ticks.x = element_blank(),
      axis.text.x = element_text(size = 20),
      axis.text.x = element_text(angle = 45),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )

  return(chart)
}

#' @export
yplot__FAMILY_LEVEL <- function(data) {
  chart <- ggplot(
    data,
    aes(y = forcats::fct_rev(date_from), x = rank, fill = rank)
  ) +
    geom_col() +
    theme_minimal() +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme(
      # axis.text.x = element_blank(),
      # axis.ticks.x = element_blank(),
      # axis.text.x = element_text(angle = 45),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none"
    )

  return(chart)
}

#' @export
draw_average_length_data_top_five_gridplot__FAMILY_LEVEL <- function(data, AVAILABLE_ART_FISH_IMG_TBL) {

  # AVERAGE_LENGTH_DATA_TOP_FIVE_FISH_CHART <- draw_average_length_data_top_five_plot(data)
  AVERAGE_LENGTH_DATA_TOP_FIVE_FISH_CHART <- plot_top_5_species(
    data = data,
    column = 'median_kg_trip',
    pal = c("#2c3e50", "#495d72", "#667b8d", "#8399a7", "#a0b6c5", '#9ca8b1'),
    text_color = "#ffffff",
    AVAILABLE_ART_FISH_IMG_TBL = AVAILABLE_ART_FISH_IMG_TBL,
    number_label = TRUE
  )

  xplot_ <- xplot(
    AVERAGE_LENGTH_DATA_TOP_FIVE_DATE_TBL(
      AVERAGE_LENGTH_DATA_TOP_FIVE_TBL(
        TRANSFORM_DATA(data)
      )
    )
  )
  yplot_ <- yplot(
    AVERAGE_LENGTH_DATA_TOP_FIVE_DATE_TBL(
      AVERAGE_LENGTH_DATA_TOP_FIVE_TBL(
        TRANSFORM_DATA(data)
      )
    )
  )

  # Arranging the plot using cowplot
  res_plot_ <- plot_grid(
    xplot_,
    NULL,
    AVERAGE_LENGTH_DATA_TOP_FIVE_FISH_CHART,
    yplot_,
    ncol = 2,
    align = "hv",
    rel_widths = c(3, 2),
    rel_heights = c(2, 3)
  )

  return(res_plot_)
}
