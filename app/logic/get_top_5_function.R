box::use(
  dplyr[...],
  ggplot2[...],
  ggimage[...],
  tidyr[...],
  lubridate[...],
  stringr[...],
  utils[...],
  forcats[fct_rev],
  utils[...],
  cowplot[...]
)


#' @export
find_top_5_species <- function(data, column, AVAILABLE_ART_FISH_IMG_TBL, mean_measure = FALSE) {

  # get total number
  total_weight_and_purchase_price <- data |>
    mutate(
      total_weight_landed_kg = na_if(total_weight_landed_kg, "NA"),
      total_purchase_price = na_if(total_purchase_price, "NA")
    ) |>
    mutate(
      total_weight_landed_kg = as.numeric(total_weight_landed_kg),
      total_purchase_price = as.numeric(total_purchase_price)
    ) |>
    summarise(
      total_weight_landed_kg = sum(total_weight_landed_kg, na.rm = TRUE),
      total_purchase_price = sum(total_purchase_price, na.rm = TRUE)
    ) |>
    collect()

  # get top five
  data_ <- data |>
    mutate(
      date_landed = floor_date(date_landed, "month")
    ) |>
    filter(!is.na(scientific_family) & !is.na(scientific_species) & !is.na(local_name)) |>  # remove NA
    filter(scientific_family != "" & scientific_species  != "" & local_name != "") |>  # remove empty
    mutate(scientific_species = as.character(
      paste0(scientific_family, "___", scientific_species, "___", local_name)
    )
    # stringr::str_glue('{scientific_family}___{scientific_species}___{local_name}'))
    ) %>%
    mutate(
      total_weight_landed_kg = na_if(total_weight_landed_kg, "NA"),
      total_purchase_price = na_if(total_purchase_price, "NA")
    ) |>
    mutate(
      total_weight_landed_kg = as.numeric(total_weight_landed_kg),
      total_purchase_price = as.numeric(total_purchase_price)
    ) |>
    group_by(scientific_species) |>
    summarise(
      total_kg_landed = sum(total_weight_landed_kg, na.rm = TRUE),
      total_local_price = sum(total_purchase_price, na.rm = TRUE),
      median_kg_trip = mean(total_weight_landed_kg, na.rm = TRUE),
      percent_kg_landed = sum(total_weight_landed_kg, na.rm = TRUE) / total_weight_and_purchase_price$total_weight_landed_kg,
      percent_local_price = sum(total_purchase_price, na.rm = TRUE) / total_weight_and_purchase_price$total_purchase_price
    ) |>
    ungroup()

  res_ <- data_ |>
    select(scientific_species, !!sym(column)) |>
    # Remove NAs
    filter(!is.na(!!sym(column))) |>
    arrange(desc(!!sym(column))) |>
    mutate(id = if_else(row_number() <= 5, 1L, 2L)) |>
    group_by(id) |>
    arrange(id, -!!sym(column)) |>
    mutate(scientific_species = if_else(id == 2L, "Others", scientific_species))

  if(mean_measure) {
    res_ <- res_ |>
      mutate(
        {{column}} := if_else(scientific_species == "Others", mean(!!sym(column), na.rm = TRUE), !!sym(column))
      )
  } else {
    res_ <- res_ |>
      mutate(
        {{column}} := if_else(scientific_species == "Others", sum(!!sym(column), na.rm = TRUE), !!sym(column))
      )
  }

  res_ <- res_ |>
    # pull from database
    collect() |>
    ungroup() |>
    distinct() |>
    select(-id) |>
    tidyr::separate(
      scientific_species,
      c("scientific_family", "scientific_species", "local_name"),
      sep = "___",
      remove = TRUE,
      fill = 'left'
    ) |>
    # fill NA to scientific_family to 'Others'
    mutate(
      scientific_family  = na_if(scientific_family, "Unknown"),
      scientific_species = na_if(scientific_species, "Unknown"),
      local_name         = na_if(local_name, "Unknown")
    ) |>
    mutate(
      scientific_family  = if_else(is.na(scientific_family), "Others", scientific_family),
      scientific_species = if_else(is.na(scientific_species), "Others", scientific_species),
      local_name         = if_else(is.na(local_name), "Others", local_name)
    ) |>
    head(6) |>
    mutate(order_num = (row_number() * 3)) |>
    mutate(order_num = factor(order_num, levels = order_num))

  # Join image path
  res_ <- res_ |>
    left_join(
      AVAILABLE_ART_FISH_IMG_TBL,
      by = c(
        'scientific_family' = 'scientific_family',
        'scientific_species' = 'scientific_species',
        'local_name' = 'local_name'
      )
    ) |>
    distinct() |>
    group_by(scientific_family, scientific_species) |>
    slice(1) |>
    ungroup() |>
    arrange(desc(desc(order_num))) |>
    select(-scientific_family) |>
    mutate(
      img_path = if_else(is.na(img_path), "app/static/img/fish_images/default_fish_img.png", img_path),
      img_lineart_path = if_else(is.na(img_lineart_path), "app/static/img/fish_images/default_fish_lineart_img.png", img_lineart_path)
    )


  return(res_)
}

#' @export
plot_top_5_species <- function(data, column, pal, text_color, AVAILABLE_ART_FISH_IMG_TBL, number_label = FALSE) {

  size_arr = c(0.55, 0.50, 0.45, 0.4, 0.35, 0.35)

  plot_data <- find_top_5_species(data, column, AVAILABLE_ART_FISH_IMG_TBL)

  plot_ <- ggplot(
    plot_data,
    aes(x = "1", y = forcats::fct_rev(order_num), group = order_num)
  ) +
    geom_image(
      aes(
        image = img_path,
        # image = 'app/static/img/fish_images/default_fish_img.png',
        color = factor(order_num),
        size = factor(order_num)
      ),
      alpha = 0.7
      # ,
      # size = c(0.55, 0.50, 0.45, 0.4, 0.35, 0.35),
      # color = pal
    ) +
    geom_image(
      aes(
        image = img_lineart_path,
        # image = 'app/static/img/fish_images/default_fish_lineart_img.png',
        size = factor(order_num)
      ),
      alpha = 0.7,
      # size = c(0.55, 0.50, 0.45, 0.4, 0.35, 0.35),
      color = '#000000'
    ) +
    {
      if(number_label) {
        geom_text(
          aes(fontface = 2, label = scales::number(!!sym(column), accuracy = 0.01)),
          vjust = -0.25,
          color = text_color
        )
      } else {
        geom_text(
          aes(fontface = 2, label = scales::percent(!!sym(column), accuracy = 0.01)),
          vjust = -0.25,
          color = text_color
        )
      }
    } +
    geom_text(
      aes(label = local_name),
      vjust = 1.5,
      size = 3.5,
      color = text_color
    ) +
    scale_y_discrete(breaks = factor(-2:20), limits = c(19:0) |> as.character()) +
    scale_color_manual(values = c("3" = pal[1], "6" = pal[2], "9" = pal[3], "12" = pal[4], "15" = pal[5], "18" = pal[6])) +
    scale_size_manual(values = c("3" = size_arr[1], "6" = size_arr[2], "9" = size_arr[3], "12" = size_arr[4], "15" = size_arr[5], "18" = size_arr[6])) +
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
      panel.grid.minor = element_blank() #remove minor gridlines
    )

  return(plot_)

}

#' @export
find_top_5_species_family_level <- function(data, column, AVAILABLE_ART_FISH_IMG_TBL, mean_measure = FALSE) {

  # get total number
  total_weight_and_purchase_price <- data |>
    mutate(
      total_weight_landed_kg = na_if(total_weight_landed_kg, "NA"),
      total_purchase_price = na_if(total_purchase_price, "NA")
    ) |>
    mutate(
      total_weight_landed_kg = as.numeric(total_weight_landed_kg),
      total_purchase_price = as.numeric(total_purchase_price)
    ) |>
    summarise(
      total_weight_landed_kg = sum(total_weight_landed_kg, na.rm = TRUE),
      total_purchase_price = sum(total_purchase_price, na.rm = TRUE)
    ) |>
    collect()

  # get top five
  data_ <- data |>
    mutate(
      date_landed = floor_date(date_landed, "month")
    ) |>
    filter(!is.na(scientific_family) & !is.na(scientific_species) & !is.na(local_name)) |>  # remove NA
    filter(scientific_family != "" & scientific_species  != "" & local_name != "") |>  # remove empty
    # mutate(scientific_species = as.character(
    #   paste0(scientific_family, "___", scientific_species, "___", local_name)
    # )
    # # stringr::str_glue('{scientific_family}___{scientific_species}___{local_name}'))
    # ) %>%
    mutate(
      total_weight_landed_kg = na_if(total_weight_landed_kg, "NA"),
      total_purchase_price = na_if(total_purchase_price, "NA")
    ) |>
    mutate(
      total_weight_landed_kg = as.numeric(total_weight_landed_kg),
      total_purchase_price = as.numeric(total_purchase_price)
    ) |>
    group_by(scientific_family) |>
    summarise(
      total_kg_landed = sum(total_weight_landed_kg, na.rm = TRUE),
      total_local_price = sum(total_purchase_price, na.rm = TRUE),
      median_kg_trip = mean(total_weight_landed_kg, na.rm = TRUE),
      percent_kg_landed = sum(total_weight_landed_kg, na.rm = TRUE) / total_weight_and_purchase_price$total_weight_landed_kg,
      percent_local_price = sum(total_purchase_price, na.rm = TRUE) / total_weight_and_purchase_price$total_purchase_price
    ) |>
    ungroup()

  res_ <- data_ |>
    select(scientific_family, !!sym(column)) |>
    # Remove NAs
    filter(!is.na(!!sym(column))) |>
    arrange(desc(!!sym(column))) |>
    mutate(id = if_else(row_number() <= 5, 1L, 2L)) |>
    group_by(id) |>
    arrange(id, -!!sym(column)) |>
    mutate(scientific_family = if_else(id == 2L, "Others", scientific_family))

  if(mean_measure) {
    res_ <- res_ |>
      mutate(
        {{column}} := if_else(scientific_family == "Others", mean(!!sym(column), na.rm = TRUE), !!sym(column))
      )
  } else {
    res_ <- res_ |>
      mutate(
        {{column}} := if_else(scientific_family == "Others", sum(!!sym(column), na.rm = TRUE), !!sym(column))
      )
  }

  res_ <- res_ |>
    # pull from database
    collect() |>
    ungroup() |>
    distinct() |>
    select(-id) |>
    # tidyr::separate(
    #   scientific_species,
    #   c("scientific_family", "scientific_species", "local_name"),
    #   sep = "___",
    #   remove = TRUE,
    #   fill = 'left'
    # ) |>
    # fill NA to scientific_family to 'Others'
    mutate(
      scientific_family  = na_if(scientific_family, "Unknown")
      # ,
      # scientific_species = na_if(scientific_species, "Unknown"),
      # local_name         = na_if(local_name, "Unknown")
    ) |>
    mutate(
      scientific_family  = if_else(is.na(scientific_family), "Others", scientific_family)
      # ,
      # scientific_species = if_else(is.na(scientific_species), "Others", scientific_species),
      # local_name         = if_else(is.na(local_name), "Others", local_name)
    ) |>
    head(6) |>
    mutate(order_num = (row_number() * 3)) |>
    mutate(order_num = factor(order_num, levels = order_num))

  # Join image path
  res_ <- res_ |>
    left_join(
      AVAILABLE_ART_FISH_IMG_TBL |> select(scientific_family, img_path, img_lineart_path),
      by = c(
        'scientific_family' = 'scientific_family'
        # ,
        # 'scientific_species' = 'scientific_species',
        # 'local_name' = 'local_name'
      )
    ) |>
    distinct() |>
    group_by(scientific_family) |>
    slice(1) |>
    ungroup() |>
    arrange(desc(desc(order_num))) |>
    # select(-scientific_family) |>
    mutate(
      img_path = if_else(is.na(img_path), "app/static/img/fish_images/default_fish_img.png", img_path),
      img_lineart_path = if_else(is.na(img_lineart_path), "app/static/img/fish_images/default_fish_lineart_img.png", img_lineart_path)
    )


  return(res_)
}


#' @export
plot_top_5_species_family_level <- function(data, column, pal, text_color, AVAILABLE_ART_FISH_IMG_TBL, number_label = FALSE) {

  size_arr = c(0.55, 0.50, 0.45, 0.4, 0.35, 0.35)

  plot_data <- find_top_5_species_family_level(data, column, AVAILABLE_ART_FISH_IMG_TBL)

  # browser()

  plot_ <- ggplot(
    plot_data,
    aes(x = "1", y = forcats::fct_rev(order_num), group = order_num)
  ) +
    geom_image(
      aes(
        image = img_path,
        # image = 'app/static/img/fish_images/default_fish_img.png',
        color = factor(order_num),
        size = factor(order_num)
      ),
      alpha = 0.7
      # ,
      # size = c(0.55, 0.50, 0.45, 0.4, 0.35, 0.35),
      # color = pal
    ) +
    geom_image(
      aes(
        image = img_lineart_path,
        # image = 'app/static/img/fish_images/default_fish_lineart_img.png',
        size = factor(order_num)
      ),
      alpha = 0.7,
      # size = c(0.55, 0.50, 0.45, 0.4, 0.35, 0.35),
      color = '#000000'
    ) +
    {
      if(number_label) {
        geom_text(
          aes(fontface = 2, label = scales::number(!!sym(column), accuracy = 0.01)),
          vjust = -0.25,
          color = text_color
        )
      } else {
        geom_text(
          aes(fontface = 2, label = scales::percent(!!sym(column), accuracy = 0.01)),
          vjust = -0.25,
          color = text_color
        )
      }
    } +
    geom_text(
      aes(label = scientific_family),
      vjust = 1.5,
      size = 3.5,
      color = text_color
    ) +
    scale_y_discrete(breaks = factor(-2:20), limits = c(19:0) |> as.character()) +
    scale_color_manual(values = c("3" = pal[1], "6" = pal[2], "9" = pal[3], "12" = pal[4], "15" = pal[5], "18" = pal[6])) +
    scale_size_manual(values = c("3" = size_arr[1], "6" = size_arr[2], "9" = size_arr[3], "12" = size_arr[4], "15" = size_arr[5], "18" = size_arr[6])) +
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
      panel.grid.minor = element_blank() #remove minor gridlines
    )

  return(plot_)

}

#' @export
get_top_5_gear_data <- function(data) {


  res_ <- data |>
    dplyr::select(starts_with("gear")) |>
    dplyr::collect() |>
    dplyr::mutate(
      local = coalesce(
        gear_local_1_label, gear_local_2_label, gear_local_3_label, gear_local_4_label, gear_local_5_label
      ),
      type = coalesce(
        gear_type_1, gear_type_2, gear_type_3, gear_type_4, gear_type_5
      )
    ) |>
    dplyr::mutate(
      local = if_else(is.na(local) | local == "", "Unknown", local),
      type = if_else(is.na(type) | type == "", "Unknown", type)
    ) |>
    dplyr::group_by(type) |>
    dplyr::summarise(
      count = n(),
      local = paste(unique(local), collapse = "; <br> "),
      .groups = 'drop'
    ) |>
    # Calculate the percentage of each type-local combination
    dplyr::mutate(percentage = (count / sum(count)) * 100) |>
    # Arrange in descending order of count to get the most frequent first
    dplyr::arrange(desc(count)) |>
    dplyr::select(-count) |>
    head(5)


  return(res_)
}


#' @export
DRAW_TOP_FIVE_SPECIES__FAMILY_LEVEL <- function(data, AVAILABLE_ART_FISH_IMG_TBL) {

  res_plot_ <- plot_grid(
    NULL,
    NULL,
    plot_top_5_species_family_level(
      data = data,
      column = 'percent_kg_landed',
      pal = c("#002d48", "#004d68", "#006e8b", "#0091af", "#07b6d5", '#96b0b7'),
      text_color = "#ffffff",
      AVAILABLE_ART_FISH_IMG_TBL = AVAILABLE_ART_FISH_IMG_TBL,
      number_label = FALSE
    ),
    plot_top_5_species_family_level(
      data = data,
            column = 'percent_local_price',
            pal = c("#a63068", "#9c4f98", "#806ebe", "#548ad5", "#19a2db", '#96b0b7'),
            text_color = "#ffffff",
            AVAILABLE_ART_FISH_IMG_TBL = AVAILABLE_ART_FISH_IMG_TBL,
            number_label = FALSE
          ),
    ncol = 2,
    align = "hv",
    rel_widths = c(3, 3),
    rel_heights = c(0, 6)
  )

  return(res_plot_)
}

#' @export
DRAW_TOP_FIVE_SPECIES <- function(data, AVAILABLE_ART_FISH_IMG_TBL) {

  res_plot_ <- plot_grid(
    NULL,
    NULL,
    plot_top_5_species(
      data = data,
      column = 'percent_kg_landed',
      pal = c("#002d48", "#004d68", "#006e8b", "#0091af", "#07b6d5", '#96b0b7'),
      text_color = "#ffffff",
      AVAILABLE_ART_FISH_IMG_TBL = AVAILABLE_ART_FISH_IMG_TBL,
      number_label = FALSE
    ),
    plot_top_5_species(
      data = data,
      column = 'percent_local_price',
      pal = c("#a63068", "#9c4f98", "#806ebe", "#548ad5", "#19a2db", '#96b0b7'),
      text_color = "#ffffff",
      AVAILABLE_ART_FISH_IMG_TBL = AVAILABLE_ART_FISH_IMG_TBL,
      number_label = FALSE
    ),
    ncol = 2,
    align = "hv",
    rel_widths = c(3, 3),
    rel_heights = c(0, 6)
  )

  return(res_plot_)
}
