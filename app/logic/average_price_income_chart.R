box::use(
  tidyr[pivot_wider, pivot_longer],
  dplyr[...],
  ggplot2[...],
  ggimage[...],
  forcats[fct_rev],
  lubridate[...]
)

box::use(
  app/logic/utils
)

#' @export
TRANSFORM_DATA <- function(data) {

  # browser()

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

  res_ <- data |>
    # select(date_landed, total_weight_landed_kg, total_purchase_price_usd) |>
    select(date_landed, total_weight_landed_kg, total_purchase_price) |>
    # round local currenct to 2 decimal places
    mutate(
      total_purchase_price = as.numeric(total_purchase_price),
      total_purchase_price = round(total_purchase_price, 2)
    ) |>
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
      total_weight_landed_kg = as.numeric(total_weight_landed_kg)
      # ,
      # total_purchase_price_usd = as.numeric(total_purchase_price_usd)
    ) |>
    # select(date_from, total_weight_landed_kg, total_purchase_price_usd) |>
    select(date_from, total_weight_landed_kg, total_purchase_price) |>
    # mutate(date_from = factor(date_from, levels = unique(date_from))) %>%
    group_by(date_from) %>%
    summarise(
      mean_kg_trip = mean(total_weight_landed_kg, na.rm = TRUE),
      # mean_USD_trip = mean(total_purchase_price_usd, na.rm = TRUE)
      mean_price_trip = mean(total_purchase_price, na.rm = TRUE)
    ) %>%
    ungroup() |>
    # select(date_from, mean_kg_trip, mean_USD_trip) |>
    select(date_from, mean_kg_trip, mean_price_trip) |>
    # pivot_longer(cols = c(mean_kg_trip, mean_USD_trip), names_to = "indicator", values_to = "value") |>
    pivot_longer(cols = c(mean_kg_trip, mean_price_trip), names_to = "indicator", values_to = "value") |>
    collect() |>
    mutate(date_from = factor(date_from, levels = unique(date_from)))

  return(res_)

}



#' @export
draw_average_income_price_plot <- function(data, local_currency) {

  chart <- TRANSFORM_DATA(data) %>%
    # test_data %>%
    ggplot(aes(x = date_from, y = indicator, color = indicator, fill = indicator)) +
    geom_line(aes(y = value, group = indicator, size = 3)) +
    geom_area(aes(y = value, group = indicator), alpha = 0.5, position = 'identity') +
    geom_label(aes(y = value, label = paste0(scales::number(value, accuracy = 0.01))), size = 5, label.size = 1, alpha = 0.8, fill = '#ffffff') +
    facet_wrap(~indicator,
               scales = "free_y",
               strip.position = "left",
               # labeller = as_labeller(c(mean_kg_trip = "Average Catch (kG)", mean_USD_trip = "Average Income (USD)"))
               labeller = as_labeller(c(
                 mean_kg_trip = "Average Catch (kG)",
                 mean_price_trip = paste0("Average Income (", local_currency, ")")
               ))
    ) +
    scale_color_manual(values = c("#07b6d5", "#fa9137")) +
    scale_fill_manual(values = c("#07b6d5", "#fa9137")) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    labs(
      x = NULL,
      y = NULL,
      color = 'Indicator'
    ) +
    theme(
      axis.text.x = element_text(angle = 45),
      axis.text = element_text(size = 14, colour = "#2c3e50"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none",
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(size = 14, colour = "#2c3e50"),
      # panel.background = element_rect(fill='transparent'), #transparent panel bg
      # plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      # panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )

  return(chart)
}

