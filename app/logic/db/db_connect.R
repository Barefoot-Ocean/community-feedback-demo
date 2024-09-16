box::use(
  dplyr[...],
  stringr[...],
  tibble[...],
  bfo.db[...]
)

#' @export
get_filtered_community_feedback_app_data <- function(connection, filters) {

  # browser()

  # Get data
  res_ <- connection |>
    dplyr::filter(
      # country
      country %in% filters$country_selected &
      # subnational
      admin1 %in% filters$subnational_selected &
      # community
      admin3_label %in% filters$community_selected &
      # species_group
      species_group %in% filters$species_group_selected &
      # species_family
      scientific_family %in% filters$species_family_selected
    ) |>
    # date
    dplyr::filter(
      dplyr::between(date_landed, filters$date_selected[1], filters$date_selected[2])
    )

  return(res_)

}

