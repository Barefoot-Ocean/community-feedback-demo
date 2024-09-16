box::use(
  lubridate[...],
  stringr[...],
  utils[...],
  dplyr[...]
)

#' @export
get_country_values <- function(connection) {
  countries <- connection |>
    dplyr::select(country, country_name) |>
    dplyr::distinct() |>
    dplyr::arrange(country) |>
    dplyr::collect() |>
    dplyr::pull(var = country, name =  country_name)

  return(countries)
}

#' @export
get_subnational_values <- function(connection, country_selected) {

  subnational <- connection |>
    dplyr::select(admin1, country) |>
    dplyr::filter(country == country_selected) |>
    dplyr::distinct() |>
    dplyr::select(admin1) |>
    dplyr::arrange(admin1) |>
    # tidyr::pivot_longer(cols = c('admin1', 'admin2')) |>
    # dplyr::select(value) |>
    # dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::pull()

  return(subnational)

}


#' @export
get_communities_values <- function(connection, country_selected, subnational_selected) {

  # browser()

  subnational <- connection |>
    dplyr::filter(country == country_selected & admin1 == subnational_selected) |>
    dplyr::select(admin3_label) |>
    dplyr::distinct() |>
    dplyr::arrange(admin3_label) |>
    dplyr::pull()

  return(subnational)

}


#' @export
get_species_group_values <- function(connection, country_selected, subnational_selected, community_selected) {

  species_group <- connection |>
    dplyr::filter(country == country_selected & admin1 == subnational_selected & admin3_label == community_selected) |>
    dplyr::select(species_group) |>
    dplyr::mutate(species_group = ifelse(species_group != 'All', stringr::str_to_lower(species_group), species_group)) |>
    dplyr::distinct() |>
    dplyr::arrange(species_group) |>
    dplyr::collect() |>
    dplyr::pull()

  return(species_group)

}

#' @export
get_scientific_family_values <- function(connection, country_selected, subnational_selected, community_selected, species_group_selected) {

  scientific_family <- connection |>
    dplyr::filter(
      country == country_selected &
        admin1 == subnational_selected &
        admin3_label == community_selected &
        species_group %in% species_group_selected
    ) |>
    dplyr::select(scientific_family) |>
    dplyr::distinct() |>
    dplyr::arrange(scientific_family) |>
    dplyr::collect() |>
    dplyr::pull()

  return(scientific_family)

}

#' @export
get_date_range_values <- function(connection, country_selected, subnational_selected, community_selected, species_group_selected, scientific_family_selected) {

  date_range <- connection |>
    dplyr::filter(
      country == country_selected &
        admin1 == subnational_selected &
        admin3_label == community_selected &
        species_group %in% species_group_selected &
        scientific_family %in% scientific_family_selected
    ) |>
    dplyr::select(date_landed) |>
    dplyr::distinct() |>
    dplyr::arrange(date_landed) |>
    # Select the first and last date
    dplyr::filter(
      dplyr::row_number() == 1 |
      dplyr::row_number() == n()
    ) |>
    dplyr::collect() |>
    dplyr::pull()

  return(date_range)

}
