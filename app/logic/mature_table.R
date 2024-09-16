box::use(
  lubridate[...],
  stringr[...],
  utils[...],
  dplyr[...]
)

#' @export
get_mature_table_data <- function(data) {
  # Get data
  res_ <- data |>
    select(scientific_family, ismature) |>
    filter(!is.na(ismature)) |>
    filter(!is.na(scientific_family)) |>
    group_by(scientific_family, ismature) |>
    tally() |>
    # tidyr::pivot_wider(names_from = ismature, names_prefix = "mature_", values_from = n) |>
    group_by(scientific_family) |>
    mutate(
      total_mature = sum(n, na.rm = TRUE),
    ) |>
    filter(ismature == 'True') |>
    mutate(
      perc_mature = n / total_mature * 100
    ) |>
    ungroup() |>
    select(scientific_family, perc_mature) |>
    collect() |>
    arrange(desc(perc_mature))

  # Return the data
  return(res_)
}
