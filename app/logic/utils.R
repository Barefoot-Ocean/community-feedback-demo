box::use(
  lubridate[...],
  stringr[...],
  utils[...],
  dplyr[...]
)

#' @export
get_time_unit <- function(start_date, end_date) {
  timelen <- time_length(interval(ymd(start_date), ymd(end_date)), "days")

  if(timelen < 14) {
    return('day')
  } else if(timelen < 60) {
    return('week')
  } else if(timelen < 365) {
    return('month')
  } else {
    return('year')
  }

}

#' @export
get_gender_data <- function(data) {

  data_gender <- data |>
    dplyr::select(buyer_id, buyer_gender, fisher_id, fisher_gender)

  buyer_data <- data_gender |>
    dplyr::select(buyer_id, buyer_gender) |>
    dplyr::filter(buyer_gender %in% c('female', 'male')) |>
    dplyr::collect()

  fisher_data <- data_gender |>
    dplyr::select(fisher_id, fisher_gender) |>
    dplyr::filter(fisher_gender %in% c('female', 'male')) |>
    dplyr::collect()

  return(
    list(
      buyer_info = buyer_data,
      fisher_info = fisher_data
    )
  )

}


#' @export
clean_input_values <- function(vec) {
  # Clean the vector by removing invalid entries
  cleaned_vec <- vec[!is.na(vec) & vec != "" & !is.null(vec)]

  # Return a vector containing the cleaned vector
  return(cleaned_vec)
}

#' @export
extract_items_prime_react_multiselect <- function(named_vec) {
  named_vec[names(named_vec) %in% "item"] |> unname()
}
