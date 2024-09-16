box::use(
  shiny[...],
  dplyr[...],
  shinyWidgets[...],
  shiny.i18n[...],
  primereact[...],
  polished[...]
)

box::use(
  app/logic/get_filter_values,
  app/logic/constants,
  app/logic/utils
)


#' @export
ui <- function(id, i18n) {
  ns <- NS(id)

  tagList(
  div(class = "block-conteiner",
  div(class = "filter-block-card",
    usei18n(i18n),
    div(class = "button-controlles-one",
      div(
          class = "filter-block-card-input-style",
          primereact::select_input(
            inputId = ns("country_selected"),
            value = NULL,
            width = "100%",
            options = NULL,
            placeholder = i18n$translate("Country")$children[[1]], # get children of the i18n object
            iconClass="country-flag country-flag-"
          )

      ),
      div(
          class = "filter-block-card-input-style",
          primereact::select_input(
            inputId = ns("subnational_selected"),
            value = NULL,
            options = NULL,
            width = "100%",
            placeholder = i18n$translate("Subnational")$children[[1]] # get children of the i18n object
          )
      ),
      div(
          class = "filter-block-card-input-style",
          primereact::select_input(
            inputId = ns("community_selected"),
            value = NULL,
            options = NULL,
            width = "100%",
            placeholder = i18n$translate("Community")$children[[1]] # get children of the i18n object
          )
      ),
    ), # end fluidRow
    div(class = "button-controlles-two",
      div(
          class = "filter-block-card-input-style",
          primereact::multiple_select_input(
            inputId = ns("species_group_selected"),
            value = NULL,
            options = NULL,
            width = "100%",
            iconClass = "species-group species-group-"
          )
      ),
      div(
          class = "filter-block-card-input-style",
          primereact::multiple_select_input(
            inputId = ns("species_family_selected"),
            value = NULL,
            width = "100%",
            options = NULL
          )
      ),
      div(
          class = "filter-block-card-input-style",
          primereact::date_range_input(
            inputId = ns("date_selected"),
            width = "100%",
            placeholder = i18n$translate("Date")$children[[1]], # get children of the i18n object
            value = NULL
          )

      )
    ), # end fluidRow
    div(class = "button-controlles-three",
      div(
        toggle_button(
          inputId = ns("family_level_grouping"),
          value = FALSE,
          onLabel = i18n$translate("Group by family level")$children[[1]],
          offLabel = "Don't group by family level",
          onIcon = "pi pi-check",
          width = "100%",
          offIcon = "pi pi-times"
        ),
      ),
      div(
        action_button(
          inputId = ns("apply_filter"),
          label = i18n$translate("Apply filter")$children[[1]],
          icon = "pi pi-check"
        )
      )
    )
  )
  ),
    div(
      div(id = "scroll-to-filter-button",
          class = "scroll-to-filter-button",
          img(src = "static/img/filter.png")
      ),
      div(id = "exit-button",
          class = "exit-button",
          img(src = "static/img/exit.png")
      )
    )
  )# End Taglist

}

#' @export
server <- function(id, API_USER_INFO = NULL, monitoring_app_data = NULL, i18n) {
  moduleServer(id, function(input, output, session) {

    FILTER_VALUES <- reactiveVal(NULL)

    # DATABASE BLOCK
    countries <- reactive({
      req(monitoring_app_data)

      res_ <- get_filter_values$get_country_values(monitoring_app_data)
      # Clean result
      res_ <- res_ |> utils$clean_input_values()
      res_ <- lapply(names(res_), function(name) {
        list(title = name, item = res_[[name]])
      })

      return(res_)

    })

    observeEvent(countries(), {
      primereact::update_select_input(
        session,
        inputId = "country_selected",
        value = countries()[[1]],
        configuration = list(
          options = countries(),
          width = "100%",
          iconClass="country-flag country-flag-"
        )
      )
    })

    observeEvent(input$country_selected$item, {
      res_ <- get_filter_values$get_subnational_values(
        connection = monitoring_app_data,
        country_selected = input$country_selected$item
      )
      # Clean result
      res_ <- res_ |> utils$clean_input_values()

      res_ <- lapply(res_, function(name) {
        list(title = name, item = name)
      })

      primereact::update_select_input(
        session,
        inputId = "subnational_selected",
        value = res_[[1]],
        configuration = list(
          options = res_,
          width = "100%"
        )
      )
    })

    observeEvent(input$subnational_selected$item, {
      res_ <- get_filter_values$get_communities_values(
        connection = monitoring_app_data,
        country_selected = input$country_selected$item,
        subnational_selected = input$subnational_selected$item
      )
      res_ <- lapply(res_, function(name) {
        list(title = name, item = name)
      })
      # Clean result
      res_ <- res_ |> utils$clean_input_values()

      # Update input
      primereact::update_select_input(
        session,
        inputId = "community_selected",
        value = res_[[1]],
        configuration = list(
          width = "100%",
          options = res_
        )
      )
    })

    observeEvent(input$community_selected$item, {

      res_ <- get_filter_values$get_species_group_values(
        connection = monitoring_app_data,
        country_selected = input$country_selected$item,
        subnational_selected = input$subnational_selected$item,
        community_selected = input$community_selected$item
      )
      # Clean result
      res_ <- res_ |> utils$clean_input_values()

      res_ <- constants$species_group_structure[res_]

      output_list <- lapply(res_, function(x) {
        list(title = x$title, item = x$item)
      }) |> unname()


      # Update input
      primereact::update_multiple_select_input(
        session,
        inputId = "species_group_selected",
        value = output_list,
        configuration = list(
          options = output_list,
          width = "100%",
          iconClass="species-group species-group-"
        )
      )
    })

    observeEvent(input$species_group_selected, {

      res_ <- get_filter_values$get_scientific_family_values(
        connection = monitoring_app_data,
        country_selected = input$country_selected$item,
        subnational_selected = input$subnational_selected$item,
        community_selected = input$community_selected$item,
        species_group_selected = input$species_group_selected
      )
      # Clean result
      res_ <- res_ |> utils$clean_input_values()

      res_ <- lapply(res_, function(name) {
        list(title = name, item = name)
      })

      # Update input
      primereact::update_multiple_select_input(
        session,
        inputId = "species_family_selected",
        value = res_,
        configuration = list(
          width = "100%",
          options = res_
        )
      )

    })

    observeEvent(input$species_family_selected, {
      date_range_selection <- get_filter_values$get_date_range_values(
        connection = monitoring_app_data,
        country_selected = input$country_selected$item,
        subnational_selected = input$subnational_selected$item,
        community_selected = input$community_selected$item,
        species_group_selected = input$species_group_selected,
        scientific_family_selected = input$species_family_selected
      )

      # If only one date available, set the end date to the start date
      if(!isTruthy(date_range_selection[2])) date_range_selection[2] <- date_range_selection[1]

      primereact::update_date_range_input(
        session,
        inputId = "date_selected",
        value = c(as.character(date_range_selection[1]), as.character(date_range_selection[2])),
        configuration = list(
          placeholder = i18n$translate("Date"), # get children of the shiny.tag object,
          # placeholder = "Date",
          width = "100%",
          minDate = date_range_selection[1] |> as.character(),
          maxDate = date_range_selection[2] |> as.character()
        )
      )

    })


    observeEvent(input$apply_filter, {
      FILTER_VALUES(
        list(
          country_selected = input$country_selected$item,
          subnational_selected = input$subnational_selected$item,
          community_selected = input$community_selected$item,
          species_group_selected = utils$extract_items_prime_react_multiselect(input$species_group_selected),
          species_family_selected = utils$extract_items_prime_react_multiselect(input$species_family_selected),
          date_selected = input$date_selected |> as.Date(),
          family_level_grouping = input$family_level_grouping
        )
      )

    }, ignoreInit = TRUE)


    # Sign out button
    observeEvent(input$exit_button_clicked, {
      session$reload()
    })

    return(reactive({FILTER_VALUES()}))


  })
}
