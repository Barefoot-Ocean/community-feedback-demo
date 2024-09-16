box::use(
  shiny[h3, moduleServer, NS, HTML, tagList,
        tags, h1, h2, img, div, p, span, uiOutput,
        renderUI, req, renderPlot, plotOutput, outputOptions,
        observe, observeEvent, actionButton, reactiveVal, invalidateLater, reactive],
  reactable[...],
  dplyr[...],
  shiny.i18n[...]
)

box::use(
  app/logic/gender_chart[TRANSFORM_DATA, draw_total_fishers_number_plot],
  app/logic/average_price_income_chart[draw_average_income_price_plot],
  app/logic/total_price_income_chart[draw_total_income_price_plot],
  app/logic/average_length_time_chart[draw_average_length_data_top_five_gridplot, DRAW_TOP_LEN_PLOT],
  app/logic/get_top_5_function[plot_top_5_species]
)

box::use(
  app/view/chart_block/gender_block,
  app/view/chart_block/catch_income_block,
  app/view/chart_block/top_five_block,
  app/view/chart_block/average_length_block,
  app/view/chart_block/adults_block
)


#' @export
ui <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    div(class = "block-conteiner",
      div(class = "main-block-container",
        div(class = "topic", "Community Feedback"),
        div(class = "content",
            # Radio buttons (Tabs)
            tags$input(
              type = "radio",
              name = "slider",
              checked = "checked",
              id = "gender_distribution__tabitem"
            ),
            tags$input(
              type = "radio",
              name = "slider",
              id = "catch_income__tabitem"
            ),
            tags$input(
              type = "radio",
              name = "slider",
              id = "catch_composition__tabitem"
            ),
            tags$input(
              type = "radio",
              name = "slider",
              id = "top_gear__tabitem"
            ),
            tags$input(
              type = "radio",
              name = "slider",
              id = "average_length__tabitem"
            ),
            tags$input(
              type = "radio",
              name = "slider",
              id = "adults_catch__tabitem"
            ),
            # Tab panel
            div(class = "list",
                tags$label(
                  `for` = "gender_distribution__tabitem",
                  class = "gender_distribution__tabitem",
                  tags$i(class = "fas fa-gender-distribution"),
                  span(class = "title", "Gender Distribution")
                ),
                tags$label(
                  `for` = "catch_income__tabitem",
                  class = "catch_income__tabitem",
                  `onclick` = "triggerUpdatePlotSize()",
                  span(class = "icon",
                      tags$i(class = "fas fa-catch-income")
                  ),
                  span(class = "title","Catch and Income")
                ),
                tags$label(
                  `for` = "catch_composition__tabitem",
                  class = "catch_composition__tabitem",
                  `onclick` = "triggerUpdatePlotSize()",
                  span(class = "icon",
                      tags$i(class = "far fa-composition")
                  ),
                  span(class = "title", "Catch composition")
                ),
                tags$label(
                  `for` = "top_gear__tabitem",
                  class = "top_gear__tabitem",
                  span(class = "icon",
                       tags$i(class = "far fa-top-gears")
                  ),
                  span(class = "title", "Top Gears")
                ),
                tags$label(
                  `for` = "average_length__tabitem",
                  class = "average_length__tabitem",
                  `onclick` = "triggerUpdatePlotSize()",
                  span(class = "icon",
                       tags$i(class = "fas fa-length")
                  ),
                  span(class = "title", "Average length")
                ),
                tags$label(
                  `for` = "adults_catch__tabitem",
                  class = "adults_catch__tabitem",
                  span(class = "icon",
                       tags$i(class = "far fa-adult-fish")
                  ),
                  span(class = "title", "Adults in Catch")
                ),
                div(class = "slider")
            ), # End Tab panel
            # Tab Content
            div(class = "text-content",
                div(class = "gender_distribution__tabitem text",
                    # div(class = "title",
                    #     "gender_distribution__tabitem Content"),
                    gender_block$ui(ns('gender_chart'), i18n = i18n)
                ),
                div(class = "catch_income__tabitem text",
                    # div(class = "title",
                    #     "catch_income__tabitem Content"),
                    catch_income_block$ui(ns('catch_income_chart'), i18n = i18n)
                ),
                div(class = "catch_composition__tabitem text",
                    div(class = "title", "Catch Composition Block"),
                    top_five_block$ui_top_fish_name(ns('top_five_charts'), i18n = i18n)
                ),
                div(class = "top_gear__tabitem text",
                    div(class = "title", "Top 5 Gears"),
                    top_five_block$ui_top_gears(ns('top_five_gears'), i18n = i18n)
                ),
                div(class = "average_length__tabitem text",
                    div(class = "title", "Average Length (over time) block"),
                    average_length_block$ui(ns('average_length_chart'), i18n = i18n)
                ),
                div(class = "adults_catch__tabitem text",
                    div(class = "title", "Proportion of Adults in Catch"),
                    adults_block$ui(ns('adults_chart'), i18n = i18n)
                )
              ) # End Tab Content
            ) # End Tabpanel container
        ) # End main-block
    ) # End block-container
  ) # End tagList
}


#' @export
server <- function(id, gender_data, monitoring_app_data, species_data, AVAILABLE_ART_FISH_IMG_TBL, GEAR_TYPE, show_only_family_level, i18n, local_currency) {
  moduleServer(id, function(input, output, session) {
    print("Main Block module server part works!")

    gender_block$server(
      'gender_chart',
      gender_data = gender_data
    )

    catch_income_block$server(
      'catch_income_chart',
      species_data = monitoring_app_data,
      local_currency = local_currency
    )

    top_five_block$server_top_fish_name(
      'top_five_charts',
      species_data = monitoring_app_data,
      AVAILABLE_ART_FISH_IMG_TBL = AVAILABLE_ART_FISH_IMG_TBL,
      GEAR_TYPE = GEAR_TYPE,
      show_only_family_level = show_only_family_level,
      i18n = i18n
    )

    average_length_block$server(
      'average_length_chart',
      species_data = monitoring_app_data,
      AVAILABLE_ART_FISH_IMG_TBL = AVAILABLE_ART_FISH_IMG_TBL,
      show_only_family_level = show_only_family_level
    )

    adults_block$server(
      'adults_chart',
      species_data = monitoring_app_data,
      i18n = i18n
    )

    top_five_block$server_top_gears(
      'top_five_gears',
      GEAR_TYPE = monitoring_app_data,
       i18n = i18n
    )

  })
}
