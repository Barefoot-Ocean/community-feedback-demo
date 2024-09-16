box::use(
  shiny[h3, moduleServer, NS, HTML, tagList,
        tags, h1, h2, img, div, span, uiOutput,
        renderUI, req, renderPlot, plotOutput, outputOptions],
  dplyr[...],
  reactable[...],
  shiny.i18n[...],
  shinycustomloader[...]
)

box::use(
  app/logic/get_top_5_function[
    plot_top_5_species,
    get_top_5_gear_data,
    plot_top_5_species_family_level,
    DRAW_TOP_FIVE_SPECIES__FAMILY_LEVEL,
    DRAW_TOP_FIVE_SPECIES,
  ]
)

#' @export
ui_top_fish_name <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    usei18n(i18n),
                div(class = "tab-chart-container",
                    div(class = "col-md-12",
                      div(class = "header-title-facet",
                          div(class = "col-md-6",
                              div(class = "secondary-title text-center",
                                  h2(i18n$translate("Total Weight")),
                              ),
                              # div(class = "img-fluid",
                              #     plotOutput(ns("top_five_kg_landed"), width = "100%", height = "500px")
                              # )
                          ),
                          div(class = "col-md-6",
                              div(class = "secondary-title text-center",
                                  h2(i18n$translate("Total Income")),
                              ),
                              # div(class = "img-fluid",
                              #     plotOutput(ns("top_five_price"), width = "100%", height = "500px")
                              # )
                          )
                        ),
                        div(class = "img-fluid",
                          # plotOutput(ns("top_five_species_chart"), width = "100%", height = "500px")
                          shinycustomloader::withLoader(
                            plotOutput(ns("top_five_species_chart"), width = "100%", height = "500px"),
                            type = "html",
                            loader = "app-loader/watch-loader"
                          )
                        )
                  )# End col-md-12
                ) # End tab-chart-container

  ) # End Taglist


}


#' @export
server_top_fish_name <- function(id, species_data, AVAILABLE_ART_FISH_IMG_TBL, GEAR_TYPE, show_only_family_level, i18n, session) {
  moduleServer(id, function(input, output, session) {
    print("Top Five Chart View module server part works!")

    output$top_five_species_chart <- renderPlot({
      req(species_data)

      if(show_only_family_level) {
        plot_ <- DRAW_TOP_FIVE_SPECIES__FAMILY_LEVEL(
          data = species_data,
          AVAILABLE_ART_FISH_IMG_TBL = AVAILABLE_ART_FISH_IMG_TBL
        )

        return(plot_)
      }

      if(!show_only_family_level) {
        plot_ <- DRAW_TOP_FIVE_SPECIES(
          data = species_data,
          AVAILABLE_ART_FISH_IMG_TBL = AVAILABLE_ART_FISH_IMG_TBL
        )

        return(plot_)
      }

    }, bg = "transparent")

    outputOptions(output, "top_five_species_chart", suspendWhenHidden = FALSE)

  })
}


#' @export
ui_top_gears <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    usei18n(i18n),
    div(class = "tab-chart-container",
        div(class = "col-md-12",
            # reactableOutput(ns("gear_table"), width = "100%")
            shinycustomloader::withLoader(
              reactableOutput(ns("gear_table"), width = "100%"),
              type = "html",
              loader = "app-loader/watch-loader"
            )
        )
    )
  ) # End Taglist

}

#' @export
server_top_gears <- function(id, GEAR_TYPE, i18n, session) {
  moduleServer(id, function(input, output, session) {
    print("Top Five Gears View module server part works!")

    # Gear Table
    output$gear_table <- renderReactable({
      req(GEAR_TYPE)

      # browser()

      reactable(
        get_top_5_gear_data(GEAR_TYPE),
        searchable = FALSE,
        bordered = TRUE,
        columns = list(
          type = colDef(
            name = "Type",
            minWidth = 100,
            headerStyle = list(textAlign = "left"),  # Align title
            html = TRUE,
            cell = function(value) {
              tags$div(
                class = "cell-container",
                span(
                  class = paste0("gear-type", " gear-type-", tolower(value)),
                  style = "margin-right: 15px; height: 4rem; width: 40px;"
                ),
                value
              )
            }
          ),
          local = colDef(
            name = "Local Name",
            minWidth = 200,
            headerStyle = list(textAlign = "left"),  # Align title
            html = TRUE,
            cell = function(value) {
              HTML(
                paste0(
                  "<div class='cell-container'>",
                  value,
                  "</div>"
                )
              )
            }
          ),
          percentage = colDef(
            name = "Value",
            minWidth = 100,
            align = "right",
            cell = function(value) {
              div(class = "cell-container",
                  style = "justify-content: center;",
                  span(
                    class = "top-five-gear-percentage",
                    paste0(round(value, 5), " %")
                  )
              )
            }
          )
        )
      )

    })

    outputOptions(output, "gear_table", suspendWhenHidden = FALSE)

  })
}

