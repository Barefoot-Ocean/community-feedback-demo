box::use(
  shiny[h3, moduleServer, NS, HTML, tagList,
        tags, h1, h2, img, div, span, uiOutput,
        renderUI, req, renderPlot, plotOutput, outputOptions],
  dplyr[...],
  shiny.i18n[...],
  # shinycssloaders[...],
  shinycustomloader[...]
)

box::use(
  app/logic/average_price_income_chart[draw_average_income_price_plot],
  app/logic/total_price_income_chart[draw_total_income_price_plot],
)


#' @export
ui <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    usei18n(i18n),
    div(class = "tab-chart-container",
        div(class = "col-md-12",
            div(class = "header-title-facet",
                div(class = "secondary-title text-center",
                    h2(i18n$translate("Average Catch per Trip")),
                ),
                div(class = "secondary-title text-center",
                    h2(i18n$translate("Average  Income per Trip")),
                )
            ),
            div(class = "img-fluid",
                # plotOutput(ns("average_income_chart"), width = "100%") |> shinycssloaders::withSpinner()

                shinycustomloader::withLoader(
                  plotOutput(ns("average_income_chart"), width = "100%"),
                  type = "html",
                  loader = "app-loader/watch-loader"
                )
            )
        ),
        div(class = "col-md-12",
            div(class = "header-title-facet",
                div(class = "secondary-title text-center",
                    h2(i18n$translate("Total Catch per Trip")),
                ),
                div(class = "secondary-title text-center",
                    h2(i18n$translate("Total Income per Trip")),
                )
            ),
            div(class = "img-fluid",
                plotOutput(ns("total_income_chart"))
            )
        )
    )

  ) # End Taglist


}


#' @export
server <- function(id, species_data, local_currency) {
  moduleServer(id, function(input, output, session) {
    print("Catch-Income Chart View module server part works!")

    output$average_income_chart <- renderPlot({
      req(species_data)

      draw_average_income_price_plot(species_data, local_currency)
    }, bg = "transparent")

    output$total_income_chart <- renderPlot({
      req(species_data)

      draw_total_income_price_plot(species_data, local_currency)
    }, bg="transparent")

    outputOptions(output, "average_income_chart", suspendWhenHidden = FALSE)
    outputOptions(output, "total_income_chart", suspendWhenHidden = FALSE)


  })
}
