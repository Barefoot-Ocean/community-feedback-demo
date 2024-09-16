box::use(
  shiny[...],
  dplyr[...],
  shiny.i18n[...],
  shinycustomloader[...]
)

box::use(
  app/logic/gender_chart[TRANSFORM_DATA, draw_total_fishers_number_plot, TRANSFORM_DATA_BUYER, draw_total_buyers_number_plot],
)


#' @export
ui <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    usei18n(i18n),
    div(class = "gender-container",
        div(class = "col-md-6",
            div(class = "secondary-title text-center",
                h2(i18n$translate("Total Fishers - NUMBER")),
            ),
            div(class = "img-fluid",
                # plotOutput(ns("fishers_gender_distribution"), width = "100%")
                shinycustomloader::withLoader(
                  plotOutput(ns("fishers_gender_distribution"), width = "100%"),
                  type = "html",
                  loader = "app-loader/watch-loader"
                )
            )
        ),
        div(class = "col-md-6",
            div(class = "secondary-title text-center",
                h2(class = "text-center", i18n$translate("Total Fish Buyers - NUMBER")),
            ),
            div(class = "img-fluid",
                # plotOutput(ns("buyers_gender_distribution"), width = "100%")
                shinycustomloader::withLoader(
                  plotOutput(ns("buyers_gender_distribution"), width = "100%"),
                  type = "html",
                  loader = "app-loader/watch-loader"
                )
            )
        )
    )

  ) # End Taglist


}


#' @export
server <- function(id, gender_data) {
  moduleServer(id, function(input, output, session) {
    print("Gender Chart View module server part works!")

    output$fishers_gender_distribution <- renderPlot({
      req(gender_data)

        draw_total_fishers_number_plot(
            TRANSFORM_DATA(gender_data$fisher_info)
        )

    }, bg = "transparent")

    output$buyers_gender_distribution <- renderPlot({
      req(gender_data)

      draw_total_buyers_number_plot(TRANSFORM_DATA_BUYER(gender_data$buyer_info))
    }, bg = "transparent")

    outputOptions(output, "fishers_gender_distribution", priority = 3, suspendWhenHidden = FALSE)
    outputOptions(output, "buyers_gender_distribution", priority = 1, suspendWhenHidden = FALSE)

  })
}
