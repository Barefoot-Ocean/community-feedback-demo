box::use(
  shiny[h3, moduleServer, NS, HTML, tagList,
        tags, h1, h2, img, div, span, uiOutput,
        renderUI, req, renderPlot, plotOutput, outputOptions],
  dplyr[...],
  shiny.i18n[...],
  shinycustomloader[...]
)

box::use(
  app/logic/average_length_time_chart[
    draw_average_length_data_top_five_gridplot,
    DRAW_TOP_LEN_PLOT,
    DRAW_TOP_LEN_PLOT__FAMILY_LEVEL
  ],
)

#' @export
ui <- function(id, i18n) {
  ns <- NS(id)

  tagList(

    div(class = "tab-chart-container",
        div(class = "col-md-12",
            div(class = "secondary-title text-center",
                h2(i18n$translate("")),
            ),
            div(class = "img-fluid",
                # plotOutput(ns("fishers_gender_distribution"), width = "100%")
                shinycustomloader::withLoader(
                  plotOutput(ns("average_length_top_five"), width = "100%", height = "600px"),
                  type = "html",
                  loader = "app-loader/watch-loader"
                )
            )
        )
    )

  ) # End Taglist


}


#' @export
server <- function(id, species_data, AVAILABLE_ART_FISH_IMG_TBL, show_only_family_level) {
  moduleServer(id, function(input, output, session) {
    print("Length Five Chart View module server part works!")

    output$average_length_top_five <- renderPlot({
      req(species_data)

      if(show_only_family_level) {
        plot_ <- DRAW_TOP_LEN_PLOT__FAMILY_LEVEL(species_data, AVAILABLE_ART_FISH_IMG_TBL)

        return(plot_)
      }

      if(!show_only_family_level) {
        plot_ <- DRAW_TOP_LEN_PLOT(species_data, AVAILABLE_ART_FISH_IMG_TBL)

        return(plot_)
      }

    }, bg="transparent")

    outputOptions(output, "average_length_top_five", suspendWhenHidden = FALSE)

  })
}
