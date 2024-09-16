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
  app/logic/average_length_time_chart[draw_average_length_data_top_five_gridplot, DRAW_TOP_LEN_PLOT],
  app/logic/mature_table
)




#' @export
ui <- function(id, i18n) {
  ns <- NS(id)

  tagList(

    div(class = "tab-chart-container",
        div(class = "col-md-12",
            # div(class = "twitter-followers",
                # reactableOutput(ns("adults_table"), width = "100%"),
                shinycustomloader::withLoader(
                  reactableOutput(ns("adults_table"), width = "100%"),
                  type = "html",
                  loader = "app-loader/watch-loader"
                )
            # )
        )
    )

  ) # End Taglist


}


#' @export
server <- function(id, species_data, i18n) {
  moduleServer(id, function(input, output, session) {
    print("Adults Chart View module server part works!")

    # Adults Table
    output$adults_table <- renderReactable({
      req(species_data)

      reactable(
        mature_table$get_mature_table_data(species_data),
        pagination = FALSE,
        searchable = FALSE,
        bordered = TRUE,
        defaultColDef = colDef(headerClass = "header", align = "left"),
        columns = list(
          scientific_family = colDef(
            name = i18n$translate("Scientific Family"),
            cell = function(value) {
              div(
                style = "display: flex; align-items: center; height: 1.75rem;",
                value
              )
            },
            width = 250
          ),
          perc_mature = colDef(
            name = i18n$translate("Adults %"),
            defaultSortOrder = "desc",
            cell = function(value) {
              width <- paste0(round(value, digits = 2), "%")
              bar_style = paste0("margin-right: 0.375rem;
                                  margin-left: 0.375rem;
                                  background: #06b6d438;
                                  border-radius: 15px;
                                  height: 1.75rem;
                                  padding: 5px 10px;
                                  color: #06b6d4;
                                  font-weight: bold;
                                  width:", value, "%;")

              div(class = "bar-cell",
                  style = "display: flex; align-items: center; justify-content: flex-start;",
                  div(
                    style = bar_style,
                    span(class = "number", style = "position: absolute", width),
                  )
              )

            }
          )
        ),
        compact = TRUE,
        class = "followers-tbl"
      )

    })

    outputOptions(output, "adults_table", suspendWhenHidden = FALSE)

  })
}
