box::use(
  shiny[...],
  dplyr[...],
  # bslib[...],
  shinyWidgets[...],
  shiny.i18n[...]
)


#' @export
ui <- function(id, i18n) {
  ns <- NS(id)

  fluidPage(
    usei18n(i18n),
    fluidRow(
      column(2, offset = 10,
             selectInput(
               ns("selected_language"),
               i18n$translate("Change language"),
               choices = i18n$get_languages(),
               selected = i18n$get_key_translation()
             )
      )
    )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$selected_language, {
      update_lang(input$selected_language)
    })

  })
}
