box::use(
  shiny[...],
  dplyr[as_tibble, tibble],
  utils[...],
  polished[...],
  config[...],
  shinyjs[...],
  shiny.i18n[...],
  jsonlite[...],
  httr[GET],
  RcppSimdJson[fparse]
)

box::use(
  app/view/filter_block,
  app/view/main_block,
  app/view/translation_view
)

box::use(
  app/logic/db/db_connect,
  app/logic/utils,
)

### Path to images
AVAILABLE_ART_FISH_IMG_TBL <- readRDS('app/data/available_art_fish_img_tbl.rds')

app_config <- config::get()

# Ttanslation
i18n <- Translator$new(translation_json_path = "app/translations/translations.json")
i18n$set_translation_language("en")


#' @export
ui_inner <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      # HTML("<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.1.3/css/bootstrap.min.css'>"),
      HTML("<link rel='stylesheet' href='https://fonts.googleapis.com/css?family=Lobster&amp;display=swap'>"),
      HTML("<link rel='stylesheet' href='static/primeicons.css'>"),
      HTML("<link rel='stylesheet' href='static/countries_flag.css'>"),
      HTML("<link rel='stylesheet' href='static/species_group.css'>"),
      HTML("<link rel='stylesheet' href='static/gear_type.css'>"),
      HTML("<script src='static/js/index.js' defer></script>"),
      # Remove built in bootstrap
      tags$script(HTML('
        $(document).ready(function() {
          $("link[href=\'bootstrap-3.4.1/css/bootstrap.min.css\']").remove();
        });
      ')),

      # translation_view$ui(ns('translation_view'), i18n = i18n),
      filter_block$ui(ns('filters'), i18n = i18n),
      main_block$ui(ns('main_block'), i18n = i18n),

    )
  )
}

#' @export
server_inner <- function(id) {
  moduleServer(id, function(input, output, session) {

    message("Server started")


    COMMUNITY_FEEDBACK_APP_DATA_FILTERED <- reactiveVal(NULL)
    COUNTRY_LOCAL_CURRENCY <- reactiveVal(NULL)

    # Connections
    mv_community_feedback_app_connection <- readRDS('app/data/dummy_data_mdg.rds')

    translation_view$server(
      'translation_view'
    )

    FILTERS_SELECTED <- filter_block$server(
      'filters',
      monitoring_app_data = mv_community_feedback_app_connection,
      i18n = i18n
    )

    observeEvent(FILTERS_SELECTED(), {
      print(FILTERS_SELECTED())
      COMMUNITY_FEEDBACK_APP_DATA_FILTERED(
        db_connect$get_filtered_community_feedback_app_data(
          connection = mv_community_feedback_app_connection,
          filters = FILTERS_SELECTED()
        )
      )
      COUNTRY_LOCAL_CURRENCY(
        # Generic Currency Symbol
        "¤¤¤"
      )
    })


    observeEvent(COMMUNITY_FEEDBACK_APP_DATA_FILTERED(), {
      main_block$server(
        "main_block",
        gender_data = utils$get_gender_data(COMMUNITY_FEEDBACK_APP_DATA_FILTERED()),
        monitoring_app_data = COMMUNITY_FEEDBACK_APP_DATA_FILTERED(),
        species_data = SPECIES_DATA_TBL,
        AVAILABLE_ART_FISH_IMG_TBL = AVAILABLE_ART_FISH_IMG_TBL,
        GEAR_TYPE = COMMUNITY_FEEDBACK_APP_DATA_FILTERED(),
        show_only_family_level = FILTERS_SELECTED()$family_level_grouping,
        local_currency = COUNTRY_LOCAL_CURRENCY(),
        i18n = i18n
      )

    })


  })
}


#' @export
ui <- ui_inner('app') # dev

#' @export
server <- function(input, output, session) {
  server_inner('app')
}


