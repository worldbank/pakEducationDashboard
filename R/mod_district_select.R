# Module UI
  
#' @title   mod_district_select_ui and mod_district_select_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_district_select
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_district_select_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("indicator"),
                label = "Choose an indicator",
                choices = indicator_choices_district,
                selectize = TRUE,
                selected = "division_9_11"),
    selectInput(inputId = ns("province"),
                label = "Choose one or more province(s)",
                choices = sort(unique(pakeduc_province[["province"]])),
                multiple = TRUE,
                selectize = TRUE,
                selected = sort(unique(pakeduc_province[["province"]]))),
    selectInput(inputId = ns("district"),
                label = "Choose one or more district(s)",
                choices = sort(unique(pakeduc_district[["dist_nm"]])),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Attock", "Lahore", "Chakwal", "Rajanpur")),
    checkboxInput(inputId = ns("gender"),
                  label = "Disaggregate by gender",
                  value = FALSE),
    selectInput(inputId = ns("dataset"),
                label = "Choose one or more survey(s)",
                choices = sort(unique(pakeduc_district[["dataset"]])),
                selectize = TRUE,
                multiple = TRUE),
    sliderInput(inputId = ns("year"),
                label = "Select a year",
                min = min(pakeduc_district$year, na.rm = TRUE),
                max = max(pakeduc_district$year, na.rm = TRUE),
                value = max(pakeduc_district$year, na.rm = TRUE))
  
  )
}
    
# Module Server
    
#' @rdname mod_district_select
#' @export
#' @keywords internal
    
mod_district_select_server <- function(input, output, session){
  ns <- session$ns
  
  province <- reactive({
    dplyr::filter(pakeduc_province, province %in% input$province)
  })
  
  observeEvent(province(), {
    provinces <- unique(province()$province)
    choices_district <- unique(pakeduc_district$dist_nm[pakeduc_district$province %in% provinces])
    updateSelectizeInput(session, "district",
                         choices = choices_district,
                         selected = input$district)
  })
  
  return(
    list(
      indicator     = reactive({ input$indicator }),
      gender        = reactive({ input$gender }),
      dataset       = reactive({ input$dataset }),
      province      = reactive({ input$province }),
      district      = reactive({ input$district }),
      year          = reactive({ input$year })
    )
  )
}
    
## To be copied in the UI
# mod_district_select_ui("district_select_ui_1")
    
## To be copied in the server
# callModule(mod_district_select_server, "district_select_ui_1")
 
