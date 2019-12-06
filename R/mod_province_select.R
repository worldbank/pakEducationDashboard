# Module UI
  
#' @title   mod_province_select_ui and mod_province_select_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_province_select
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_province_select_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("indicator"),
                label = "Choose an indicator",
                choices = indicator_choices_province,
                selectize = TRUE,
                selected = "division_9_11"),
    selectInput(inputId = ns("province"),
                label = "Choose one or more province(s)",
                choices = sort(unique(pakeduc_province[["province"]])),
                multiple = TRUE,
                selectize = TRUE,
                selected = sort(unique(pakeduc_province[["province"]]))),
    checkboxInput(inputId = ns("gender"),
                  label = "Disaggregate by gender",
                  value = FALSE),
    selectInput(inputId = ns("dataset"),
                label = "Choose one or more survey(s)",
                choices = sort(unique(pakeduc_province[["dataset"]])),
                selectize = TRUE,
                selected = c("aser", "hies", "mics", "pslm"),
                multiple = TRUE)
  )
}
    
# Module Server
    
#' @rdname mod_province_select
#' @export
#' @keywords internal
    
mod_province_select_server <- function(input, output, session){
  ns <- session$ns
  
  return(
    list(
      indicator     = reactive({ input$indicator }),
      gender        = reactive({ input$gender }),
      dataset       = reactive({ input$dataset }),
      province      = reactive({ input$province })
    )
  )
}
    
## To be copied in the UI
# mod_province_select_ui("province_select_ui_1")
    
## To be copied in the server
# callModule(mod_province_select_server, "province_select_ui_1")
 
