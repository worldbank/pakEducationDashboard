# Module UI
  
#' @title   mod_main_ui_ui and mod_main_ui_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_main_ui
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_main_ui_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Input: Slider for the number of bins ----
    selectInput(inputId = ns("indicator"),
                label = "Choose an indicator",
                choices = sort(unique(pakeduc_district[["indicator"]])),
                selectize = TRUE,
                selected = "in_school"),
    selectInput(inputId = ns("province"),
                label = "Choose one or more province(s)",
                choices = sort(unique(pakeduc_province[["province"]])),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Punjab")),
    selectInput(inputId = ns("district_name"),
                label = "Choose one or more district(s)",
                choices = sort(unique(pakeduc_district[["dist_nm"]])),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Attock", "Lahore", "Chakwal", "Rajanpur")),
    selectInput(inputId = ns("age_range"),
                label = "Choose an age range",
                choices = sort(unique(pakeduc_district[["age_range"]])),
                selectize = TRUE,
                selected = "11 to 16"),
    selectInput(inputId = ns("gender"),
                label = "Choose one or more gender(s)",
                choices = sort(unique(pakeduc_district[["gender"]])),
                selectize = TRUE,
                selected = c("Boy", "Girl"),
                multiple = TRUE),
    selectInput(inputId = ns("dataset"),
                label = "Choose one or more survey(s)",
                choices = sort(unique(pakeduc_district[["dataset"]])),
                selectize = TRUE,
                selected = c("aser"),
                multiple = TRUE),
    # sliderInput(inputId = ns("year"),
    #             label = "Choose a year range",
    #             min = min(pakeduc_district[["year"]], na.rm = TRUE),
    #             max = max(pakeduc_district[["year"]], na.rm = TRUE),
    #             value = c(2012, 2018))
  )
}
    
# Module Server
    
#' @rdname mod_main_ui
#' @export
#' @keywords internal
    
mod_main_ui_server <- function(input, output, session){
  ns <- session$ns
  
  indicator <- reactive({
    dplyr::filter(pakeduc_district, indicator %in% input$indicator)
  })
  
  observeEvent(indicator(), {
    choices_dataset <- unique(indicator()$dataset)
    updateSelectizeInput(session, "dataset", 
                         choices = choices_dataset,
                         selected = input$dataset) 
  })
  
  province <- reactive({
    dplyr::filter(pakeduc_province, province %in% input$province)
  })
  
  observeEvent(province(), {
    provinces <- unique(province()$province)
    choices_district <- unique(pakeduc_district$dist_nm[pakeduc_district$province %in% provinces])
    updateSelectizeInput(session, "district_name",
                         choices = choices_district,
                         selected = input$district_name)
  })
  
  district <- reactive({
    dplyr::filter(indicator(), dist_nm %in% input$district_name)
  })
  
  observeEvent(district(), {
    choices <- unique(indicator()$age_range)
    updateSelectInput(session, "age_range", choices = choices) 
  })
  
  # output$data <- renderTable({
  #   district() 
  # })
  
  return(
    list(
      indicator     = reactive({ input$indicator }),
      district_name = reactive({ input$district_name }),
      age_range     = reactive({ input$age_range }),
      gender        = reactive({ input$gender }),
      dataset       = reactive({ input$dataset }),
      province      = reactive({ input$province })
    )
  )
}
    
## To be copied in the UI
# mod_main_ui_ui("main_ui_ui_1")
    
## To be copied in the server
# callModule(mod_main_ui_server, "main_ui_ui_1")
 
