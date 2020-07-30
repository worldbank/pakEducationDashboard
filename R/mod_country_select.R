# Module UI
  
#' @title   mod_country_select_ui and mod_country_select_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_country_select
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_country_select_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Indicator selection
    selectInput(inputId = ns("indicator"),
                label = "Choose an indicator",
                choices = indicator_choices_country,
                selectize = TRUE,
                selected = c("reading_9_11")),
    # Trigger disaggregation
    radioButtons(inputId = ns("dimension"),
                 label = "Disaggregate by:",
                 choices = c("Gender" = "gender",
                             "Urban - Rural" = "urban-rural",
                             "Wealth quintile" = "wealth quintile",
                             "No disaggregation" = "aggregate"),
                 selected = "aggregate"),
    
    # Trigger display of weighted average trend line
    checkboxInput(inputId = ns("weighted_mix"),
                  label = "Show weighted average trend line",
                  value = FALSE),

    # Dynamically chooses dataset based on inputs
    uiOutput(ns("tmp_dataset")),
    
    # Hide and Show button for data sources description
    actionButton(ns("hideshow"), "Survey Descriptions"),
    shinyjs::hidden(
      uiOutput(ns("data_src_description"))
    )
  )
}
    
# Module Server
    
#' @rdname mod_country_select
#' @export
#' @keywords internal
    
mod_country_select_server <- function(input, output, session){
  ns <- session$ns
  
  # Only display data sets based on inputs for non-weighted
  datasets <- reactive({
    
    dim <- ifelse(!is.null(input$dimension), input$dimension, "aggregate")
    
    d <- pakeduc_country[which(pakeduc_country$indicator == input$indicator &
                                 !is.na(pakeduc_country$point_estimate) &
                                 pakeduc_country$dimensions %in% dim), "dataset"]
    
    if (nrow(d) > 0) {return(d[["dataset"]])} else {return("")}
  })
  
  output$tmp_dataset<-  renderUI({
    
    selectInput(inputId = ns("dataset"),
                label = "Choose one or more survey(s)",
                choices = sort(unique(datasets())),
                selectize = TRUE,
                multiple = TRUE,
                selected = c("pslm", "aser", "hies", "egra", "dhs"))
  })
  
  output$data_src_description <- renderUI({data_sources_description})
  # if clicked show survey descriptions
  observeEvent(
    input$hideshow,
    {
      shinyjs::toggle("data_src_description", anim = TRUE)
    }
  )
  
  return(
    list(
      indicator     = reactive({ input$indicator }),
      dimension     = reactive({ input$dimension }),
      dataset       = reactive({ input$dataset }),
      weighted_mix  = reactive({ input$weighted_mix})
      
    )
  )
}
    
## To be copied in the UI
# mod_country_select_ui("country_select_ui_1")
    
## To be copied in the server
# callModule(mod_country_select_server, "country_select_ui_1")
 
