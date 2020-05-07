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
    selectInput(inputId = ns("indicator"),
                label = "Choose an indicator",
                choices = indicator_choices_country,
                selectize = TRUE,
                
                #TODO: Revert to Tony
                ## had to comment to account for title not rendering
                # multiple = TRUE,
                # selected = c("reading_9_11", "share_private_6_10")),
                selected = c("reading_9_11")),
    checkboxInput(inputId = ns("gender"),
                  label = "Disaggregate by gender",
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
  
  # Only display datasets based on inputs for non-weighted
  datasets <- reactive({
    
    g <- ifelse(input$gender, c("Boy","Girl"), "Both")
    
    d <- pakeduc_country[which(pakeduc_country$indicator == input$indicator &
                                 !is.na(pakeduc_country$point_estimate) &
                                 pakeduc_country$gender %in% g), "dataset"]
    
    if (nrow(d) > 0) {return(d[["dataset"]])} else {return("")}
  })
  
  output$tmp_dataset<-  renderUI({
    
    selectInput(inputId = ns("dataset"),
                label = "Choose one or more survey(s)",
                choices = sort(unique(datasets())),
                selectize = TRUE,
                multiple = TRUE)
  })
  
  output$data_src_description <- renderUI({data_sources_description})
  # if clciked show survey descriptions
  observeEvent(
    input$hideshow,
    {
      shinyjs::toggle("data_src_description", anim = TRUE)
    }
  )
  
  return(
    list(
      indicator     = reactive({ input$indicator }),
      gender        = reactive({ input$gender }),
      dataset       = reactive({ input$dataset })
    )
  )
}
    
## To be copied in the UI
# mod_country_select_ui("country_select_ui_1")
    
## To be copied in the server
# callModule(mod_country_select_server, "country_select_ui_1")
 
