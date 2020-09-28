#' map_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_select_ui <- function(id){
  ns <- NS(id)
  # Reorder indicator_choices_province based on Koen input
  important_inds              <-  c("reading_9_11", "share_private_6_10")
  indicator_choices_district  <-  indicator_choices_district[order(match(indicator_choices_district, important_inds))]
  
  tagList(
    selectInput(inputId = ns("indicator"),
                label = "Choose an indicator",
                choices = indicator_choices_district,
                selectize = TRUE,
                selected = "reading_9_11"),

    # Dynamically chooses dataset based on inputs
    uiOutput(ns("tmp_dataset")),

    # Dynamically chooses year based on inputs
    uiOutput(ns("tmp_year")),
    # Hide and Show button for data sources description
    actionButton(ns("hideshow"), "Survey Descriptions"),
    shinyjs::hidden(
      uiOutput(ns("data_src_description"))
    )
  )
}
    
#' map_select Server Function
#'
#' @noRd 
mod_map_select_server <- function(input, output, session){
  ns <- session$ns
  
  # Used this solution to unhide uiOutput()
  # https://stackoverflow.com/questions/36613018/r-shiny-uioutput-not-rendering-inside-menuitem
  # output$tmp_year <- renderUI({})
  # outputOptions(output, "tmp_year",    suspendWhenHidden = FALSE)
  
  # output$tmp_dataset <- renderUI({})
  # outputOptions(output, "tmp_dataset", suspendWhenHidden = FALSE)
  
  # Only display years based on inputs for either weighted or non-weighted
  years <- reactive({
    pakeduc_district_weighted[which(pakeduc_district_weighted$indicator == input$indicator &
                                                 !is.na(pakeduc_district_weighted$point_estimate)
                                               ), "year"]
  })

  output$tmp_year <- renderUI({
    shinyWidgets::sliderTextInput(inputId  = ns("year"),
                                  label    = "Select a year",
                                  choices  = sort(unlist(unique(years()))),
                                  selected = max(years(), na.rm = TRUE),
                                  to_min   = min(years(), na.rm = TRUE),
                                  to_max   = max(years(), na.rm = TRUE)
    )
  })
  
  # Only display datasets based on inputs for non-weighted
  datasets <- reactive({
    
    # dim <- ifelse(!is.null(input$dimension), input$dimension, "aggregate")
    dim <- "aggregate"
    
    d <- pakeduc_district[which(pakeduc_district$indicator == input$indicator &
                                  !is.na(pakeduc_district$point_estimate) &
                                  pakeduc_district$dimensions %in% dim), "dataset"]
    
    if (nrow(d) > 0) {return(d[["dataset"]])} else {return("")}
  })
  
  output$tmp_dataset <-  renderUI({
    selectInput(inputId = ns("dataset"),
                label = "Choose one survey(s)",
                choices = c("weighted_average", sort(unlist(unique(datasets())))),
                selectize = TRUE,
                multiple = FALSE,
                selected = "weighted_average")
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
      # dimension     = reactive({ input$dimension }),
      dataset       = reactive({ input$dataset }),
      year          = reactive({ input$year })
    )
  )
}
    
## To be copied in the UI
# mod_map_select_ui("map_select_ui_1")
    
## To be copied in the server
# callModule(mod_map_select_server, "map_select_ui_1")
 
