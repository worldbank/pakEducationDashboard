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
  
  # Reorder indicator_choices_province based on Koen input
  important_inds              <-  c("reading_9_11", "share_private_6_10")
  indicator_choices_district  <-  indicator_choices_district[order(match(indicator_choices_district, important_inds))]
  
  tagList(
    selectInput(inputId = ns("indicator"),
                label = "Choose an indicator",
                choices = indicator_choices_district,
                selectize = TRUE,
                selected = "reading_9_11"),
    selectInput(inputId = ns("province"),
                label = "Choose one or more province(s)",
                choices = pakeduc_province_selection,
                multiple = TRUE,
                selectize = TRUE,
                selected = pakeduc_province_selection
    ),
    selectInput(inputId = ns("district"),
                label = "Choose one or more district(s)",
                choices = sort(unique(pakeduc_district[["district"]])),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Attock", "Lahore", "Chakwal", "Rajanpur")),
    
    # Trigger gender disaggregation
    radioButtons(inputId = ns("dimension"),
                 label = "Disaggregate by:",
                 choices = c("Gender" = "gender",
                             "Urban - Rural" = "urban-rural",
                             # "Wealth quintile" = "wealth quintile",
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
    
#' @rdname mod_district_select
#' @export
#' @keywords internal
    
mod_district_select_server <- function(input, output, session){
  ns <- session$ns
  
  # Used this solution to unhide uiOutput()
  # https://stackoverflow.com/questions/36613018/r-shiny-uioutput-not-rendering-inside-menuitem
  # output$tmp_year <- renderUI({})
  # outputOptions(output, "tmp_year",    suspendWhenHidden = FALSE)
  
  output$tmp_dataset <- renderUI({})
  outputOptions(output, "tmp_dataset", suspendWhenHidden = FALSE)
  
  province <- reactive({
    dplyr::filter(pakeduc_province, province %in% input$province)
  })
  
  # Only display datasets based on inputs for non-weighted
  datasets <- reactive({
    dim <- ifelse(!is.null(input$dimension), input$dimension, "aggregate")
    
    d <- pakeduc_district[which(pakeduc_district$province %in% input$province &
                                  pakeduc_district$indicator == input$indicator &
                                  !is.na(pakeduc_district$point_estimate) &
                                  pakeduc_district$dimensions %in% dim), "dataset"]
    
    if (nrow(d) > 0) {return(d[["dataset"]])} else {return("")}
  })
  
  output$tmp_dataset<-  renderUI({
    selectInput(inputId = ns("dataset"),
                label = "Choose one or more survey(s)",
                choices = sort(unlist(unique(datasets()))),
                selectize = TRUE,
                multiple = TRUE,
                selected = c("pslm", "aser", "hies", "egra", "dhs"))
  })
  
  observeEvent(province(), {
    provinces <- unique(province()$province)
    choices_district <- unique(pakeduc_district$district[pakeduc_district$province %in% provinces])
    updateSelectizeInput(session, "district",
                         choices = choices_district,
                         selected = input$district)
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
      dimension     = reactive({ input$dimension }),
      dataset       = reactive({ input$dataset }),
      province      = reactive({ input$province }),
      district      = reactive({ input$district }),
      weighted_mix  = reactive({ input$weighted_mix})#,
      #year          = reactive({ input$year })
    )
  )
}
    
## To be copied in the UI
# mod_district_select_ui("district_select_ui_1")
    
## To be copied in the server
# callModule(mod_district_select_server, "district_select_ui_1")
 
