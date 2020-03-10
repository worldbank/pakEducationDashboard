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
  
  # Reorder indicator_choices_province based on Koen input
  important_inds              <-  c("reading_9_11", "share_private_6_10")
  indicator_choices_province  <-  indicator_choices_province[order(match(indicator_choices_province, important_inds))]
  
  
  tagList(
    selectInput(inputId = ns("indicator"),
                label = "Choose an indicator",
                choices = indicator_choices_province,
                selectize = TRUE,
                selected = "reading_9_11"),
    selectInput(inputId = ns("province"),
                label = "Choose one or more province(s)",
                choices = sort(unique(pakeduc_province[["province"]])),
                multiple = TRUE,
                selectize = TRUE,
                selected = sort(unique(pakeduc_province[["province"]]))),
    checkboxInput(inputId = ns("gender"),
                  label = "Disaggregate by gender",
                  value = FALSE),
    # Dynamically chooses dataset based on inputs
    uiOutput(ns("tmp_dataset")),
    # Dynamically chooses year based on inputs
    #uiOutput(ns("tmp_year")),
    tags$h4("Data sources"),
    tags$ul(
      tags$li(tags$a("ASER", href = "http://aserpakistan.org/index.php"),
              ": The Annual Status of Education Report is a citizen-led; household-based survey, led by ITA. "),
      tags$li(tags$a("DHS",  href ="https://dhsprogram.com/what-we-do/survey/survey-display-523.cfm"),
              ": The Pakistan Demographic and Health Survey is a household survey implemented by the National Institute of Population Studies."),
      tags$li(tags$a("EGRA", href ="https://www.usaid.gov/news-information/videos/early-grade-reading-assessment-egra-extra-mile-better-journey"),
              ": EGRA is an individually administered assessment tool that measures foundational literacy skills, which has been administered as part of USAID’s Pakistan Reading Project."),
      tags$li(tags$a("HIES", href = "http://www.pbs.gov.pk/content/household-integrated-economic-survey-hies-2015-16"), 
              ": The Household Integrated Economic Survey is a household survey, representative at the provincial level, and led by the Pakistan Bureau of Statistics."),
      tags$li(tags$a("MICS", href ="http://www.bos.gop.pk/mics"),
              ": The Multiple Indicator Cluster Survey is a multipurpose household survey implemented on a provincial level basis, and is representative at district level."),
      tags$li(tags$a("PSLM", href ="http://www.pbs.gov.pk/content/pakistan-social-and-living-standards-measurement"),
              ": The Pakistan Social and Living Standards Measurement (PSLM) is another household survey, representative at the district level, and led by the Pakistan Bureau of Statistics.")
    )
    
  )
}
    
# Module Server
    
#' @rdname mod_province_select
#' @export
#' @keywords internal
    
mod_province_select_server <- function(input, output, session){
  ns <- session$ns
  
  # Used this solution to unhide uiOutput()
  # https://stackoverflow.com/questions/36613018/r-shiny-uioutput-not-rendering-inside-menuitem
  output$tmp_year <- renderUI({})
  outputOptions(output, "tmp_year",    suspendWhenHidden = FALSE)
  
  output$tmp_dataset <- renderUI({})
  outputOptions(output, "tmp_dataset", suspendWhenHidden = FALSE)
  
  province <- reactive({
    dplyr::filter(pakeduc_province, province %in% input$province)
  })
  
  # Only display datasets based on inputs for non-weighted
  datasets <- reactive({
   
    g <- ifelse(input$gender, c("Boy","Girl"), "Both")
    
    d <- pakeduc_province[which(pakeduc_province$province %in% input$province &
                                  pakeduc_province$indicator == input$indicator &
                                  !is.na(pakeduc_province$point_estimate) &
                                pakeduc_province$gender %in% g), "dataset"]

    if (nrow(d) > 0) {return(d[["dataset"]])} else {return("")}
  })
  
  output$tmp_dataset<-  renderUI({
    selectInput(inputId = ns("dataset"),
                label = "Choose one or more survey(s)",
                choices = sort(unique(datasets())),
                selectize = TRUE,
                multiple = TRUE)
  })
  
  # Only display years based on inputs for either weighted or non-weighted
  years <- reactive({
    # Assuming that years only effect map
    pakeduc_province_weighted[which(pakeduc_province_weighted$province %in% input$province & 
                               pakeduc_province_weighted$indicator == input$indicator &
                               !is.na(pakeduc_province_weighted$point_estimate)), "year"]
  })
  
  output$tmp_year <- 
    renderUI({
      
      shinyWidgets::sliderTextInput(inputId  = ns("year"), 
                                    label    = "Select a year",
                                    choices  = sort(unlist(unique(years()))),
                                    selected = max(years(), na.rm = TRUE),
                                    to_min   = min(years(), na.rm = TRUE),
                                    to_max   = max(years(), na.rm = TRUE) 
                                                )
  })
  
  return(
    list(
      indicator     = reactive({ input$indicator }),
      gender        = reactive({ input$gender }),
      dataset       = reactive({ input$dataset }),
      province      = reactive({ input$province }),
      year          = reactive({ input$year })
    )
  )
}
    
## To be copied in the UI
# mod_province_select_ui("province_select_ui_1")
    
## To be copied in the server
# callModule(mod_province_select_server, "province_select_ui_1")
 
