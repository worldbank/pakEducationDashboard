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
    # Dynamically chooses year based on inputs
    uiOutput(ns("tempYear")),
    tags$h4("Data sources"),
    tags$ul(
      tags$li(tags$a("ASER", href = "http://aserpakistan.org/index.php"),
              ": The Annual Status of Education Report is a citizen-led; household-based
              survey that aims to provide reliable estimates on the schooling status of
              children aged 3-16 years residing in all rural and several urban districts
              of Pakistan. "),
      tags$li(tags$a("HIES", href = "http://www.pbs.gov.pk/content/household-integrated-economic-survey-hies-2015-16"), 
              ": The Household Integrated Economic Survey implemented by the Pakistan Bureau of Statistics provides information
              on household income, savings, liabilities, and consumption expenditure, and social indicators at national 
              and provincial levels. "),
      tags$li(tags$a("PSLM", href ="http://www.pbs.gov.pk/content/pakistan-social-and-living-standards-measurement"),
              ": The Pakistan Social and Living Standards Measurement (PSLM) is a household survey that provides social
              and economic indicators, which is representative at provincial and district levels. It includes survey 
              modules on education, health, employment, household assets and amenities, income and expenditure, population
              welfare, water supply and sanitation."),
      tags$li(tags$a("MICS", href ="http://www.bos.gop.pk/mics"),
              ": The Multiple Indicator Cluster Survey is a multipurpose household survey implemented on a provincial level
              basis, and is representative at district level. The MICS provides data to track poverty, education and health
              indicators."),
      tags$li(tags$a("DHS",  href ="https://dhsprogram.com/what-we-do/survey/survey-display-523.cfm"),
              ": The Pakistan Demographic and Health Survey is a household survey implemented by the National Institute of
              Population Studies. DHS provides internationally standardized indicators of demographic and health indicators.
              The results from this survey are representative at the provincial and district levels."),
      tags$li(tags$a("EGRA", href ="https://www.usaid.gov/news-information/videos/early-grade-reading-assessment-egra-extra-mile-better-journey"),
              ": EGRA is an individually administered assessment tool that measures the foundational literacy skills that readers need before they
              can read, such as letter recognition, and oral reading fluency, as well as basic reading comprehension. The Pakistan EGRA survey was
              administered as part of a USAID funded reading project, providing baseline, midline and end-line data.")
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
  output$tempYear <- renderUI({})
  outputOptions(output, "tempYear", suspendWhenHidden = FALSE)
  
  province <- reactive({
    dplyr::filter(pakeduc_province, province %in% input$province)
  })
  
  # Only display years based on inputs for either weighted or non-weighted
  years <- reactive({
    if(is.null(input$dataset)){
      pakeduc_district_weighted[which(pakeduc_district_weighted$province  %in% input$province & 
                                        pakeduc_district_weighted$indicator == input$indicator &
                                        !is.na(pakeduc_district_weighted$point_estimate) &
                                        pakeduc_district_weighted$dist_nm %in% input$district), "year"]
    } 
    else{
      pakeduc_district[which(pakeduc_district$province %in% input$province & 
                             pakeduc_district$indicator == input$indicator &
                             !is.na(pakeduc_district$point_estimate) &
                             pakeduc_district$dist_nm %in% input$district), "year"]
    }
  })
  
  output$tempYear <- renderUI({
    sliderInput(inputId = ns("year"),
                label = "Select a year",
                min = min(years(), na.rm = TRUE),
                max = max(years(), na.rm = TRUE),
                value = max(years(), na.rm = TRUE),
                sep = "")
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
 
