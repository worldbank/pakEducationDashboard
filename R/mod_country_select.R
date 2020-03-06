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
    selectInput(inputId = ns("dataset"),
                label = "Choose one or more survey(s)",
                choices = sort(unique(pakeduc_province[["dataset"]])),
                selectize = TRUE,
                multiple = TRUE),
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
    
#' @rdname mod_country_select
#' @export
#' @keywords internal
    
mod_country_select_server <- function(input, output, session){
  ns <- session$ns
  
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
 
