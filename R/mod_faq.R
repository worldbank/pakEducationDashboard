# Module UI
  
#' @title   mod_faq_ui and mod_faq_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_faq
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_faq_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$title("Frequently Asked Questions:"),
    tags$h3("Why create a data portal?"),
    tags$p("The Pakistan Education Portal aims to provide a platform for 
            proactive and open accessto standardized micro-data relevant
            to education in Pakistan. Currently, information on education 
            is scattered across multiple datasets in Pakistan. This portal
            harmonizes the education relevant data for users through an
            easy-to-use online interface. "),
    tags$p("Our hope is that it will allow policymakers and researchers 
           to understand annual schooling trends and make informed policy
           decisions by visualizing data that are tailored to their 
           interest—be it in comparisons of indicators over time, across
           districts, provinces, or by gender within the country."),
    tags$h3("How did you calculate these indicators?"),
    tags$ul(
            tags$li("After  data cleaning and aggregation of the data on key
            indicators from different sources, we estimated means 
            and standard errors using survey weights to correct for
            the sampling design. We carefully followed the documentation
            of each individual survey. "),
            
            # TODO: ADD GITHUB CODE TO WBG AND MAKE PUBLIC
            # tags$li("In order to be as transparent as possible, we are
            #          publishing all the source code and documentation on
            #          Github. (Note: A hyper link will be added later)"),
            
            tags$li("If you see an error in any of the indicators, 
                    please let us know, as we strive to provide the 
                    best possible data available.")
            ),
    tags$h3("Who created this data portal?"),
    
    #TODO: ASK TONY ABOUT ADDING US TO "WHO CREATED THE PORTAL"
    tags$p("This portal was created by a team led by Koen Geven at
           the World Bank. Ahmed Raza led the cleaning of the data.
           Isabel Harbaugh Macdonald contributed to the narrative interpretation.
           Ayesha Tahir contributed to the publication of the dataset.
           Jishnu Das, Tahir Andrabi, Deon Filmer and others provided valuable
           advice in the design of the portal."),
    tags$h3("What are the sources for these indicators?"),
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
    
#' @rdname mod_faq
#' @export
#' @keywords internal
    
mod_faq_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_faq_ui("faq_ui_1")
    
## To be copied in the server
# callModule(mod_faq_server, "faq_ui_1")
 
