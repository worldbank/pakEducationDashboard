#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      theme = shinythemes::shinytheme("flatly"),
      titlePanel("Pakistan Education Dashboard"),
      
      fluidRow(
        column(
          width = 3,
          wellPanel(
            mod_main_ui_ui("main_ui_ui_1")
          )
        ),
        column(
          width = 9,
          fluidRow(
            #wellPanel(
              mod_line_charts_province_ui("line_charts_province_ui_1"),
            #),
            #wellPanel(
              mod_line_charts_ui("line_charts_ui_1")
            #)
          )
          
        )
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'pakEducationDashboard')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
