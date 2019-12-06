#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyUI(
      navbarPage("Pakistan Education Dashboard",
                 theme = shinythemes::shinytheme("flatly"),
                 tabPanel("Country level",
                          sidebarPanel(mod_country_select_ui("country_select_ui_1")),
                          mainPanel(
                            mod_country_visuals_ui("country_visuals_ui_1")
                          )
                 ),
                 tabPanel("Province level",
                          sidebarPanel(mod_province_select_ui("province_select_ui_1")),
                          mainPanel(
                            mod_province_visuals_ui("province_visuals_ui_1")
                          )
                 ),
                 tabPanel("District level",
                          sidebarPanel(mod_district_select_ui("district_select_ui_1")),
                          mainPanel(
                            mod_district_visuals_ui("district_visuals_ui_1")
                          )
                 )
      )
      
    )
  )
}   
    
    
    
    
    # fluidPage(
    #   theme = shinythemes::shinytheme("flatly"),
    #   titlePanel("Pakistan Education Dashboard"),
    #   sidebarLayout(
    #       tabsetPanel(type = "tabs",
    #                   tabPanel("province", mod_province_select_ui("province_select_ui_1"))
    #     )
    #   ),
    #   mainPanel()
    # )
    # List the first level UI elements here 
    # fluidPage(
    #   theme = shinythemes::shinytheme("flatly"),
    #   titlePanel("Pakistan Education Dashboard"),
    #   
    #   fluidRow(
    #     column(
    #       width = 3,
    #       wellPanel(
    #         mod_main_ui_ui("main_ui_ui_1")
    #       )
    #     ),
    #     column(
    #       width = 9,
    #       fluidRow(
    #         #wellPanel(
    #           mod_line_charts_province_select("line_charts_province_select_1"),
    #         #),
    #         #wellPanel(
    #           mod_line_charts_ui("line_charts_ui_1")
    #         #)
    #       )
    #       
    #     )
    #   )
    # )

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
