# Module UI
  
#' @title   mod_country_visuals_ui and mod_country_visuals_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_country_visuals
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_country_visuals_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(textOutput(ns("country_title"))),
    p(textOutput(ns("country_ind_description"))),
    shinycssloaders::withSpinner(
      ggiraph::ggiraphOutput(outputId = ns("country_plot"), 
                             width = "100%", 
                             height = "800px"),
      type = 3, 
      color = "#006350",
      color.background = "#FFFFFF"),
    
    textOutput(ns("warning_message"))
  )
}
    
# Module Server
    
#' @rdname mod_country_visuals
#' @export
#' @keywords internal
    
mod_country_visuals_server <- function(input, 
                                       output, 
                                       session,
                                       selection_vars){
  ns <- session$ns

  output$country_title <- renderText({
    names(indicator_choices_country)[indicator_choices_country == selection_vars$indicator()]
  })
  
  # Non-weighted data
  surveydf <- reactive({
    gender_selection <- if(selection_vars$gender()) {
      c("Boy", "Girl")
    } else {
      "Both"
    }
    
    dplyr::filter(pakeduc_country,
                  indicator %in% !!selection_vars$indicator(),
                  gender %in% !!gender_selection, 
                  dataset %in% !!selection_vars$dataset(),
                  !is.na(point_estimate)) 
  })
  
  # Weighted data
  
  df <- reactive({

    gender_selection <- if(selection_vars$gender()) {
      c("Boy", "Girl")
    } else {
      "Both"
    }

    dplyr::filter(pakeduc_country_weighted,
                  indicator %in% !!selection_vars$indicator(),
                  !is.na(point_estimate),
                  gender %in% !!gender_selection)
  })
  
  output$country_ind_description <- renderText({
    unique(surveydf()$indicator_definition)
  })
  
  output$warning_message <- renderText({
    # TODO: GET CONTACT EMAIL FROM KOEN
    if (nrow(surveydf()) == 0) {"No data available. Please make a new selection or contact Koen M. Geven at kgeven@worldbank.org"}
  })
  
 
  output$country_plot <- ggiraph::renderggiraph({
    
    if (nrow(surveydf()) > 0 & selection_vars$weighted_mix()) {
      
      p <- plot_lines(data = surveydf(),
                      wght_data = df(),
                      x = year,
                      y = point_estimate,
                      color = gender,
                      dataset = dataset,
                      gender = gender,
                      year = year,
                      tooltip_value = pe_percent)
    } else {
      p <- plot_lines(data = surveydf(),
                      x = year,
                      y = point_estimate,
                      color = gender,
                      dataset = dataset,
                      gender = gender,
                      year = year,
                      tooltip_value = pe_percent)
    }
    
    #Only return plot if filtered dataframe has rows
    if(nrow(surveydf()) > 0) {
      
      ggiraph::girafe(ggobj = p,
                      pointsize = 16,
                      width_svg = 18,
                      height_svg = 10,
                      options = list(ggiraph::opts_tooltip(use_fill = TRUE)))
    }
  })
}
 
