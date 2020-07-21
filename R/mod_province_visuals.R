# Module UI
  
#' @title   mod_province_visuals_ui and mod_province_visuals_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_province_visuals
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_province_visuals_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(textOutput(ns("province_title"))),
    p(textOutput(ns("province_ind_description"))),
    shinycssloaders::withSpinner(
      ggiraph::ggiraphOutput(outputId = ns("province_plot"), 
                             width = "100%", 
                             height = "400px"),
      type = 3, 
      color = "#009DA7",
      color.background = "#FFFFFF"),
    textOutput(ns("warning_message"))
  )
}
    
# Module Server
    
#' @rdname mod_province_visuals
#' @export
#' @keywords internal
    
mod_province_visuals_server <- function(input, 
                                        output, 
                                        session, 
                                        selection_vars){
  ns <- session$ns
  
  output$province_title <- renderText({
    names(indicator_choices_province)[indicator_choices_province == selection_vars$indicator()]
  })
  
  # Non-weigthed 
  surveydf <- reactive({
    # Handles potential issues due to missing selection inputs
    req(selection_vars$dataset())
    
    dplyr::filter(pakeduc_province,
                  indicator %in% !!selection_vars$indicator(),
                  province %in% !!selection_vars$province(),
                  dimensions %in% !!selection_vars$dimension(), 
                  dataset %in% !!selection_vars$dataset(),
                  !is.na(point_estimate)) 
  })
  
  # weighted data
  df <- reactive({
   
    ## Disable "Disaggregate by gender" option if for that combintation of indicator,
    ## select, year there is no "Boy" or "Girl"
    dplyr::filter(pakeduc_province_weighted,
                  indicator == !!selection_vars$indicator(),
                  province %in% !!selection_vars$province(),
                  !is.na(point_estimate),
                  dimensions %in% !!selection_vars$dimension()) 
  })
  
  
  output$province_ind_description <- renderText({
    unique(df()$indicator_definition)
  })
  
  output$warning_message <- renderText({
    if (nrow(surveydf()) == 0) {"No data available. Please make a new selection or contact Koen M. Geven at kgeven@worldbank.org"}
  })
  
  
  output$province_plot <- ggiraph::renderggiraph({
    # Adjust scale and tooltip according to indicator
    # if (stringr::str_detect(selection_vars$indicator(), "^egra")) {
    #   y_scale <- ggplot2::scale_y_continuous(limits = c(0, 100))
    # } else {
    #   y_scale <- ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent)
    # }
    #Only return plot if filtered dataframe has rows
    if( nrow(surveydf()) > 0) {
      
    if (nrow(surveydf()) > 0 & selection_vars$weighted_mix()) {
      
      p <- plot_lines(data = surveydf(),
                      wght_data = df(),
                      x = year,
                      y = point_estimate,
                      color = dimension_levels,
                      dataset = dataset,
                      group = dimension_levels,
                      year = year,
                      tooltip_value = pe_percent,
                      font_size = 18)
    } else {
      p <- plot_lines(data = surveydf(),
                      x = year,
                      y = point_estimate,
                      color = dimension_levels,
                      dataset = dataset,
                      group = dimension_levels,
                      year = year,
                      tooltip_value = pe_percent,
                      font_size = 18)
    }
    
    p <- p +
      ggplot2::facet_wrap(~province, ncol = 5, 
                          labeller = ggplot2::labeller(indicator = indicator_choices_country_inv)) +
      ggplot2::theme(
        strip.text       = ggplot2::element_text(color = "#009DA7", family = "Arial",
                                                 face = "bold", size = 30),
        strip.background = ggplot2::element_rect(color = "white", fill = "white")
      )
    
      ggiraph::girafe(ggobj = p,
                      pointsize = 16,
                      width_svg = 22,
                      height_svg = 8,
                      options = list(ggiraph::opts_tooltip(use_fill = TRUE)))
    }
  })
}
 
