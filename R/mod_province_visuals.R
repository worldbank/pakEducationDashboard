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
    plotly::plotlyOutput(outputId = ns("province_plot")),
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
  
  # title <- reactive({
  #   names(indicator_choices_province)[indicator_choices_province == selection_vars$indicator()]
  # })
  
  output$province_title <- renderText({
    names(indicator_choices_province)[indicator_choices_province == selection_vars$indicator()]
  })
  
  df <- reactive({
    
    gender_selection <- if(selection_vars$gender()) {
      c("Boy", "Girl")
    } else {
      "Both"
    }
    
    # TODO:HERE
    ## Disable "Disaggregate by gender" option if for that combintation of indicator,
    ## select, year there is no "Boy" or "Girl"
    
    
    dplyr::filter(pakeduc_province_weighted,
                  indicator == !!selection_vars$indicator(),
                  province %in% !!selection_vars$province(),
                  !is.na(point_estimate),
                  gender %in% !!gender_selection) 
  })
  
  
  output$province_ind_description <- renderText({
    unique(df()$indicator_definition)
  })
  
  output$warning_message <- renderText({
    # TODO: GET CONTACT EMAIL FROM KOEN
    if (nrow(df()) == 0) {"No data available. Please make a new selection. Contact us at EMAIL_GOES_HERE"}
  })
  
  surveydf <- reactive({
    gender_selection <- if(selection_vars$gender()) {
      c("Boy", "Girl")
    } else {
      "Both"
    }
    
    dplyr::filter(pakeduc_province,
                  indicator %in% !!selection_vars$indicator(),
                  province %in% !!selection_vars$province(),
                  gender %in% !!gender_selection, 
                  dataset %in% !!selection_vars$dataset(),
                  !is.na(point_estimate)) 
  })
  
  output$province_plot <- plotly::renderPlotly({
    # Adjust scale and tooltip according to indicator
    if (stringr::str_detect(selection_vars$indicator(), "^egra")) {
      y_scale <- ggplot2::scale_y_continuous(limits = c(0, 100))
    } else {
      y_scale <- ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent)
    }
    
    if (nrow(df()) > 0) {
      p <- plot_lines_weighted(data = df(),
                               x = year,
                               y = point_estimate,
                               color = gender,
                               dataset = dataset,
                               gender = gender,
                               year = year,
                               tooltip_value = pe_percent) +
        ggplot2::facet_wrap(~province, ncol = 5)
      
      
    }
    
    if (nrow(surveydf()) > 0) {
      # When survey/dataset selected remove Weighted Mix
      p <- plot_lines(data = surveydf(),
                      x = year,
                      y = point_estimate,
                      color = gender,
                      dataset = dataset,
                      gender = gender,
                      year = year,
                      tooltip_value = pe_percent) +
        ggplot2::facet_wrap(~province, ncol = 5, 
                            labeller = ggplot2::labeller(indicator = indicator_choices_country_inv))
      
     }
    
    #Only return plot if filtered dataframe has rows
    if(nrow(df()) > 0 || nrow(surveydf()) > 0){
      
      plotly::ggplotly(p, tooltip = c("text")) %>% 
        plotly::style(hoveron = "color") 
    }
  })
}
 
