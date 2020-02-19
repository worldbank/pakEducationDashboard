# Module UI
  
#' @title   mod_district_visuals_ui and mod_district_visuals_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_district_visuals
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_district_visuals_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(textOutput(ns("district_title"))),
    p(textOutput(ns("district_ind_description"))),
    plotly::plotlyOutput(outputId = ns("district_plot")),
    textOutput(ns("warning_message"))
  
  )
}
    
# Module Server
    
#' @rdname mod_district_visuals
#' @export
#' @keywords internal
    
mod_district_visuals_server <- function(input, 
                                        output, 
                                        session,
                                        selection_vars) {
  ns <- session$ns
  
  output$district_title <- renderText({
    names(indicator_choices_district)[indicator_choices_district == selection_vars$indicator()]
  })
  
  df <- reactive({
    
    gender_selection <- if(selection_vars$gender()) {
      c("Boy", "Girl")
    } else {
      "Both"
    }
  
    dplyr::filter(pakeduc_district_weighted,
                  indicator == !!selection_vars$indicator(),
                  province %in% !!selection_vars$province(),
                  dist_nm %in% !!selection_vars$district(),
                  !is.na(point_estimate),
                  gender %in% !!gender_selection) %>%
      dplyr::mutate(
        pe_percent = sprintf("%.1f%%", point_estimate * 100)
      )
  })
  
  
  output$district_ind_description <- renderText({
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
    
    dplyr::filter(pakeduc_district,
                  indicator %in% !!selection_vars$indicator(),
                  gender %in% !!gender_selection, 
                  dist_nm %in% !!selection_vars$district(),
                  dataset %in% !!selection_vars$dataset(),
                  !is.na(point_estimate)) %>%
      dplyr::mutate(
        pe_percent = sprintf("%.1f%%", point_estimate * 100)
      )
  })
 
  output$district_plot <- plotly::renderPlotly({
    # Adjust scale according to indicator
    if (stringr::str_detect(selection_vars$indicator(), "^egra")) {
      y_scale <- ggplot2::scale_y_continuous(limits = c(0, 100))
    } else {
      y_scale <- ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent)
    }
    
    
    if (nrow(df()) > 0) {
      facet_rows <- ((length(unique(df()$dist_nm)) - 1) %/% 4) + 1
      
      p <- plot_lines_weighted(data = df(),
                               x = year,
                               y = point_estimate,
                               color = gender,
                               dataset = dataset,
                               gender = gender,
                               year = year,
                               tooltip_value = pe_percent) +
        ggplot2::facet_wrap(~dist_nm, nrow = facet_rows)
        
    }
    
    if (nrow(surveydf()) > 0) {
      # When survey/dataset selected remove Weighted Mix
      facet_rows <- ((length(unique(surveydf()$dist_nm)) - 1) %/% 4) + 1
      
      p <- plot_lines(data = surveydf(),
                      x = year,
                      y = point_estimate,
                      color = gender,
                      dataset = dataset,
                      gender = gender,
                      year = year,
                      tooltip_value = pe_percent) +
        ggplot2::facet_wrap(~dist_nm, nrow = facet_rows, 
                            labeller = ggplot2::labeller(indicator = indicator_choices_country_inv))
      
    }
    
    #Only return plot if filtered dataframe has rows
    if(nrow(df()) > 0 || nrow(surveydf()) > 0){
      plotly::ggplotly(p, tooltip = c("text")) %>% 
        plotly::style(hoveron = "color") 
    }
  })
}
