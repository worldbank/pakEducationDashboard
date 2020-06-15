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
    shinycssloaders::withSpinner(
      ggiraph::ggiraphOutput(outputId = ns("district_plot"),
                             width = "100%", 
                             height = "200px"),
      type = 3, 
      color = "#009DA7",
      color.background = "#FFFFFF"),
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
  
  # Non-weighted data
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
                  !is.na(point_estimate)) 
  })
  
  # Weighted data
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
                  gender %in% !!gender_selection) 
  })
  
  
  output$district_ind_description <- renderText({
    unique(df()$indicator_definition)
  })
  
  output$warning_message <- renderText({
    if (nrow(surveydf()) == 0) {"No data available. Please make a new selection or contact Koen M. Geven at kgeven@worldbank.org"}
  })
  
  
 
  output$district_plot <- ggiraph::renderggiraph({
    # Adjust scale according to indicator
    # if (stringr::str_detect(selection_vars$indicator(), "^egra")) {
    #   y_scale <- ggplot2::scale_y_continuous(limits = c(0, 100))
    # } else {
    #   y_scale <- ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent)
    # }
   
    
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
    
    facet_rows <- ((length(unique(surveydf()$dist_nm)) - 1) %/% 4) + 1    
    p <- p +
      ggplot2::facet_wrap(~dist_nm, nrow = facet_rows) +
        ggplot2::theme(
          strip.text       = ggplot2::element_text(color = "#009DA7", family = "Arial",
                                                   face = "bold", size = 30),
          strip.background = ggplot2::element_rect(color = "white", fill = "white")
        )
    
    #Only return plot if filtered dataframe has rows
    if(nrow(surveydf()) > 0){
      ggiraph::girafe(ggobj = p,
                      width_svg = 22,
                      height_svg = 8,
                      options = list(ggiraph::opts_tooltip(use_fill = TRUE))) 
    }
  })
}
