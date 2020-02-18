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
                  gender %in% !!gender_selection) %>%
      dplyr::mutate(
        pe_percent = sprintf("%.1f%%", point_estimate * 100)
      )
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
                  !is.na(point_estimate)) %>%
      dplyr::mutate(
        pe_percent = sprintf("%.1f%%", point_estimate * 100)
      )
  })
  
  output$province_plot <- plotly::renderPlotly({
    if (nrow(df()) > 0) {
      p <- ggplot2::ggplot(df(), ggplot2::aes(x = year, 
                                              y = point_estimate, 
                                              color = gender,
                                              text = paste("Value (Share of population %):", pe_percent,
                                                           "<br />Year:", year,
                                                           "<br />Dataset:", dataset,
                                                           "<br />Gender:", gender))) +
        ggplot2::geom_line(ggplot2::aes(group = gender),
                           size = ggplot2::rel(0.8)) +
        ggplot2::geom_point(size = ggplot2::rel(2.8)) +
        ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
        ggthemes::scale_color_colorblind() +
        cowplot::theme_cowplot(14) +
        ggplot2::facet_wrap(~province, ncol = 5) +
        # ggplot2::theme(
        #   legend.title = ggplot2::element_blank(),
        #   legend.position = "none"
        # ) +
        ggplot2::labs(
          x = "",
          y = ""
        ) +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          legend.position = "none"
          )
      
    }
    
    if (nrow(surveydf()) > 0) {
      # When survey/dataset selected remove Weighted Mix
      p <- ggplot2::ggplot(surveydf(), ggplot2::aes(x = year, 
                                                    y = point_estimate, 
                                                    color = gender,
                                                    text = paste("Value (Share of population %):", pe_percent,
                                                                 "<br />Year:", year,
                                                                 "<br />Dataset:", dataset,
                                                                 "<br />Gender:", gender))) +
        ggplot2::geom_line(ggplot2::aes(group = interaction(dataset, gender), 
                                        linetype = dataset),
                           size = ggplot2::rel(0.6),
                           alpha = .6 ) +
        ggplot2::geom_point(data = surveydf(),
                            ggplot2::aes(shape = dataset),
                            size = ggplot2::rel(2.2), alpha = .6) +
        ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
        ggthemes::scale_color_colorblind() +
        cowplot::theme_cowplot(14) +  
        ggplot2::facet_wrap(~province, ncol = 5, 
                            labeller = ggplot2::labeller(indicator = indicator_choices_country_inv)) +
        ggplot2::labs(
          x = "",
          y = ""
        ) +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          legend.position = "none"
        )
    }
    
    #Only return plot if filtered dataframe has rows
    if(nrow(df()) > 0 || nrow(surveydf()) > 0){
      
      plotly::ggplotly(p, tooltip = c("text")) %>% 
        plotly::style(hoveron = "color") 
    }
  })
}
    
## To be copied in the UI
# mod_province_visuals_ui("province_visuals_ui_1")
    
## To be copied in the server
# callModule(mod_province_visuals_server, "province_visuals_ui_1")
 
