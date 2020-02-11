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
    plotly::plotlyOutput(outputId = ns("country_plot"), height = "600px"),
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
  
  
  df <- reactive({
    
    gender_selection <- if(selection_vars$gender()) {
      c("Boy", "Girl")
    } else {
      "Both"
    }
    
    dplyr::filter(pakeduc_country_weighted,
                  indicator %in% !!selection_vars$indicator(),
                  !is.na(point_estimate),
                  gender %in% !!gender_selection) %>%
      dplyr::mutate(
        pe_percent = sprintf("%.1f%%", point_estimate * 100)
      )
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
    
    dplyr::filter(pakeduc_country,
                  indicator %in% !!selection_vars$indicator(),
                  gender %in% !!gender_selection, 
                  dataset %in% !!selection_vars$dataset(),
                  !is.na(point_estimate)) %>%
      dplyr::mutate(
        pe_percent = sprintf("%.1f%%", point_estimate * 100)
      )
  })
  
  
  
  output$country_plot <- plotly::renderPlotly({
    if (nrow(df()) > 0) {
      
      p <- ggplot2::ggplot(df(), ggplot2::aes(x = year, 
                                              y = point_estimate, 
                                              color = gender,
                                              text = paste("Value (Share of population %):", pe_percent,
                                                           "<br />Year:", year,
                                                           "<br />Dataset:", dataset))) +
        ggplot2::geom_line(ggplot2::aes(group = gender),
                           size = ggplot2::rel(0.8)) +
        ggplot2::geom_point(size = ggplot2::rel(2.8)) +
        ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
        ggthemes::scale_color_colorblind() +
        cowplot::theme_cowplot(14) +
        # ggplot2::facet_wrap(~indicator, ncol = 2) +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          legend.position = "none"
        ) +
        ggplot2::labs(
          x = "",
          y = ""
        )
    }
    
    if (nrow(surveydf()) > 0) {
      # When survey/dataset selected remove Weighted Mix
      p <- ggplot2::ggplot(surveydf(), ggplot2::aes(x = year, 
                                              y = point_estimate, 
                                              color = gender,
                                              text = paste("Value (Share of population %):", pe_percent,
                                                           "<br />Year:", year,
                                                           "<br />Dataset:", dataset))) +
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
        # ggplot2::facet_wrap(~indicator, ncol = 2) +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          legend.position = "none"
        ) +
        ggplot2::labs(
          x = "",
          y = "Share of population (%)\n "
        )
    }
    
    #Only return plot if filtered dataframe has rows
    if(nrow(df()) > 0 || nrow(surveydf()) > 0){
      plotly::ggplotly(p, tooltip = c("text")) %>% plotly::style(hoveron = "color")
    }
  })
}
    
## To be copied in the UI
# mod_country_visuals_ui("country_visuals_ui_1")
    
## To be copied in the server
# callModule(mod_country_visuals_server, "country_visuals_ui_1")
 
