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
    if (nrow(df()) > 0) {
      p <- ggplot2::ggplot(df(), ggplot2::aes(x = year, 
                                              y = point_estimate, 
                                              color = gender,
                                              text = paste("Value:", pe_percent,
                                                           "<br />Year:", year,
                                                           "<br />Dataset:", dataset))) +
        ggplot2::geom_line(ggplot2::aes(group = gender),
                           size = ggplot2::rel(0.8)) +
        ggplot2::geom_point(size = ggplot2::rel(2.8)) +
        ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
        ggthemes::scale_color_colorblind() +
        ggplot2::facet_wrap(~dist_nm, ncol = 2) +
        cowplot::theme_cowplot(14)  +
        ggplot2::theme(
          legend.position = "none"
        ) +
        ggplot2::labs(
          x = "",
          y = "Share of population (%)"
        )
    }
    
    if (nrow(surveydf()) > 0) {
      # When survey/dataset selected remove Weighted Mix
      p <- ggplot2::ggplot(surveydf(), ggplot2::aes(x = year, 
                                                    y = point_estimate, 
                                                    color = gender,
                                                    text = paste("Value:", pe_percent,
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
        ggplot2::facet_wrap(~dist_nm, ncol = 2, 
                            labeller = ggplot2::labeller(indicator = indicator_choices_country_inv)) +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          legend.position = "none"
        )
    }
    
    plotly::ggplotly(p, tooltip = c("text")) %>% plotly::style(hoveron = "color")
  })
}
    
## To be copied in the UI
# mod_district_visuals_ui("district_visuals_ui_1")
    
## To be copied in the server
# callModule(mod_district_visuals_server, "district_visuals_ui_1")
 
