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
  
  df <- reactive({
    
    gender_selection <- if(selection_vars$gender()) {
      c("Boy", "Girl")
    } else {
      "Both"
    }
    
    dplyr::filter(pakeduc_country,
                  indicator %in% !!selection_vars$indicator(),
                  dataset %in% !!selection_vars$dataset(),
                  !is.na(point_estimate),
                  gender %in% !!gender_selection) 
  })
  
  output$warning_message <- renderText({
    if (nrow(df()) == 0) {"No data available. Please make a new selection"}
  })
  
  output$country_plot <- plotly::renderPlotly({
    if (nrow(df()) > 0) {
      
      p <- ggplot2::ggplot(df(), ggplot2::aes(x = year, y = point_estimate, color = gender)) +
        ggplot2::geom_line(ggplot2::aes(group = interaction(dataset, gender),
                                        linetype = dataset),
                           size = ggplot2::rel(0.8)) +
        ggplot2::geom_point(size = ggplot2::rel(2.8),
                            ggplot2::aes(shape = dataset)) +
        ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
        ggthemes::scale_color_colorblind() +
        cowplot::theme_cowplot(14) +
        ggplot2::facet_wrap(~indicator, ncol = 2, 
                            labeller = ggplot2::labeller(indicator = indicator_choices_country_inv)) +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          legend.position = "none"
        ) +
        ggplot2::labs(
          x = "",
          y = "Share of population (%)\n "
        )
      plotly::ggplotly(p)
    }
    #p
  })
}
    
## To be copied in the UI
# mod_country_visuals_ui("country_visuals_ui_1")
    
## To be copied in the server
# callModule(mod_country_visuals_server, "country_visuals_ui_1")
 
