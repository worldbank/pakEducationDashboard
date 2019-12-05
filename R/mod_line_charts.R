# Module UI
  
#' @title   mod_line_charts_ui and mod_line_charts_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @import  ggplot2
#'
#' @rdname mod_line_charts
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_line_charts_ui <- function(id){
  ns <- NS(id)
  tagList(
        # Output: Histogram ----
        
        h4("By district"),
        # plotOutput(outputId = ns("district_plot")),
        plotly::plotlyOutput(outputId = ns("district_plot"))
      )
}
    
# Module Server
    
#' @rdname mod_line_charts
#' @export
#' @keywords internal
    
mod_line_charts_server <- function(input,
                                   output,
                                   session,
                                   selection_vars
                                   ){
  ns <- session$ns
  
  df <- reactive({
      dplyr::filter(pakeduc_district,
             age_range == !!selection_vars$age_range(),
             indicator == !!selection_vars$indicator(),
             dist_nm %in% !!selection_vars$district_name(),
             dataset %in% !!selection_vars$dataset(),
             !is.na(point_estimate),
             gender %in% !!selection_vars$gender())
  })
  
  # output$district_plot <- renderPlot({
  output$district_plot <- plotly::renderPlotly({
    p <- ggplot2::ggplot(df(), ggplot2::aes(x = year, y = point_estimate, color = gender)) +
      ggplot2::geom_line(ggplot2::aes(group = interaction(dist_nm, dataset, gender),
                                      linetype = dataset),
                         size = ggplot2::rel(0.8)) +
      ggplot2::geom_point(size = ggplot2::rel(2.8)) +
      ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      ggthemes::scale_color_colorblind() +
      ggplot2::facet_wrap(~dist_nm, nrow = 1) +
      cowplot::theme_cowplot(14)  +
      ggplot2::theme(
        legend.position = "none"
      ) +
      ggplot2::labs(
        x = "",
        y = "Share of population (%)"
      )
    plotly::ggplotly(p)
    # p
    
  })
  
  # output$district_table <- renderTable({df()})
}


    
## To be copied in the UI
# mod_line_charts_ui("line_charts_ui_1")
    
## To be copied in the server
# callModule(mod_line_charts_server, "line_charts_ui_1")
 
