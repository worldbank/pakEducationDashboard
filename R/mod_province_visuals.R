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
    
    dplyr::filter(pakeduc_province,
                  indicator == !!selection_vars$indicator(),
                  province %in% !!selection_vars$province(),
                  dataset %in% !!selection_vars$dataset(),
                  !is.na(point_estimate),
                  gender %in% !!gender_selection) %>%
      dplyr::mutate(
        label = dplyr::if_else(year == max(year), as.character(province), NA_character_)
      )
  })
  
  output$warning_message <- renderText({
    if (nrow(df()) == 0) {"No data available. Please make a new selection"}
  })
  
  
  output$province_plot <- plotly::renderPlotly({
    if (nrow(df()) > 0) {
      p <- ggplot2::ggplot(df(), ggplot2::aes(x = year, y = point_estimate, color = gender)) +
        ggplot2::geom_line(ggplot2::aes(group = interaction(province, dataset, gender),
                                        linetype = dataset),
                           size = ggplot2::rel(0.8)) +
        ggplot2::geom_point(size = ggplot2::rel(2.8),
                            ggplot2::aes(shape = dataset)) +
        ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
        ggthemes::scale_color_colorblind() +
        cowplot::theme_cowplot(14) +
        ggplot2::facet_wrap(~province, nrow = 1) +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          legend.position = "none"
        ) +
        ggplot2::labs(
          x = "",
          y = "Share of population (%)"
        )
      plotly::ggplotly(p)
    }
    #p
    
  })
}
    
## To be copied in the UI
# mod_province_visuals_ui("province_visuals_ui_1")
    
## To be copied in the server
# callModule(mod_province_visuals_server, "province_visuals_ui_1")
 
