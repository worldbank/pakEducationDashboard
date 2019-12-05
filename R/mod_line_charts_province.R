# Module UI
  
#' @title   mod_line_charts_province_ui and mod_line_charts_province_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @import  ggplot2
#'
#' @rdname mod_line_charts_province
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_line_charts_province_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(textOutput(ns("province_title"))),
    plotly::plotlyOutput(outputId = ns("province_plot"))
    #plotOutput(outputId = ns("province_plot"))
  
  )
}
    
# Module Server
    
#' @rdname mod_line_charts_province
#' @export
#' @keywords internal
    
mod_line_charts_province_server <- function(input, 
                                            output, 
                                            session,
                                            selection_vars){
  ns <- session$ns
  
  df <- reactive({
    dplyr::filter(pakeduc_province,
                  age_range == !!selection_vars$age_range(),
                  indicator == !!selection_vars$indicator(),
                  province %in% !!selection_vars$province(),
                  dataset %in% !!selection_vars$dataset(),
                  !is.na(point_estimate),
                  gender %in% !!selection_vars$gender()) %>%
      dplyr::mutate(
        label = dplyr::if_else(year == max(year), as.character(province), NA_character_)
        )
  })
  
  df_national <- reactive({
    dplyr::filter(pakeduc_country,
                  age_range == !!selection_vars$age_range(),
                  indicator == !!selection_vars$indicator(),
                  dataset %in% !!selection_vars$dataset(),
                  !is.na(point_estimate),
                  gender == "Both") %>%
      dplyr::mutate(
        label = dplyr::if_else(year == max(year), "National average", NA_character_)
      )
  })
  
  output$province_title <- renderText({
    indic <- stringr::str_replace_all(selection_vars$indicator(), "_", " ")
    indic <- stringr::str_to_title(indic)
    paste0(indic, ", Age ", selection_vars$age_range(), "\n")
  })
  
  #output$province_plot <- renderPlot({
  output$province_plot <- plotly::renderPlotly({
    p <- ggplot2::ggplot(df(), ggplot2::aes(x = year, y = point_estimate)) +
      ggplot2::geom_line(ggplot2::aes(group = interaction(province, dataset, gender),
                                      linetype = dataset,
                                      color = gender),
                         size = ggplot2::rel(0.8)) +
      ggplot2::geom_line(data = df_national(),
                         ggplot2::aes(linetype = dataset),
                         size = ggplot2::rel(0.8),
                         alpha = .45,
                         color = "red") +
      ggplot2::geom_text(data = df_national(),
                         ggplot2::aes(label = label),
                         nudge_x = -.5,
                         nudge_y = -.05 ) +
      ggplot2::geom_point(size = ggplot2::rel(2.8),
                          ggplot2::aes(color = gender)) +
      # ggrepel::geom_label_repel(ggplot2::aes(label = label,
      #                                        nudge_x = 1,
      #                                        na_rm = TRUE)) +
      ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      ggthemes::scale_color_colorblind() +
      cowplot::theme_cowplot(14) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom"
      ) +
      ggplot2::labs(
        x = "",
        y = "Share of population (%)"
      )
    plotly::ggplotly(p)
    #p
    
  })
}
    
## To be copied in the UI
# mod_line_charts_province_ui("line_charts_province_ui_1")
    
## To be copied in the server
# callModule(mod_line_charts_province_server, "line_charts_province_ui_1")
 
