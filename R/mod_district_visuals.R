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
                  !is.na(point_estimate_weighted),
                  gender %in% !!gender_selection) %>%
      dplyr::mutate(
        label = dplyr::if_else(year == max(year), as.character(dist_nm), NA_character_)
      )
  })
  
  output$warning_message <- renderText({
    if (nrow(df()) == 0) {"No data available. Please make a new selection"}
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
                  !is.na(point_estimate))
  })
  
  output$district_plot <- plotly::renderPlotly({
    if (nrow(df()) > 0) {
      p <- ggplot2::ggplot(df(), ggplot2::aes(x = year, y = point_estimate_weighted, color = gender)) +
        ggplot2::geom_line(ggplot2::aes(group = gender),
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
    }
    
    if (nrow(surveydf()) > 0) {
      p <- p +
        ggplot2::geom_line(data = surveydf(),
                           ggplot2::aes(y = point_estimate, 
                                        group = interaction(dataset, gender), 
                                        linetype = dataset),
                           size = ggplot2::rel(0.6),
                           alpha = .6
        )
    }
    
    
    plotly::ggplotly(p)
    # p
    
  })
}
    
## To be copied in the UI
# mod_district_visuals_ui("district_visuals_ui_1")
    
## To be copied in the server
# callModule(mod_district_visuals_server, "district_visuals_ui_1")
 
