# Module UI
  
#' @title   mod_province_map_ui and mod_province_map_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_province_map
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_province_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(outputId = ns("province_map"), height = "600px"),
    textOutput(ns("warning_message"))
  )
}
    
# Module Server
    
#' @rdname mod_province_map
#' @export
#' @keywords internal
    
mod_province_map_server <- function(input, 
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
    
    # TODO: Mention to Tony that
    ##  40/1900 rows have Unnamed provinces which will be excluded for visualizatin
    out <- dplyr::filter(pakeduc_province,
                         indicator == !!selection_vars$indicator(),
                         dataset == max(!!selection_vars$dataset()),
                         gender == max(!!gender_selection),
                         year == !!selection_vars$year()) %>%
      dplyr::distinct()
    
    out <- pakgeo_province %>%
      dplyr::left_join(out, by = c("province_id" = "province_id"))
  })
  
  output$warning_message <- renderText({
    if (nrow(df()) == 0) {"No data available. Please make a new selection"}
  })
  
  output$province_map <- plotly::renderPlotly({
    if (nrow(df()) > 0) {
      p <- ggplot2::ggplot(df()) +
        # TODO:: investigate
        # province.x and province.y are forming from the left join in df()
        ggplot2::geom_sf( ggplot2::aes(fill = point_estimate, text = province.x)) +
        ggplot2::scale_fill_viridis_c() +
        ggplot2::theme_void()
      
      plotly::ggplotly(p, tooltip = c("text", "fill")) %>% plotly::style(hoveron = "fill")
      
      
      
      
    }
    # p
    
  })
}
    
## To be copied in the UI
# mod_province_map_ui("province_map_ui_1")
    
## To be copied in the server
# callModule(mod_province_map_server, "province_map_ui_1")
 
