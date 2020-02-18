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
    ggiraph::ggiraphOutput(outputId = ns("province_map"), height = "600px"),
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
    
    out <- dplyr::filter(pakeduc_province_weighted,
                         indicator == !!selection_vars$indicator(),
                         gender %in% !!gender_selection,
                         year == !!selection_vars$year()) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        pe_percent = sprintf("%.1f%%", point_estimate * 100)
      )
    
    # Changed to right join, so that rows == 0 when out returns 0 rows
    out <- pakgeo_province %>%
      dplyr::left_join(out, by = c("province_id" = "province_id"))
      # dplyr::right_join(out, by = c("province_id" = "province_id"))
  })
  
  output$warning_message <- renderText({
    if (nrow(df()) == 0) {"No data available. Please make a new selection"}
  })
  
  output$province_map <- ggiraph::renderggiraph({
    if (nrow(df()) > 0) {
      p <- ggplot2::ggplot(df()) +
        ggiraph::geom_sf_interactive(ggplot2::aes(fill = point_estimate,
                                                  tooltip = paste("Province:", province,
                                                                  "<br />Value:", pe_percent,
                                                                  "<br />Year:", year),
                                                  data_id = province
                                                  )) +
        ggplot2::scale_fill_viridis_c(limits = c(0, 1), labels = scales::percent) +
        ggthemes::theme_map() +
        ggplot2::facet_wrap(~gender) +
        ggplot2::labs(
          fill = ""
        )
      
      ggiraph::girafe(ggobj = p, width_svg = 8, height_svg = 7)
    }
  })
}
    
## To be copied in the UI
# mod_province_map_ui("province_map_ui_1")
    
## To be copied in the server
# callModule(mod_province_map_server, "province_map_ui_1")
 
