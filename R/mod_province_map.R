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
    shinycssloaders::withSpinner(
      ggiraph::ggiraphOutput(outputId = ns("province_map"), 
                             width = "100%", 
                             height = "1000px"), 
      type = 3, 
      color = "#6c3b96",
      color.background = "#FFFFFF"),
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
    out <- dplyr::filter(pakeduc_province_weighted,
                         indicator == !!selection_vars$indicator(),
                         #gender %in% !!gender_selection,
                         year == !!selection_vars$year(),
                         # Add only both
                         gender == "Both"
                         ) %>% 
      dplyr::distinct() 
    
    pakgeo_province %>%
      dplyr::left_join(out, by = c("province_id" = "province_id"))
  })
  
  output$warning_message <- renderText({
    if (nrow(df()) == 0) {"No data available. Please make a new selection"}
  })
  
  output$province_map <- ggiraph::renderggiraph({
    if (nrow(df()) > 0) {
      p <- plot_map(data = df(),
                    fill = point_estimate,
                    data_id = province,
                    year = year,
                    tooltip_region_header = "Province:",
                    tooltip_region_value = province,
                    tooltip_value = pe_percent) #+
        #ggplot2::facet_wrap(~gender)
    
      ggiraph::girafe(ggobj = p,
                      width_svg = 12,
                      height_svg = 12,
                      options = list(ggiraph::opts_tooltip(use_fill = TRUE)))
    }
  })
}
 
