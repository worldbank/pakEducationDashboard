# Module UI
  
#' @title   mod_district_map_ui and mod_district_map_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_district_map
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_district_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    ggiraph::ggiraphOutput(outputId = ns("district_map"), height = "600px"),
    #plotOutput(outputId = ns("district_map"), height = "600px"),
    textOutput(ns("warning_message"))
  
  )
}
    
# Module Server
    
#' @rdname mod_district_map
#' @export
#' @keywords internal
    
mod_district_map_server <- function(input, 
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
   
    out <- dplyr::filter(pakeduc_district_weighted,
                         indicator == !!selection_vars$indicator(),
                         gender %in% !!gender_selection,
                         year == !!selection_vars$year()) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        pe_percent = sprintf("%.1f%%", point_estimate * 100)
      )
    
    out <- pakgeo_district %>%
      dplyr::left_join(out, by = c("dist_key" = "dist_key")) 
  })

  output$warning_message <- renderText({
    if (nrow(df()) == 0) {"No data available. Please make a new selection"}
  })

  output$district_map <- ggiraph::renderggiraph({
  #output$district_map <- renderPlot({
    if (nrow(df()) > 0) {
      p <- ggplot2::ggplot(df()) +
        #ggplot2::geom_sf( ggplot2::aes(fill = point_estimate)) +
        ggiraph::geom_sf_interactive( ggplot2::aes(fill = point_estimate, 
                                                     tooltip = paste(
                                                       "District:",    DISTRICT,
                                                       "<br />Value:", pe_percent,
                                                       "<br />Year:",  year
                                                       ))) +
        ggplot2::scale_fill_viridis_c(limits = c(0, 1), labels = scales::percent) +
        ggthemes::theme_map() +
        #ggplot2::facet_wrap(~gender) +
        ggplot2::labs(
          fill = ""
        )

      ggiraph::girafe(ggobj = p)
    }
  })
}
    
## To be copied in the UI
# mod_district_map_ui("district_map_ui_1")
    
## To be copied in the server
# callModule(mod_district_map_server, "district_map_ui_1")
 
