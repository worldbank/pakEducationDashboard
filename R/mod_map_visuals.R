#' map_visuals UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_visuals_ui <- function(id){
  ns <- NS(id)
  tagList(
    # h3(textOutput(ns("map_title"))),
    # p(textOutput(ns("map_ind_description"))),
    shinycssloaders::withSpinner(
      ggiraph::ggiraphOutput(outputId = ns("district_map"),
                             width = "100%", 
                             height = "1000px"),
      type = 3, 
      color = "#009DA7",
      color.background = "#FFFFFF"),
    textOutput(ns("warning_message"))
  )
}
    
#' map_visuals Server Function
#'
#' @noRd

mod_map_visuals_server <- function(input, 
                                   output, 
                                   session,
                                   selection_vars){
  ns <- session$ns
  
  map_title <- reactive({
    names(unlist(unname(indicator_choices_country)))[unlist(indicator_choices_country) == selection_vars$indicator()]
  })
  
  df <- reactive({
    # Handles potential issues due to missing selection inputs
    req(selection_vars$year())
    
    if (selection_vars$dataset() == "weighted_average") {
      
      out <- dplyr::filter(pakeduc_district_weighted,
                           indicator == !!selection_vars$indicator(),
                           year == !!selection_vars$year(),
                           dimensions == "aggregate") %>%
        dplyr::distinct()
      
      out <- pakgeo_district %>%
        dplyr::left_join(out, by = c("dist_key" = "identifier"))
    } else {
      out <- dplyr::filter(pakeduc_district,
                           dataset == !!selection_vars$dataset(),
                           indicator == !!selection_vars$indicator(),
                           year == !!selection_vars$year(),
                           dimensions == "aggregate") %>%
        dplyr::distinct()
      
      out <- pakgeo_district %>%
        dplyr::left_join(out, by = c("dist_key" = "identifier"))
    }
  })
  
  output$warning_message <- renderText({
    if (nrow(df()) == 0) {"No data available. Please make a new selection"}
  })
  
  map_ind_description <- reactive({
    unique(df()$indicator_definition[!is.na(df()$indicator_definition)])
  })
  
  output$district_map <- ggiraph::renderggiraph({
    if (nrow(df()) > 0) {
      map_data <- df()
      sf::st_crs(map_data) <- 4326
      p <- plot_map(data = map_data,
                    fill = point_estimate,
                    data_id = district,
                    year = year,
                    tooltip_region_header = "District:",
                    tooltip_region_value = DISTRICT,
                    tooltip_value = pe_percent,
                    tooltip_dataset = dataset,
                    title = map_title(),
                    subtitle = map_ind_description()) #+
      #ggplot2::facet_wrap(~gender)
      
      p <- finalise_plot(p) +
        theme_wb()
      
      ggiraph::girafe(ggobj = p, 
                      width_svg = 12, 
                      height_svg = 12,
                      options = list(ggiraph::opts_tooltip(use_fill = TRUE)))
    }
  })
}
    
## To be copied in the UI
# mod_map_visuals_ui("map_visuals_ui_1")
    
## To be copied in the server
# callModule(mod_map_visuals_server, "map_visuals_ui_1")
