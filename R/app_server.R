#' @import shiny
app_server <- function(input, output,session) {
  options(shiny.usecairo = TRUE)
  options(shiny.reactlog=TRUE) 

  selection_vars_province <- callModule(mod_province_select_server, "province_select_ui_1")
  callModule(mod_province_visuals_server, "province_visuals_ui_1", selection_vars = selection_vars_province)
  #callModule(mod_province_map_server, "province_map_ui_1", selection_vars = selection_vars_province)
  
  selection_vars_district <- callModule(mod_district_select_server, "district_select_ui_1")
  callModule(mod_district_visuals_server, "district_visuals_ui_1", selection_vars = selection_vars_district)
  #callModule(mod_district_map_server, "district_map_ui_1", selection_vars = selection_vars_district)
  
  selection_vars_country <- callModule(mod_country_select_server, "country_select_ui_1")
  callModule(mod_country_visuals_server, "country_visuals_ui_1", selection_vars = selection_vars_country)
}
