#' @import shiny
app_server <- function(input, output,session) {
  options(shiny.usecairo = TRUE)
  # List the first level callModules here
  selection_vars <- callModule(mod_main_ui_server, "main_ui_ui_1")
  callModule(mod_line_charts_server, "line_charts_ui_1", selection_vars = selection_vars)
  callModule(mod_line_charts_province_server, "line_charts_province_ui_1", selection_vars = selection_vars)
}
