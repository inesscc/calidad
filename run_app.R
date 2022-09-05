addResourcePath(prefix = "www", directoryPath = "./www")

shiny_calidad <- function() {


shinyApp(ui = app_ui, server = app_server)
  # if (interactive()) {
  #
  #   runApp(appDir = system.file("app",
  #                               package = "advancedShiny"))
  #
  # } else {

  #
  #   shinyAppDir(appDir = system.file("app",
  #                                    package = "advancedShiny"))
  #
  # }

}
