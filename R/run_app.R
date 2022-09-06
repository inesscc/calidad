# addResourcePath(prefix = "www", directoryPath = "./www")



shiny_calidad <- function(){

shinyApp(ui = shinycalidad2::app_ui, server = shinycalidad2::app_server)

#shinyApp(ui = app_ui, server = app_server)
   # if (interactive()) {
   #
   #   runApp(appDir = system.file("app",
   #                               package = "shinycalidad2"))
   #
   # } else {
   #
   #
   #   shinyAppDir(appDir = system.file("app",
   #                                    package = "shinycalidad2"))
   #
   # }

}
