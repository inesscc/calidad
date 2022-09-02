
# UI ----
library(shiny)

ui <- shiny::div(shinyFeedback::useShinyFeedback(),
          shinyjs::useShinyjs(),
          # tags$head(
          #   tags$link(rel = "stylesheet", type = "text/css", href = "maqueta.css")
          # ),
          shiny::includeCSS("www/maqueta.css"),

          shiny::div(class="top-ine",
              shiny::fluidPage(
                shiny::div(class="container",
                    shiny::HTML('<div class="menu-ine">
                <img class="logo-ine" src="/ine_blanco.svg" alt="INE">
            </div>
            <div class="pull-right">
                <a class="btn btn-xs btn-primary" href="https://www.ine.cl" target="_blank">Volver al home INE</a>
            </div>'),
                )
              )
          ),
          shiny::div(class="conten-ine",

              ### fluid page de texto de descripción
              shiny::fluidPage(
                #   shiny::div(class="container-fluid",
                shiny::div(class="container",
                    shiny::HTML('<div class="row">
                <div class="col-md-12">
                    <h3 class="titu-ine">Evaluación de Calidad de Estimaciones en Encuestas de Hogares</h3>
                    <p class="text-ine">
Esta aplicación permite acercar a las personas usuarias la implementación del estándar de calidad para la evaluación de estimaciones en encuestas de hogares del INE. A través de ella, las personas usuarias pueden conocer la precisión que tienen las estimaciones generadas a partir de encuestas producidas por el INE u otras encuestas que utilicen muestreo probabilístico estratificado y en 2 etapas. Con esto se busca poner a disposición de la comunidad una herramienta interactiva para la cual no se requiere contar con conocimientos de programación, promoviendo el uso adecuado de la información publicada. Esta aplicación permite evaluar la calidad de la estimación de medias, totales y proporciones.                    </p>
                </div>
            </div>')
                )
              )
          ),
          # Agregar el logo del INE

          shiny::div(class="dash-ine",
              shiny::fluidPage(
                waiter::useWaitress(),
                shiny::div(class="container",
                    sidebarLayout(
                      ## Sidebar ####
                      sidebarPanel(width = 3,
                                   ## UI INPUT ####
                                   shinyWidgets::radioGroupButtons(
                                     inputId = "Id004",
                                     label = h4("Selecciona desde donde cargar base de datos"),
                                     choices = c("Cargar datos propios", "Trabajar con datos INE"),
                                     status = "primary",
                                     justified = TRUE
                                   ),
                                   h5("En esta Sección puedes seleccionar la opción de cargar una base de datos desde tu computador, o cargar una base de datos del INE"),
                                   uiOutput("datos_locales"),
                                   uiOutput("DescargaINE"),
                                   shinyWidgets::radioGroupButtons(
                                     inputId = "SCHEME",
                                     label = h5("Selecciona el esquema de evaluación, INE o CEPAL"),
                                     choices = c("chile", "cepal"),
                                     status = "primary",
                                     justified = TRUE
                                   ),
                                   ## render selección de variables de interes, y de cruce
                                   # uiOutput("seleccion1"),
                                   selectInput("varINTERES", label = h5("Variable de interés"),choices = "",  multiple = F),
                                   #textOutput("wrn_var_int"),

                                   uiOutput("denominador"),

                                   radioButtons("tipoCALCULO", "¿Qué tipo de cálculo deseas realizar?",
                                                choices = list("Media","Proporción","Suma variable Continua","Conteo casos"), inline = F ),
                                   selectInput("varCRUCE", label = h5("Desagregación"), choices = "", selected = NULL, multiple = T),
                                   checkboxInput("IC", "¿Deseas agregar intervalos de confianza?",value = F),
                                   #checkboxInput("ajuste_ene", "¿Deseas agregar los ajuste del MM ENE?",value = F),
                                   uiOutput("etiqueta"),
                                   selectInput("varSUBPOB", label = h5("Subpoblación"), choices = "", selected = NULL, multiple = F),
                                   selectInput("varFACT1", label = h5("Variable para factor de expansión"), choices = "",selected ="", multiple = F),
                                   selectInput("varCONGLOM", label = h5("Variable para conglomerados"), choices = "", selected = "", multiple = F),
                                   selectInput("varESTRATOS",label = h5("Variable para estratos"), choices = "", selected = "", multiple = F),
                                   shinyjs::disabled(downloadButton("tabla", label = "Descargar tabulado")),
                                   actionButton("actionTAB", label = "Generar tabulado"),
                                   ## render selección variables DC
                                   uiOutput("seleccion2"),
                                   ## botón generación tabulado
                                   uiOutput("botonTAB")
                      ),
                      ## Main PANEL ----
                      mainPanel(width = 9,
                                verbatimTextOutput("tipoCalText"),
                                #### render titulo tabulado
                                uiOutput("tituloTAB")
                      )
                    )
                )
              )
          ),
          shiny::div(class="footer",
              shiny::fluidPage(
                shiny::div(class="container",
                    shiny::HTML('<div class="row">
                <div class="col-md-4">
                    <h4>INE en redes sociales</h4>
                    <a href="https://www.facebook.com/ChileINE/" target="_blank"><img class="facebook" src="facebook.svg"></a>
                    <a href="https://twitter.com/ine_chile?lang=es" target="_blank"><img class="twitter" src="twitter.svg"></a>
                    <a href="https://www.youtube.com/user/inechile" target="_blank"><img class="youtube" src="youtube.svg"></a>
                    <a href="https://www.instagram.com/chile.ine/" target="_blank"><img class="instagram" src="instagram.svg"></a>
                    <h4>Consultas</h4>
                    <p><a href="https://www.portaltransparencia.cl/PortalPdT/ingreso-sai-v2?idOrg=1003" target="_blank">Solicitud de acceso a la información pública</a></p>
                    <p><a href="https://atencionciudadana.ine.cl/" target="_blank">Atención ciudadana</a></p>
                </div>
                <div class="col-md-4">
                    <h4>Contacto</h4>
                    <p>
                        Dirección nacional: Morandé N°801, piso 22, Santiago, Chile<br>
                        RUT: 60.703.000-6<br>
                        Código postal: 8340148<br>
                    </p>
                </div>
                <div class="col-md-4">
                    <h4>SIAC / OIRS</h4>
                    <p>
                        Horario de atención:<br>
                        Lunes a viernes 9:00 a 17:00 horas<br>
                        Fono : <a>232461010</a> - <a>232461018</a><br>
                        Correo: ine@ine.cl<br>
                    </p>
                </div>
            </div>')
                )
              )
          ),
          shiny::div(class="pie-ine",
              shiny::fluidPage(
                shiny::div(class="container",
                    shiny::HTML('
        <div class="text-right">
            Instituto Nacional de Estadísticas
       </div>')
                )
              )
          )
)

          ###### render UI origen de datos #####

renderUI_origen_datos <- function(req){

  if(req == "Trabajar con datos INE"){

    tagList(
      ## input archivo página del INE
      selectInput("base_web_ine", label = h4("Utilizar una base de datos del INE"),
                  choices = nom_arch_ine,
                  multiple = F),

      actionButton("base_ine", label = "Cargar base desde el INE"),

    )

  }else if(req == "Cargar datos propios"){

    tagList(
      ## input de archivo local -----
      fileInput(inputId = "file", label = h4("Carga una base de datos desde tu computador"),buttonLabel = "Buscar" , placeholder = ".sav .rds .dta .sas .xlsx .csv .feather")
    )

  }
}


### modal definicion indicadores ####

modal_indicadores <- function(){
showModal(modalDialog(
  title = "Definición de indicadores",

  HTML("<strong><h2>Insumos a evaluar:</h2></strong>      <br>
<h4><strong>ES:</strong> Error Estandar.        <br>
<strong>Coef_var:</strong> Coeficiente de Variación.       <br>
<strong>GL:</strong> Grados de Libertad.        <br>
<strong>n:</strong> Casos muestrales.     </h4>     <br>
<strong><h2>Resultados de la evaluación: </h2></strong>         <br>
<h4><strong>eval_n:</strong> Evaluación de casos muestrales        <br>
<strong>eval_gl:</strong> Evaluación de grados de libertad.        <br>
<strong>tipo_eval:</strong> Tipo de evaluación utilizada: Puede ser por Error Estandar o por Coeficiente de variación.        <br>
<strong>Cuadrática:</strong> Resultado de evaluación por función cuadrática.        <br>
<strong>eval_se:</strong> Resultado de la evaluación del Error Estandar.        <br>
<strong>eval_cv:</strong> Resultado de la evaluación del Coeficiente de variación.        <br>
<strong>calidad:</strong> Evaluación final de la celda, puede ser: <p style=\"font-style: italic;\"> Fiable, Poco Fiable o No fiable </p> </h4>      <br>"
  ), easyClose = T
))
}

### render UI main panel ####

renderUI_main_panel <- function(){

tagList(
  div(id="panel_central",class="titu-ine",
      h2("Resultado evaluación de calidad"),
  actionButton("show", "Definición de indicadores"),
  verbatimTextOutput("PRUEBAS2"),
  ### render gráfico de resumen
  div(style='width:100%;overflow-x: scroll;',
      div(plotOutput('grafico'),
          align = "center",
          style = "height:200px"),
      ### render tabulado
      tags$div(
        class="my_table", # set to custom class
        htmlOutput("tabulado") %>% shinycssloaders::withSpinner(color="#0dc5c1"))
  ))
)

}
