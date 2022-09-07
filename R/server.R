# https://stackoverflow.com/questions/42422310/assigning-custom-css-classes-to-designed-object-via-external-css-file

# encontrar todas las dependencias y guardarlas en DESCRIPTION
# purrr::map(attachment::att_from_rscripts(),usethis::use_package)

library(calidad)
library(dplyr)
library(feather)
library(forcats)
library(ggplot2)
library(haven)
library(kableExtra)
library(labelled)
library(readxl)
library(readr)
library(rlang)
library(shiny)
library(shinycssloaders)
library(shinyFeedback)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(shinybusy)
library(stringr)
library(survey)
library(pkgload)



## testeo

# debug_chunk <- print(list(paste("Tipo de calculo:",input$tipoCALCULO),
#                 paste("acctionButton =",is.null(input$actionTAV)),
#                 paste("varINTERES =",input$varINTERES),
#                 paste("varCRUCE =",input$varCRUCE),
#                 paste("varCRUCE = \"\"",input$varCRUCE == ""),
#                 paste("varCRUCE = NULL?",is.null(input$varCRUCE)),
#                 paste("varDENOM =",input$varDENOM),
#                 paste("varDENOM = \"\"",input$varDENOM == ""),
#                 paste("varDENOM = NULL?",is.null(input$varDENOM)),
#                 paste("varSUBPOB =",input$varSUBPOB),
#                 paste("varSUBPOB = \"\"",input$varSUBPOB == ""),
#                 paste("varSUBPOB = NULL?",is.null(input$varSUBPOB)),
#                 paste("varFACTEXP =",input$varFACT1),
#                 paste("war_varINTERES =",wrn_var_int()),
#                 paste("war_varDENOM =",wrn_var_denom()),
#                 paste("warn_var_subpob =",wrn_var_subpop()),
#                 paste("warn_var_factEXP =",wrn_var_factexp()),
#                 paste("IC =",input$IC),
#                 paste("WARN_resume =",warning_resum()),
#                 paste("datos =",dim(datos()))
#  ))


#archivos_ine <- list.files("calidad/data/")
archivos_ine <- list.files("data/shiny/",pattern = ".rda") #c("enusc_2018.feather","enusc_2019.feather","enusc_2020.feather","epf_hogares.feather","epf_personas_ing.feather","esi_2020.feather")

nom_arch_ine <- archivos_ine %>% stringr::str_remove_all(".rda")

# posibilidad nombres de variables DC"
fact_exp = c("Fact_pers","Fact_Pers","fe","fact_cal", "fact_pers","fact_pers","fe","fact_cal", "fact_cal_esi","FACT_PERS","FACT_PERS","FE","FACT_CAL","FACT_CAL_ESI","wgt1","EXP","exp","Exp")
conglomerados = c("Conglomerado", "id_directorio","varunit", "conglomerado","id_directorio","varunit","CONGLOMERADO","ID_DIRECTORIO","VARUNIT","VarUnit")
estratos = c("VarStrat", "estrato", "varstrat","varstrat", "estrato",  "varstrat","VARSTRAT", "ESTRATO",  "VARSTRAT","VarStrat")

### DEBUG ####
debug = F
show_wrn = T



# SERVER ----

app_server <- function(input, output, session) {
  ### trakear error ####
  ## options(shiny.trace = TRUE)

  ## parametro máximo de archivos subidos ###
  options(shiny.maxRequestSize=40*1024^2, scipen=999) # to the top of server.R would increase the limit to 30MB.

# observe({
#   print(paste("fe",input$Id004b))
# })
  # ### render UI carga de datos o trabajo con datos del INE


  output$datos_locales <- renderUI({
    req(input$Id004 == "Cargar datos propios")

    renderUI_origen_datos("Cargar datos propios")})

  output$DescargaINE <-  renderUI({
    req(input$Id004 == "Trabajar con datos INE")

    renderUI_origen_datos("Trabajar con datos INE")})

  ### + I N P U T S + ####

  ### CARGA: base local ####
  # MODAL carga archivo formato no soportado ####

  ## formatos soportados
  soportados <- c(".sav",".rds",".dta", ".sas", ".xlsx", ".csv",".feather")

# observeEvent(input$file,{
#
#     req(!any(grepl(paste(soportados,collapse = "|"), tolower(input$file$datapath))))
#     # tiene que haber warning para que se muestre
#     shinyalert("¡Error!", "¡Formato de archivo no soportado!", type = "error")
# })


### función carga de datos locales por usuario
  data_input <- reactive({
    req(any(grepl(paste(soportados,collapse = "|"), input$file$datapath)))
    carga_datos_locales(input$file$datapath)
    })


# DESCARGA: DE DATOS PÁGINA INE ----
  descarga =  eventReactive(input$base_ine, {

    # Modal para descarga
    show_modal_spinner() # show the modal window

    datos <- load_object(paste0("data/shiny/",input$base_web_ine,".rda"))

    #   # Modal para descarga
    remove_modal_spinner() # remove it when done

    datos
  })

  # SWITCH: DESCARGA DATOS WEB INE | COMPUTADOR LOCAL ----

  datos <- reactiveVal(NULL)

  observeEvent(input$file, {

    if(!any(grepl(paste(soportados,collapse = "|"), tolower(input$file$datapath)))){
         # tiene que haber warning para que se muestre
         shinyalert("¡Error!", "¡Formato de archivo no soportado!", type = "error")
    }else{
    new <- data_input()
    datos(new)
    }

  })

  observeEvent(input$base_ine, {
    datos(descarga())
  })

  ### EXTRACT: names variables input datos ####
  variables_int <- reactive({
    if (!is.null(input$file)) {
      names(data_input())
    } else if (!is.null(descarga())) {
      names(descarga())
    }
  })

  ### find var fact exp ####

  var_selec_fact <- reactive({names(datos())[grep(paste0("^",fact_exp,"$",collapse = "|"),names(datos()))]})

  ### find var conglom ####

  var_select_conglom  <- reactive({names(datos())[grep(paste0("^",conglomerados,"$",collapse = "|"),names(datos()))]})

  ### find var strat ####

  var_select_estrat <- reactive({names(datos())[grep(paste0("^",estratos,"$",collapse = "|"),names(datos()))]})

  observeEvent(list(datos(),
                    input$Id004,
                    input$base_web_ine),{
    updateSelectInput(session, "varINTERES",
                      choices = variables_int(),
                      selected = "" )})

  observeEvent(list(any(is.null(datos()),input$tipoCALCULO == "Proporción"),
               input$Id004,
               input$base_web_ine),{
    updateSelectInput(session, "varDENOM",
                      choices = variables_int())
  })

  observeEvent(list(datos(),
                    input$Id004,
                    input$base_web_ine),{
    updateSelectInput(session, "varCRUCE",
                      choices = c("",variables_int()),
                      selected = "" )
    })

  ### update inputs ####

  observeEvent(list(datos(),
                    input$Id004,
                    input$base_web_ine),{
    updateSelectInput(session, "varSUBPOB",
                      choices = c("",variables_int()),
                      selected = "" )
                    })

  observeEvent(list(datos(),
                    input$Id004,
                    input$base_web_ine),{
    updateSelectInput(session, "varFACT1",
                      choices = variables_int(),
                      selected = var_selec_fact()
    )})

  observeEvent(list(datos(),
                    input$Id004,
                    input$base_web_ine),{
    updateSelectInput(session, "varCONGLOM",
                      choices = variables_int(),
                      selected = var_select_conglom()
    )})
  observeEvent(list(datos(),
                    input$Id004,
                    input$base_web_ine),{
    updateSelectInput(session, "varESTRATOS",
                      choices = variables_int(),
                      selected = var_select_estrat()
    )})

  ### + R E N D E R - U I + ####

  # ### RENDER: IN SIDE BAR  ####
  output$etiqueta <- renderUI({
    req(input$varCRUCE >= 1)
    req(labelled::is.labelled(datos()[[input$varCRUCE[1]]]))
    checkboxInput("ETIQUETAS", "Sus datos poseen etiquetas, ¿Desea agregarlas?",value = F)

  })

  output$denominador <- renderUI({
    req(input$tipoCALCULO == "Proporción" & input$SCHEME == "chile")
    selectInput("varDENOM", label = h5("Denominador - Opcional"),choices = variables_int(), selected = NULL, multiple = T)
    # selectInput("varINTERES", label = h5("Variable de interés"),choices = "",  multiple = F),

  })


  #### + O U T P U T S + ####

  ### CREATE: tabulados  ----

  tabuladoOK <- reactive({

    req(input$actionTAB)

    # para generarse necesita que no hayan warnings
    req(!warning_resum(), input$varINTERES,input$varCONGLOM, input$varESTRATOS, input$varFACT1)

    tabulado = create_tabulado(base = datos(),
                               v_interes =  input$varINTERES,
                               denominador = input$varDENOM,
                               v_cruce = input$varCRUCE,
                               v_subpob =  input$varSUBPOB,
                               v_fexp1 = input$varFACT1,
                               v_conglom = input$varCONGLOM,
                               v_estratos = input$varESTRATOS,
                               tipoCALCULO = input$tipoCALCULO,
                               ci = input$IC,
                               scheme = input$SCHEME,
                               etiquetas = input$ETIQUETAS,
                               ajuste_ene = FALSE)
  })

  #tabuladoOK <- isolate({tabulado0})


  ### RENDER: Tabulado ####

  observeEvent(tabuladoOK(),{
  output$tabulado  <- renderText({

  out <- tabla_html_shiny(tabuladoOK(), input = input) # %>% rename(es = se, cv = coef_var, media = contains("mean"))

  return(out)
  })

  ### RENDER: GRÁFICO DE BARRAS CON PORCENTAJE DE CELDAS POR CATEGORÍA ####
  output$grafico  <- renderPlot({
    create_plot(tabuladoOK(),scheme = input$SCHEME)
  }, height = 170, width = 800)

})

  ### MODAL definiciones ####

  observeEvent(input$show, {
    modal_indicadores()
  })

  ## Cerrar modal

  observeEvent(input$cerrar_modal, {
    removeModal()
    # do something after user confirmation
  })

  ### Nombre de datos utilizado ####

  output$PRUEBAS2 <- renderUI({
    if(!is.null(input$file)){
      orig  <- input$file$name
    }else{
      orig  <- input$base_web_ine
    }

    ret <- paste("Datos:",orig)

    h5(ret)
  })




  ### RENDER: IN MAIN PANEL -----
  ### Render título tabulado

observeEvent(input$actionTAB,{
  # print(paste("action :",input$actionTAB))

    req(!warning_resum())

    output$tituloTAB <- renderUI({

  renderUI_main_panel()

    })

  })
  ### anulamos el render UI en caso de cambiar selección
  observeEvent(list(input$Id004,
                    input$base_web_ine),{
                      # print(paste("opciones:",input$Id004))

                      output$tituloTAB <- renderUI({

                      })

                    })

### generamos otro render UI en caso de que se quieran editar datos


                      # print(paste("opciones:",input$Id004))

 output$edicion_datos <- renderUI({
   req(input$edit_data)
      tagList(
      div(id="panel_central",class="titu-ine",
      h2("Creación de variables"),
      actionButton("show", "Definición de indicadores"))
      )
 })



  # DESCARGA: DE TABULADO GENERADO ----

  # Habilitar botón de descarga
  observeEvent(tabuladoOK(),{
    req(!warning_resum(), input$varINTERES)
    req(input$actionTAB, tabuladoOK())

    enable("tabla")
  })


  output$tabla <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(tabuladoOK(), file)
    }
  )




  ### Alertas warnings #####
  # input$varINTERES
  # input$varDENOM
  # input$varCRUCE
  # input$varSUBPOB
  # input$varFACT1
  # input$varCONGLOM
  # input$varESTRATOS
  # input$tipoCALCULO
  # input$IC
  # input$ajuste_en

  # "tipoCALCULO", "¿Qué tipo de cálculo deseas realizar?",
  # choices = list("Media","Proporción","Suma variable Continua","Conteo casos", "Mediana")

  #### warning alert var interes ####
  wrn_var_int <- reactive({

    if(show_wrn == F){

      even = FALSE

    }else{

      var <- input$varINTERES
      even <- FALSE

      if(var != "") {

        if(input$tipoCALCULO %in% c("Media","Suma variable Continua")) {
          es_prop <- datos() %>%
            dplyr::mutate(es_prop_var = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0 | is.na(!!rlang::parse_expr(var)),1,0))

          even <- sum(es_prop$es_prop_var) == nrow(es_prop)
          shinyFeedback::feedbackWarning("varINTERES", even, "¡La variable no es continua!")

          even

        }else if(input$tipoCALCULO %in% c("Proporción","Conteo casos")){

          es_prop <- datos() %>%
            dplyr::mutate(es_prop_var = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0 | is.na(!!rlang::parse_expr(var)),1,0))

          even <- sum(es_prop$es_prop_var) != nrow(es_prop)
          shinyFeedback::feedbackWarning("varINTERES", even, "¡La variable no es de proporcion!")

          even
        }
      }else{even}
    }
  })

  ### warning alert var denom ####

  wrn_var_denom <- reactive({
    #  req(input$tipoCALCULO == "Proporción", input$varDENOM)
    if(show_wrn == F | is.null(input$varDENOM)){

      even = FALSE

    }else{

      var <- input$varDENOM
      even <- FALSE

      if(var != ""){

        es_prop <- datos() %>%
          dplyr::mutate(es_prop_var = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0 | is.na(!!rlang::parse_expr(var)),1,0))

        even <- sum(es_prop$es_prop_var) != nrow(es_prop)
        shinyFeedback::feedbackWarning("varDENOM", even, "¡La variable no es de proporcion!")

        even
      }else{even}
    }
  })


  #### warning alert var subpop ####
  wrn_var_subpop <- reactive({

    if(show_wrn == F){

      even = FALSE

    }else{

      subpop <- input$varSUBPOB
      even <- FALSE

      if(subpop != "") {

        es_prop_subpop <- datos() %>% dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop) == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))

        ## Primero se inspecciona si tiene NAs
        if(sum(is.na(es_prop_subpop[[subpop]] > 0)) > 0){

          even <- sum(is.na(es_prop_subpop[[subpop]] > 0)) > 0

          shinyFeedback::feedbackWarning("varSUBPOB", even, "subpop contiene NAs!")

          even

          ### si no tiene NA se inspecciona si es continua
        }else if(sum(is.na(es_prop_subpop[[subpop]] > 0)) == 0){

          even <- sum(es_prop_subpop$es_prop_subpop) != nrow(es_prop_subpop)

          shinyFeedback::feedbackWarning("varSUBPOB",even,"subpop debe ser una variable dummy!")

          even

        }

        # sum(es_prop_subpop$es_prop_subpop) == nrow(es_prop_subpop)

      }else{even}
    }
  })

  #### warning alert var fact_exp ####
  wrn_var_factexp <- reactive({
    req(input$varFACT1)

    if(show_wrn == F){

      even = FALSE

    }else{

      var <- input$varFACT1
      even <- FALSE

      if(var != "") {

        even <- sum(is.na(datos()[[var]] > 0)) > 0

        shinyFeedback::feedbackWarning("varFACT1", even, "La variable contiene NAs!")

        even

        # sum(es_prop_subpop$es_prop_subpop) == nrow(es_prop_subpop)

      }else{even}
    }
  })

  #### warning alert var conglom ####
  wrn_var_conglom <- reactive({

    if(show_wrn == F){

      even = FALSE

    }else{

      var <- input$varCONGLOM
      even <- FALSE

      if(var != "") {

        even <- sum(is.na(datos()[[var]] > 0)) > 0

        shinyFeedback::feedbackWarning("varCONGLOM", even, "La variable contiene NAs!")

        even

        # sum(es_prop_subpop$es_prop_subpop) == nrow(es_prop_subpop)

      }else{even}
    }
  })

  #### warning alert var strata ####
  wrn_var_strata <- reactive({

    if(show_wrn == F){

      even = FALSE

    }else{

      var <- input$varESTRATOS
      even <- FALSE

      if(var != "") {

        even <- sum(is.na(datos()[[var]] > 0)) > 0

        shinyFeedback::feedbackWarning("varESTRATOS", even, "La variable contiene NAs!")

        even

        # sum(es_prop_subpop$es_prop_subpop) == nrow(es_prop_subpop)

      }else{even}
    }
  })

  #### resume warnings ###----

  warning_resum = reactive({
    any(wrn_var_int(),wrn_var_denom(),
        wrn_var_subpop(),wrn_var_factexp(),wrn_var_conglom(),wrn_var_strata())
  })

  #    output$wrn_var_subpop <- renderText(wrn_var_subpop())

  #### MODAL warnings cálculos ####
  observeEvent(input$actionTAB,{
    # tiene que haber warning para que se muestre
    req(warning_resum())
    shinyalert("¡Error!", "¡Considere las advertencias!", type = "error")
  })

  observeEvent(input$actionTAB,{
    # tiene que haber warning para que se muestre
    req(input$varINTERES == "")
    shinyalert("¡Error!", "¡Debe seleccionar una variable de interés!", type = "error")
  })

  observeEvent(input$actionTAB,{
    # tiene que haber warning para que se muestre
    req(input$varCONGLOM == "" || input$varESTRATOS == "" || input$varFACT1 == "")
    shinyalert("¡Error!", "¡Faltan variables del diseño complejo!", type = "error")
  })

  # r <- reactiveValues(NULL)

  # observe({
  #   req(warning_resum(), input$actionTAB)
  #   # Updates goButton's label and icon
  #
  #    input$actionTAB <- r
  #
  # })}

  ##### * Pruebas de outputs * ####
}

