# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

dc <- survey::svydesign(ids = ~varunit,
                        data = calidad::epf_personas,
                        strata = ~varstrat,
                        weights = ~fe)

dc_enusc <- survey::svydesign(ids = ~Conglomerado,strata = ~VarStrat,weights = ~Fact_Pers,data = calidad::enusc)

options(survey.lonely.psu = "certainty")

###############################
########### MEDIA #############
###############################

### probamos medias - enfoque cepal

test_that("probamos medias - enfoque cepal",

  expect_equivalent(create_tabulado(calidad::epf_personas,
                  v_interes = "gastot_hd",
                  v_cruce = NULL,
                  denominador = NULL,
                  v_subpob = NULL,
                  v_fexp1 = "fe",
                  v_conglom = "varunit",
                  v_estratos = "varstrat",
                  tipoCALCULO = "Media",
                  ajuste_ene = FALSE,
                  ci = FALSE,
                  scheme = "cepal"), calidad::evaluate(calidad::create_mean(var = "gastot_hd",design = dc,eclac_input = T),scheme = "cepal")%>%
                  mutate(label = dplyr::case_when(label == "publish" ~ "Publicar",
                                                  label == "review" ~ "Revisar",
                                                  label == "supress" ~ "Suprimir")) %>%
                    rename(calidad = label))

)

### probamos medias - enfoque cepal - dominio

test_that("probamos medias - enfoque cepal - dominio ",
expect_equal(create_tabulado(calidad::epf_personas,
                v_interes = "gastot_hd",
                v_cruce = "zona",
                denominador = NULL,
                v_subpob = NULL,
                v_fexp1 = "fe",
                v_conglom = "varunit",
                v_estratos = "varstrat",
                tipoCALCULO = "Media",
                ajuste_ene = FALSE,
                ci = FALSE,
                scheme = "cepal"),calidad::evaluate(calidad::create_mean(var = "gastot_hd", domains = "zona",design = dc,eclac_input = T),scheme = "cepal")%>%
               mutate(label = dplyr::case_when(label == "publish" ~ "Publicar",
                                               label == "review" ~ "Revisar",
                                               label == "supress" ~ "Suprimir")) %>%
               rename(calidad = label)))

### probamos medias - enfoque cepal - 2 dominios

test_that("probamos medias - enfoque cepal - 2 dominios",
expect_equal(create_tabulado(calidad::epf_personas,
                v_interes = "gastot_hd",
                v_cruce = "zona+sexo",
                denominador = NULL,
                v_subpob = NULL,
                v_fexp1 = "fe",
                v_conglom = "varunit",
                v_estratos = "varstrat",
                tipoCALCULO = "Media",
                ajuste_ene = FALSE,
                ci = FALSE,
                scheme = "cepal"),calidad::evaluate(calidad::create_mean(var = "gastot_hd", domains = "zona+sexo",design = dc,eclac_input = T),scheme = "cepal") %>%
               mutate(label = dplyr::case_when(label == "publish" ~ "Publicar",
                                               label == "review" ~ "Revisar",
                                               label == "supress" ~ "Suprimir")) %>%
               rename(calidad = label)))


### probamos medias - enfoque chile

test_that("probamos medias - enfoque chile",
expect_equal(create_tabulado(calidad::epf_personas,
                v_interes = "gastot_hd",
                v_cruce = NULL,
                denominador = NULL,
                v_subpob = NULL,
                v_fexp1 = "fe",
                v_conglom = "varunit",
                v_estratos = "varstrat",
                tipoCALCULO = "Media",
                ajuste_ene = FALSE,
                ci = FALSE,
                scheme = "chile"),calidad::evaluate(calidad::create_mean(var = "gastot_hd",design = dc),publish = T,scheme = "chile")%>%
                rename(calidad = label)))

### probamos medias - enfoque chile - dominio
test_that("probamos medias - enfoque chile - dominio",
expect_equal(create_tabulado(calidad::epf_personas,
                v_interes = "gastot_hd",
                v_cruce = "zona",
                denominador = NULL,
                v_subpob = NULL,
                v_fexp1 = "fe",
                v_conglom = "varunit",
                v_estratos = "varstrat",
                tipoCALCULO = "Media",
                ajuste_ene = FALSE,
                ci = FALSE,
                scheme = "chile"),calidad::evaluate(calidad::create_mean(var = "gastot_hd",domains = "zona",design = dc),publish = T,scheme = "chile")%>%
  rename(calidad = label)))

### probamos medias - enfoque chile - 2 dominios
test_that("probamos medias - enfoque chile - 2 dominios",
expect_equal(create_tabulado(calidad::epf_personas,
                v_interes = "gastot_hd",
                v_cruce = "zona+sexo",
                denominador = NULL,
                v_subpob = NULL,
                v_fexp1 = "fe",
                v_conglom = "varunit",
                v_estratos = "varstrat",
                tipoCALCULO = "Media",
                ajuste_ene = FALSE,
                ci = FALSE,
                scheme = "chile"),calidad::evaluate(calidad::create_mean(var = "gastot_hd",domains = "zona+sexo",design = dc),publish = T,scheme = "chile")%>%
               rename(calidad = label)))


######################
#### PROPORCIONES ####
######################

### probamos proporciones - enfoque cepal
test_that("probamos proporciones - enfoque cepal",
expect_equal(create_tabulado(calidad::enusc,
                v_interes = "VP_DC",
                v_cruce = NULL,
                denominador = NULL,
                v_subpob = NULL,
                v_fexp1 = "Fact_Pers",
                v_conglom = "Conglomerado",
                v_estratos = "VarStrat",
                tipoCALCULO = "Proporción",
                ajuste_ene = FALSE,
                ci = FALSE,
                scheme = "cepal"),calidad::evaluate(calidad::create_prop(var = "VP_DC",design = dc_enusc,eclac_input = T),publish = T,scheme = "cepal") %>%
  mutate(label = dplyr::case_when(label == "publish" ~ "Publicar",
                                  label == "review" ~ "Revisar",
                                  label == "supress" ~ "Suprimir")) %>%
  rename(calidad = label)))

### probamos proporciones - enfoque chile
test_that("probamos proporciones - enfoque chile",
expect_equal(create_tabulado(calidad::enusc,
                v_interes = "VP_DC",
                v_cruce = NULL,
                denominador = NULL,
                v_subpob = NULL,
                v_fexp1 = "Fact_Pers",
                v_conglom = "Conglomerado",
                v_estratos = "VarStrat",
                tipoCALCULO = "Proporción",
                ajuste_ene = FALSE,
                ci = FALSE,
                scheme = "chile"),calidad::evaluate(calidad::create_prop(var = "VP_DC",design = dc_enusc),publish = T,scheme = "chile") %>%
                rename(calidad = label)))

### probamos proporciones - enfoque cepal - dominio
test_that("probamos proporciones - enfoque cepal - dominio",
expect_equal(create_tabulado(calidad::enusc,
                v_interes = "VP_DC",
                v_cruce = "enc_region",
                denominador = NULL,
                v_subpob = NULL,
                v_fexp1 = "Fact_Pers",
                v_conglom = "Conglomerado",
                v_estratos = "VarStrat",
                tipoCALCULO = "Proporción",
                ajuste_ene = FALSE,
                ci = FALSE,
                scheme = "cepal"),calidad::evaluate(calidad::create_prop(var = "VP_DC",domains = "enc_region",design = dc_enusc,eclac_input = T),publish = T,scheme = "cepal") %>%
               mutate(label = dplyr::case_when(label == "publish" ~ "Publicar",
                                               label == "review" ~ "Revisar",
                                               label == "supress" ~ "Suprimir")) %>%
               rename(calidad = label)))

### probamos proporciones - enfoque chile - dominio
test_that("probamos proporciones - enfoque chile - dominio",
expect_equal(create_tabulado(calidad::enusc,
                v_interes = "VP_DC",
                v_cruce = "enc_region",
                denominador = NULL,
                v_subpob = NULL,
                v_fexp1 = "Fact_Pers",
                v_conglom = "Conglomerado",
                v_estratos = "VarStrat",
                tipoCALCULO = "Proporción",
                ajuste_ene = FALSE,
                ci = FALSE,
                scheme = "chile"),calidad::evaluate(calidad::create_prop(var = "VP_DC",domains = "enc_region",design = dc_enusc),publish = T,scheme = "chile") %>%
               rename(calidad = label)))


### probamos proporciones - enfoque cepal - 2 dominios
test_that("probamos proporciones - enfoque cepal - 2 dominios",
expect_equal(create_tabulado(calidad::enusc,
                v_interes = "VP_DC",
                v_cruce = "enc_region+rph_sexo",
                denominador = NULL,
                v_subpob = NULL,
                v_fexp1 = "Fact_Pers",
                v_conglom = "Conglomerado",
                v_estratos = "VarStrat",
                tipoCALCULO = "Proporción",
                ajuste_ene = FALSE,
                ci = FALSE,
                scheme = "cepal"),calidad::evaluate(calidad::create_prop(var = "VP_DC",domains = "enc_region+rph_sexo",design = dc_enusc,eclac_input = T),publish = T,scheme = "cepal") %>%
  mutate(label = dplyr::case_when(label == "publish" ~ "Publicar",
                                  label == "review" ~ "Revisar",
                                  label == "supress" ~ "Suprimir")) %>%
  rename(calidad = label)))


### probamos proporciones - enfoque chile - 2 dominios

test_that("probamos proporciones - enfoque chile - 2 dominio",
          expect_equal(create_tabulado(calidad::enusc,
                       v_interes = "VP_DC",
                       v_cruce = "enc_region+rph_sexo",
                       denominador = NULL,
                       v_subpob = NULL,
                       v_fexp1 = "Fact_Pers",
                       v_conglom = "Conglomerado",
                       v_estratos = "VarStrat",
                       tipoCALCULO = "Proporción",
                       ajuste_ene = FALSE,
                       ci = FALSE,
                       scheme = "chile"),calidad::evaluate(calidad::create_prop(var = "VP_DC",domains = "enc_region+rph_sexo",design = dc_enusc),publish = T,scheme = "chile") %>%
            rename(calidad = label)))

######################################
#### PROPORCIONES con denominador ####
######################################


### probamos proporciones - enfoque cepalc ------------------------------------------------------------------------------------------
#
# calidad::evaluate(calidad::create_prop(var = "muj_insg_micro",denominador = "hom_insg_micro",design = dc_enusc,eclac_input = T),scheme = "cepal")
#
# calidad::evaluate(calidad::create_prop(var = "muj_insg_micro",denominador = "hom_insg_micro",design = dc_enusc),publish = T,scheme = "chile") %>%
#   rename(calidad = label)
#
#
# create_tabulado(calidad::enusc,
#                 v_interes = "muj_insg_micro",
#                 v_cruce = NULL,
#                 denominador = "hom_insg_micro",
#                 v_subpob = NULL,
#                 v_fexp1 = "Fact_Pers",
#                 v_conglom = "Conglomerado",
#                 v_estratos = "VarStrat",
#                 tipoCALCULO = "Proporción",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "cepal")

### probamos proporciones con denominador - enfoque chile

test_that("probamos proporciones - enfoque chile",
expect_equal(create_tabulado(calidad::enusc,
                v_interes = "muj_insg_micro",
                v_cruce = NULL,
                denominador = "hom_insg_micro",
                v_subpob = NULL,
                v_fexp1 = "Fact_Pers",
                v_conglom = "Conglomerado",
                v_estratos = "VarStrat",
                tipoCALCULO = "Proporción",
                ajuste_ene = FALSE,
                ci = FALSE,
                  scheme = "chile"),calidad::evaluate(calidad::create_prop(var = "muj_insg_micro",denominador = "hom_insg_micro",design = dc_enusc),publish = T,scheme = "chile") %>%
               rename(calidad = label)))

### probamos proporciones - enfoque chile - dominio

test_that("probamos proporciones - enfoque chile",
 expect_equal(create_tabulado(calidad::enusc,
           v_interes = "muj_insg_micro",
           v_cruce =  "enc_region",
           denominador = "hom_insg_micro",
           v_subpob = NULL,
           v_fexp1 = "Fact_Pers",
           v_conglom = "Conglomerado",
           v_estratos = "VarStrat",
           tipoCALCULO = "Proporción",
           ajuste_ene = FALSE,
           ci = FALSE,
           scheme = "chile"),calidad::evaluate(calidad::create_prop(var = "muj_insg_micro",denominador = "hom_insg_micro",domains = "enc_region",design = dc_enusc),publish = T,scheme = "chile") %>%
           rename(calidad = label)))

# ### probamos proporciones - enfoque cepal - 2 dominios
#
# create_tabulado(calidad::enusc,
#                 v_interes = "VP_DC",
#                 v_cruce = "enc_region+rph_sexo",
#                 denominador = NULL,
#                 v_subpob = NULL,
#                 v_fexp1 = "Fact_Pers",
#                 v_conglom = "Conglomerado",
#                 v_estratos = "VarStrat",
#                 tipoCALCULO = "Proporción",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "cepal")
#
# ### probamos proporciones - enfoque chile - 2 dominios
#
# create_tabulado(calidad::enusc,
#                 v_interes = "VP_DC",
#                 v_cruce = "enc_region+rph_sexo",
#                 denominador = NULL,
#                 v_subpob = NULL,
#                 v_fexp1 = "Fact_Pers",
#                 v_conglom = "Conglomerado",
#                 v_estratos = "VarStrat",
#                 tipoCALCULO = "Proporción",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "chile")
#
#
# ######################
# #### suma totales ####
# ######################
#
#
# ### probamos suma totales - enfoque cepal
#
# create_tabulado(calidad::epf_personas,
#                 v_interes = "gastot_hd",
#                 v_cruce = NULL,
#                 denominador = NULL,
#                 v_subpob = NULL,
#                 v_fexp1 = "fe",
#                 v_conglom = "varunit",
#                 v_estratos = "varstrat",
#                 tipoCALCULO = "Suma variable Continua",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "cepal")
#
# ### probamos suma totales - enfoque chile
#
# create_tabulado(calidad::epf_personas,
#                 v_interes = "gastot_hd",
#                 v_cruce = NULL,
#                 denominador = NULL,
#                 v_subpob = NULL,
#                 v_fexp1 = "fe",
#                 v_conglom = "varunit",
#                 v_estratos = "varstrat",
#                 tipoCALCULO = "Suma variable Continua",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "chile")
#
# ### probamos suma totales - enfoque cepal - dominio
#
# create_tabulado(calidad::epf_personas,
#                 v_interes = "gastot_hd",
#                 v_cruce = "zona",
#                 denominador = NULL,
#                 v_subpob = NULL,
#                 v_fexp1 = "fe",
#                 v_conglom = "varunit",
#                 v_estratos = "varstrat",
#                 tipoCALCULO = "Suma variable Continua",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "cepal")
#
# ### probamos suma totales - enfoque chile - dominio
#
# create_tabulado(calidad::epf_personas,
#                 v_interes = "gastot_hd",
#                 v_cruce = "zona",
#                 denominador = NULL,
#                 v_subpob = NULL,
#                 v_fexp1 = "fe",
#                 v_conglom = "varunit",
#                 v_estratos = "varstrat",
#                 tipoCALCULO = "Suma variable Continua",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "chile")
#
#
# ### probamos suma totales - enfoque cepal - 2 dominios
#
# create_tabulado(calidad::epf_personas,
#                 v_interes = "gastot_hd",
#                 v_cruce = "zona+sexo",
#                 denominador = NULL,
#                 v_subpob = NULL,
#                 v_fexp1 = "fe",
#                 v_conglom = "varunit",
#                 v_estratos = "varstrat",
#                 tipoCALCULO = "Suma variable Continua",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "cepal")
#
# ### probamos suma totales - enfoque chile - 2 dominios
#
# create_tabulado(calidad::epf_personas,
#                 v_interes = "gastot_hd",
#                 v_cruce = "zona+sexo",
#                 denominador = NULL,
#                 v_subpob = NULL,
#                 v_fexp1 = "fe",
#                 v_conglom = "varunit",
#                 v_estratos = "varstrat",
#                 tipoCALCULO = "Suma variable Continua",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "chile")
#
#
# #########################
# #### conteo de casos ####
# #########################
#
#
# ### probamos conteo de casos - enfoque cepal
#
# create_tabulado(calidad::enusc,
#                 v_interes = "VP_DC",
#                 v_cruce = NULL,
#                 denominador = NULL,
#                 v_subpob = NULL,
#                 v_fexp1 = "Fact_Pers",
#                 v_conglom = "Conglomerado",
#                 v_estratos = "VarStrat",
#                 tipoCALCULO = "Conteo casos",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "cepal")
#
# ### probamos conteo de casos - enfoque chile
#
# create_tabulado(calidad::enusc,
#                 v_interes = "VP_DC",
#                 v_cruce = NULL,
#                 denominador = NULL,
#                 v_subpob = NULL,
#                 v_fexp1 = "Fact_Pers",
#                 v_conglom = "Conglomerado",
#                 v_estratos = "VarStrat",
#                 tipoCALCULO = "Conteo casos",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "chile")
#
# ### probamos conteo de casos - enfoque cepal - dominio
#
# create_tabulado(calidad::enusc,
#                 v_interes = "VP_DC",
#                 v_cruce = "enc_region",
#                 denominador = NULL,
#                 v_subpob = NULL,
#                 v_fexp1 = "Fact_Pers",
#                 v_conglom = "Conglomerado",
#                 v_estratos = "VarStrat",
#                 tipoCALCULO = "Conteo casos",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "cepal")
#
# ### probamos conteo de casos - enfoque chile - dominio
#
# create_tabulado(calidad::enusc,
#                 v_interes = "VP_DC",
#                 v_cruce = "enc_region",
#                 denominador = NULL,
#                 v_subpob = NULL,
#                 v_fexp1 = "Fact_Pers",
#                 v_conglom = "Conglomerado",
#                 v_estratos = "VarStrat",
#                 tipoCALCULO = "Conteo casos",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "chile")
#
# ### probamos conteo de casos - enfoque cepal - 2 dominios
#
# create_tabulado(calidad::enusc,
#                 v_interes = "VP_DC",
#                 v_cruce = "enc_region+rph_sexo",
#                 denominador = NULL,
#                 v_subpob = NULL,
#                 v_fexp1 = "Fact_Pers",
#                 v_conglom = "Conglomerado",
#                 v_estratos = "VarStrat",
#                 tipoCALCULO = "Conteo casos",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "cepal")
#
# ### probamos conteo de casos - enfoque chile - 2 dominios
#
# create_tabulado(calidad::enusc,
#                 v_interes = "VP_DC",
#                 v_cruce = "enc_region+rph_sexo",
#                 denominador = NULL,
#                 v_subpob = NULL,
#                 v_fexp1 = "Fact_Pers",
#                 v_conglom = "Conglomerado",
#                 v_estratos = "VarStrat",
#                 tipoCALCULO = "Conteo casos",
#                 ajuste_ene = FALSE,
#                 ci = FALSE,
#                 scheme = "chile")
#

#### testeo de etiquetas ####
test_that("testeo de etiquetas",
expect_equal(create_tabulado(shiny_calidad2::enusc_2020_etiq,
                v_interes = "VP_DC",
                v_cruce = c("enc_region"),
                denominador = NULL,
                v_subpob = "",
                v_fexp1 = "Fact_Pers",
                v_conglom = "Conglomerado",
                v_estratos = "VarStrat",
                tipoCALCULO = "Conteo casos",
                ajuste_ene = FALSE,
                ci = FALSE,
                etiquetas = TRUE,
                scheme = "chile")$enc_region,names(labelled::val_labels(shiny_calidad2::enusc_2020_etiq$enc_region))))


#### testeamos reactividad ####

server <- function(input, output, session){

  tabulado = reactive(create_tabulado(base = input$file,
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
                             ajuste_ene = FALSE))

  output$out <- renderText(paste0("Result: ", tabulado()))

}


testServer(server, {

  session$setInputs(file = calidad::enusc,
                    varINTERES = "VP_DC",
                    varDENOM = NULL,
                    varCRUCE = NULL,
                    varSUBPOB = NULL,
                    varFACT1 = "Fact_Pers",
                    varCONGLOM = "Conglomerado",
                    varESTRATOS = "VarStrat",
                    tipoCALCULO = "Conteo casos",
                    IC = FALSE,
                    SCHEME = "chile",
                    ETIQUETAS = FALSE)

  print(tabulado())

})

## testeamos reactividad de etiquetas #####


server <- function(input, output, session){
  etiquetas <- reactive(labelled::is.labelled(input$file[[input$varCRUCE[1]]]))
}


testServer(server, {

  session$setInputs(file = shiny_calidad2::enusc_2020_etiq,
                    varCRUCE = "enc_region")

  print(etiquetas())

})
