

create_tabulado = function(base, v_interes, v_cruce,  v_subpob, v_fexp1, v_conglom, v_estratos, tipoCALCULO, ci, ajuste_ene,etiquetas=FALSE,denominador,scheme,server = T){


  # if(v_cruce[1] == "" & !is.null(v_cruce)){
  #   v_cruce <- NULL
  # }
  #
  # if(v_subpob[1] == ""){
  #   v_subpob <- NULL
  # }
  #
  # if(denominador[1] == "" & !is.null(denominador)){
  #   denominador <- NULL
  # }


  if(length(v_cruce)>1){
    v_cruce_string = paste0(v_cruce, collapse  = "+")
  }else{
    v_cruce_string = v_cruce
  }

  base[[v_interes]] = as.numeric(base[[v_interes]])
  base$unit =  as.numeric(base[[v_conglom]])
  base$varstrat =  as.numeric(base[[v_estratos]])
  base$fe =  as.numeric(base[[v_fexp1]])

  ### Diseño complejo
  dc <- survey::svydesign(ids = ~unit, strata = ~varstrat,
                          data =  base, weights = ~fe)

  options(survey.lonely.psu = "certainty")


  ### listas de funciones CALIDAD
  funciones_cal = list(calidad::create_mean, calidad::create_prop,
                       calidad::create_total, calidad::create_size)

  if(tipoCALCULO %in% "Media") {
    num = 1
  }else if(tipoCALCULO %in% "Proporción"){
    num = 2
  }else if(tipoCALCULO %in% "Suma variable Continua"){
    num = 3
  }else if(tipoCALCULO %in% "Conteo casos"){
    num = 4
  }

  if(scheme == "cepal"){
    eclac_input = TRUE
    log_cv= TRUE
  }else{
    eclac_input = FALSE
    log_cv= FALSE
  }

  if(tipoCALCULO == "Proporción"){
    if(!is.null(denominador)){

      evaluados = calidad::evaluate(funciones_cal[[num]](var = v_interes,
                                                         design = dc,
                                                         domains = v_cruce_string,
                                                         denominador = denominador,
                                                         eclac_input = eclac_input,
                                                         log_cv= log_cv)
                                    ,scheme = scheme,publish = T)
    }else{
      evaluados = calidad::evaluate(funciones_cal[[num]](var = v_interes,
                                                         design = dc,
                                                         domains = v_cruce_string,
                                                         eclac_input = eclac_input,
                                                         log_cv= log_cv),
                                    scheme = scheme,publish = T)
    }

  }else{
    evaluados = calidad::evaluate(funciones_cal[[num]](var = v_interes,
                                                       design = dc,
                                                       domains = v_cruce_string,
                                                       eclac_input = eclac_input),
                                  scheme = scheme,publish = T)
  }

  if(scheme == "cepal"){
    evaluados <- evaluados %>%
      mutate(label = dplyr::case_when(label == "publish" ~ "Publicar",
                                      label == "review" ~ "Revisar",
                                      label == "supress" ~ "Suprimir")) %>%
      rename(calidad = label)
  }else{
    evaluados <- evaluados %>%
      rename(calidad = label) %>% select(-c(publication,	pass))
  }

  #### opción de etiquetas #####
  if(etiquetas == TRUE && !is.null(v_cruce) && labelled::is.labelled(base[[v_cruce[1]]])){ #

    paste_labels = function(tabla, base, var_cruce){

      dt = data.frame(valor = labelled::val_labels(base[[var_cruce]]))
      dt = tibble::rownames_to_column(dt)

      tabla[[var_cruce]] =  unlist(lapply(tabla[[var_cruce]] ,function(x) as.character(dt$rowname[dt$valor == x])))
      tabla
    }

    ####  al hacer filtros se eliminan categorias, necesitamos sacar etiquetas de base filtrada

    if(!is.null(v_subpob)){
      datos2 = base[base[[v_subpob]] == 1,]
    }else{
      datos2 = base
    }

    #asignamos etiquetas
    for(i in v_cruce){
      evaluados = paste_labels(tabla = evaluados, base = datos2, var_cruce = i)
    }
  }

  evaluados
}










# create_tabulado = function(base, v_interes, v_cruce,  v_subpob, v_fexp1, v_conglom,  v_estratos, tipoCALCULO, ci, ajuste_ene,denominador,scheme,etiquetas ,server = T){
#   #create_Tabulado = function(base){
# #  print(ci)
# # base = base
# # v_interes =  input$varINTERES
# # v_cruce = v_cruce
# # v_subpob =  v_subpob
# # v_fexp1 = input$varFACT1
# # v_conglom = input$varCONGLOM
# # v_estratos = input$varESTRATOS
# # tipoCALCULO = input$tipoCALCULO
#
#   if(length(v_cruce)>1){
#     v_cruce_string = paste0(v_cruce, collapse  = "+")
#   }else{
#     v_cruce_string = v_cruce
#   }
#
#   base[[v_interes]] = as.numeric(base[[v_interes]])
#   base$unit =  as.numeric(base[[v_conglom]])
#   base$varstrat =  as.numeric(base[[v_estratos]])
#   base$fe =  as.numeric(base[[v_fexp1]])
#
#
#   ### Diseño complejo
#   dc <- svydesign(ids = ~unit, strata = ~varstrat,
#                   data =  base, weights = ~fe)
#
#   options(survey.lonely.psu = "certainty")
#
#
#   ### listas de funciones CALIDAD
#   log_cv = F
#   if(scheme == "chile"){
#
#     ess = F
#     deff = F
#     unweighted = F
#
#   funciones_cal = list(calidad::create_mean, calidad::create_prop,
#                        calidad::create_size, calidad::create_tot, calidad::create_median)
#
#   funciones_eval = list(calidad::evaluate_mean, calidad::evaluate_prop,
#                         calidad::evaluate_size, calidad::evaluate_tot, calidad::evaluate_median)
#
#   }else if(scheme == "cepal"){
#
#   ess = T
#   deff = T
#   unweighted = T
#
#   funciones_cal = list(calidad::create_mean, calidad::create_prop,
#                        calidad::create_tot_con, calidad::create_tot, calidad::create_median)
#
#   funciones_eval = list(calidad::evaluate_mean, calidad::evaluate_prop,
#                         calidad::evaluate_tot_con, calidad::evaluate_tot, calidad::evaluate_median)
#
#   }
#
#
#   if(tipoCALCULO %in% "Media") {
#     num = 1
#   }else if(tipoCALCULO %in% "Proporción"){
#     num = 2
#
#     if(scheme == "cepal"){
#       log_cv = T
#     }
#
#   }else if(tipoCALCULO %in% "Suma variable Continua"){
#     num = 3
#   }else if(tipoCALCULO %in% "Conteo casos"){
#     num = 4
#   }else if(tipoCALCULO %in% "Mediana"){
#     num = 5
#   }
#
# #print(log_cv)
# if(log_cv == T){
#
#   ### normal sin denominador ###
#   if(is.null(denominador)){
#
#     if(is.null(v_cruce) && v_subpob == "") {
#       insumos = funciones_cal[[num]](var = v_interes, disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
#       evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#
#     } else if (v_subpob == ""){
#       insumos = funciones_cal[[num]](var = v_interes,dominios = v_cruce_string ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
#       evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#
#     } else if (is.null(v_cruce)){
#       base[[v_subpob]] = as.numeric(base[[v_subpob]])
#       insumos = funciones_cal[[num]](var = v_interes,subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
#       evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#
#     } else {
#       base[[v_subpob]] = as.numeric(base[[v_subpob]])
#       insumos = funciones_cal[[num]](var = v_interes,dominios = v_cruce_string ,subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
#       evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#     }
#   }
#
#
#   if(!is.null(denominador)){
#     if(is.null(v_cruce) && v_subpob == "") {
#       insumos = funciones_cal[[num]](var = v_interes, denominador = denominador, disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
#       evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#
#     } else if (v_subpob == ""){
#       insumos = funciones_cal[[num]](var = v_interes,denominador = denominador, dominios = v_cruce_string ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
#       evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#
#     } else if (is.null(v_cruce)){
#       base[[v_subpob]] = as.numeric(base[[v_subpob]])
#       insumos = funciones_cal[[num]](var = v_interes ,denominador = denominador, subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
#       evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#
#     } else {
#       base[[v_subpob]] = as.numeric(base[[v_subpob]])
#       insumos = funciones_cal[[num]](var = v_interes, dominios = v_cruce_string, denominador = denominador, subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess,log_cv = log_cv, deff = deff,unweighted = unweighted)
#       evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#     }
#   }
#
#   if(scheme == "cepal"){
#     evaluados <- evaluados %>%
#       mutate(tag = dplyr::case_when(tag == "publish" ~ "Publicar",
#                                     tag == "review" ~ "Revisar",
#                                     tag == "supress" ~ "Suprimir")) %>%
#       rename(calidad = tag)
#   }
#
# }else{
#
#   ### normal sin denominador ###
#   if(is.null(denominador)){
#
#   if(is.null(v_cruce) && v_subpob == "") {
#     insumos = funciones_cal[[num]](var = v_interes, disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
#     evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#
#   } else if (v_subpob == ""){
#     insumos = funciones_cal[[num]](var = v_interes,dominios = v_cruce_string ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
#     evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#
#   } else if (is.null(v_cruce)){
#     base[[v_subpob]] = as.numeric(base[[v_subpob]])
#     insumos = funciones_cal[[num]](var = v_interes,subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
#     evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#
#   } else {
#     base[[v_subpob]] = as.numeric(base[[v_subpob]])
#     insumos = funciones_cal[[num]](var = v_interes,dominios = v_cruce_string ,subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
#     evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#   }
#   }
#
#
#   if(!is.null(denominador)){
#     if(is.null(v_cruce) && v_subpob == "") {
#       insumos = funciones_cal[[num]](var = v_interes, denominador = denominador, disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
#       evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#
#     } else if (v_subpob == ""){
#       insumos = funciones_cal[[num]](var = v_interes,denominador = denominador, dominios = v_cruce_string ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
#       evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#
#     } else if (is.null(v_cruce)){
#       base[[v_subpob]] = as.numeric(base[[v_subpob]])
#       insumos = funciones_cal[[num]](var = v_interes ,denominador = denominador, subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
#       evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#
#     } else {
#       base[[v_subpob]] = as.numeric(base[[v_subpob]])
#       insumos = funciones_cal[[num]](var = v_interes, dominios = v_cruce_string, denominador = denominador, subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T,ess = ess, deff = deff,unweighted = unweighted)
#       evaluados =  funciones_eval[[num]](insumos, publicar = TRUE,scheme = scheme)
#     }
#   }
#
#   if(scheme == "cepal"){
#   evaluados <- evaluados %>%
#     mutate(tag = dplyr::case_when(tag == "publish" ~ "Publicar",
#                                   tag == "review" ~ "Revisar",
#                                   tag == "supress" ~ "Suprimir")) %>%
#     rename(calidad = tag)
#   }
#
#   #### opción de etiquetas #####
#
#   if(etiquetas != FALSE && !is.null(v_cruce) && labelled::is.labelled(base[[v_cruce[1]]])){ #
#
#     paste_labels = function(tabla, base, var_cruce){
#
#       dt = data.frame(valor = labelled::val_labels(base[[var_cruce]]))
#       dt = tibble::rownames_to_column(dt)
#
#       tabla[[var_cruce]] =  unlist(lapply(tabla[[var_cruce]] ,function(x) as.character(dt$rowname[dt$valor == x])))
#       tabla
#     }
#
#     ####  al hacer filtros se eliminan categorias, necesitamos sacar etiquetas de base filtrada
#
#     if(v_subpob != ""){
#       datos2 = base[base[[v_subpob]] == 1,]
#     }else{
#       datos2 = base
#     }
#     for(i in v_cruce){
#       evaluados = paste_labels(tabla = evaluados, base = datos2, var_cruce = i)
#      }
#    }
#
#   evaluados
#
#   }
#
#  }
#

