context("test-evaluacion_calidad")

# Diseños muestrales
options(survey.lonely.psu = "certainty")

dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat,
                        data = epf_personas %>% dplyr::group_by(folio) %>% dplyr::slice(1),
                        weights = ~fe)

dc_ene <- survey::svydesign(ids = ~conglomerado, strata = ~estrato_unico, data = ene %>%
                              dplyr::mutate(mujer = dplyr::if_else(sexo == 2, 1, 0),
                                            hombre = dplyr::if_else(sexo == 1, 1, 0)),
                            weights = ~fact_cal)



###############
# evaluate_mean
###############


test <-  create_mean(gastot_hd, dominios =  zona+sexo+ecivil, disenio = dc, deff = T, ess = T)
eval <-  evaluate_mean(test, publicar = T, scheme = "chile", df = 10)
#eval <-  evaluate_mean(test, publicar = T, scheme = "cepal", df = 2)

test_that("tramo porcentaje correcto", {
  expect_equal(eval$eval_cv[2], "cv <= 0.15")
})

# Probar parámetro de cepal
test <-  create_mean(gastot_hd, dominios =  zona+sexo+ecivil, disenio = dc, ess = T, deff = T, unweighted = T)
eval <-  evaluate_mean(test, publicar = T,  scheme = "cepal")

test <-  create_mean(gastot_hd, disenio = dc, ess = T, deff = T, unweighted = T)
eval <-  evaluate_mean(test, scheme = "cepal")

###############
# evaluate_tot
###############

test <-  create_tot(ocupado, dominios =  zona+sexo+ecivil, disenio = dc)
eval <-  evaluate_tot(test, publicar = T)

test_that("Insumo media zona", {
  expect_equal(eval$eval_n[1], "n insuficiente")
})


# Probar parámetro de cepal
test <-  create_tot(ocupado, dominios =  zona+sexo+ecivil, disenio = dc, deff = T)
expect_error(evaluate_tot(test, publicar = T, scheme = "cepal"), "ess must be used!")

###############
# evaluate_size
###############

test <-  create_size(ocupado, dominios =  zona+sexo, disenio = dc, unweighted = T, deff = T, ess = T)
eval <-  evaluate_size(test, scheme = "cepal", df = 122, ess = 31)




##################
# evaluate_tot_con
##################
test <- create_tot_con(gastot_hd, dominios =zona, disenio = dc, deff = T, ess = T)
eval <- evaluate_tot_con(test, scheme = "cepal", n = 200, ess = 200)



###############
# evaluate_prop
###############

# Probar con la opción denomiador
test <-  create_prop(mujer, denominador = hombre, disenio = dc_ene, rel_error = T)
eval <- evaluate_prop(test)

test <-  create_prop(ocupado, dominios =  zona+sexo+ecivil, disenio = dc, deff = T,  log_cv = T, unweighted = T, ess = T )
eval <-  evaluate_prop(test, publicar = T, scheme = "cepal", n = 100)

# Probar parámetro de cepal
test <-  create_prop(ocupado, dominios =  zona+sexo+ecivil, disenio = dc, deff = T, ess = T, log_cv = T, unweighted = T )
eval <-  evaluate_prop(test, publicar = T, scheme = "cepal")
#View(eval)


###############
# evaluate_median
###############

test <-  create_median(gastot_hd, dominios =  zona+sexo+ecivil, disenio = dc, replicas = 15, seed = 1234)

test <-  create_prop(ocupado, dominios =  zona+sexo+ecivil, disenio = dc, deff = T, ess = T, log_cv = T, unweighted = T )
eval <-  evaluate_prop(test, publicar = T, scheme = "cepal", df = 9, n = 100, cv_cepal = 0.2, ess = 140, unweighted = 50, log_cv = 0.175)

test <-  create_mean(gastot_hd, dominios =  zona+sexo+ecivil, disenio = dc, deff = T, ess = T, unweighted = T )
eval <-  evaluate_mean(test, publicar = T, scheme = "cepal", df = 9, n = 100, cv_cepal = 0.2, ess = 140, unweighted = 50)

test <-  create_tot(ocupado, dominios =  zona+sexo+ecivil, disenio = dc, deff = T, ess = T, unweighted = T )
eval <-  evaluate_tot(test, publicar = T, scheme = "cepal", df = 9, n = 100, cv_cepal = 0.2, ess = 140, unweighted = 50, log_cv = 0.175)








