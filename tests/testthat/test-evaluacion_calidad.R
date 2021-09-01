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


umbrales <- list( n = 60, cv_lower_ine = 0.15, cv_upper_ine = 0.3 )

###############
# evaluate_mean
###############


test <-  create_mean(gastot_hd, dominios =  zona+sexo+ecivil, disenio = dc)
eval <-  evaluate_mean(test, publicar = T, threshold = umbrales)

test_that("tramo porcentaje correcto", {
  expect_equal(eval$eval_cv[1], "cv <= 5")
})

# Probar parámetro de cepal
test <-  create_mean(gastot_hd, dominios =  zona+sexo+ecivil, disenio = dc, ess = T, deff = T, unweighted = T)
eval <-  evaluate_mean(test, publicar = T, threshold = umbrales, scheme = "cepal")

test <-  create_mean(gastot_hd, disenio = dc, ess = T, deff = T, unweighted = T)
eval <-  evaluate_mean(test, scheme = "cepal")

###############
# evaluate_tot
###############

test <-  create_tot(ocupado, dominios =  zona+sexo+ecivil, disenio = dc)
eval <-  evaluate_tot(test, publicar = T, threshold = umbrales)

test_that("Insumo media zona", {
  expect_equal(eval$eval_n[1], "n insuficiente")
})


# Probar parámetro de cepal
test <-  create_tot(ocupado, dominios =  zona+sexo+ecivil, disenio = dc, ess = T, deff = T)
expect_error(evaluate_tot(test, publicar = T, threshold = umbrales, scheme = "cepal"), "unweighted and ess must be used!")

##################
# evaluate_tot_con
##################
test <- create_tot_con(gastot_hd, dominios =zona, disenio = dc, deff = T, ess = T, unweighted = T)
eval <- evaluate_tot_con(test, scheme = "cepal")



###############
# evaluate_prop
###############

# Probar con la opción denomiador
test <-  create_prop(mujer, denominador = hombre, disenio = dc_ene, rel_error = T)
eval <- evaluate_prop(test)



test <-  create_prop(ocupado, dominios =  zona+sexo+ecivil, disenio = dc, deff = F)
eval <-  evaluate_prop(test, publicar = T, threshold = umbrales)
eval <-  evaluate_prop(test, publicar = T)

# Probar parámetro de cepal
test <-  create_prop(ocupado, dominios =  zona+sexo+ecivil, disenio = dc, deff = T, ess = T, log_cv = T, unweighted = T )
eval <-  evaluate_prop(test, publicar = T, threshold = umbrales, scheme = "cepal")



###############
# evaluate_median
###############

test <-  create_median(gastot_hd, dominios =  zona+sexo, disenio = dc, replicas = 15, seed = 1234)
eval <-  evaluate_median(test, publicar = T)

