context("test-evaluacion_calidad")

# Diseños muestrales
options(survey.lonely.psu = "certainty")

dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat,
                        data = epf_personas %>% dplyr::group_by(folio) %>% dplyr::slice(1),
                        weights = ~fe)

dc_ene <- survey::svydesign(ids = ~conglomerado, strata = ~estrato_unico, data = ene, weights = ~fact_cal)


umbrales <- list(df = 9, n = 60, cv_lower = 5, cv_upper = 15)
###############
# evaluate_mean
###############


test <-  create_mean(gastot_hd, dominios =  zona+sexo+ecivil, disenio = dc)
eval <-  evaluate_mean(test, publicar = T, threshold = umbrales)

test_that("tramo porcentaje correcto", {
  expect_equal(eval$eval_cv[1], "cv <= 5")
})


###############
# evaluate_tot
###############

test <-  create_tot(ocupado, dominios =  zona+sexo+ecivil, disenio = dc)
eval <-  evaluate_tot(test, publicar = T, threshold = umbrales)



test_that("Insumo media zona", {
  expect_equal(eval$eval_n[1], "n insuficiente")
})


###############
# evaluate_prop
###############


test <-  create_prop(ocupado, dominios =  zona+sexo+ecivil, disenio = dc)
eval <-  evaluate_prop(test, publicar = T, threshold = umbrales)
eval <-  evaluate_prop(test, publicar = T)


