
context("test-evaluate_external")


# Diseños muestrales
options(survey.lonely.psu = "certainty")

dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat,
                        data = epf_personas %>% dplyr::group_by(folio) %>% dplyr::slice(1),
                        weights = ~fe)

dc_ene <- survey::svydesign(ids = ~conglomerado, strata = ~estrato_unico, data = ene %>%
                              dplyr::mutate(mujer = dplyr::if_else(sexo == 2, 1, 0),
                                            hombre = dplyr::if_else(sexo == 1, 1, 0),
                                            desocupado = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0)
                                            ),
                            weights = ~fact_cal)



############
# EVALUATE #
############


# INE Chile Standard for mean
test <-  create_mean("gastot_hd", dominios =  "zona+sexo+ecivil", disenio = dc, deff = T, ess = T)
test2 <- evaluate(test)
test2 <- evaluate(test, publish = T)


# INE Chile Standard for proportion
test <-  create_prop("desocupado", dominios =  "region", disenio = dc_ene, deff = T, ess = T, log_cv = T)
test2 <- evaluate(test)



