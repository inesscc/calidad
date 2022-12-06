context("test-assess_internal")

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




##############
# assess INE
##############


# Defaults params for INE Chile for mean
default_params_ine = list(df = 9, n = 60, cv_lower_ine = 0.15, cv_upper_ine = 0.3 )
test <-  create_mean("gastot_hd", domains =  "zona+sexo+ecivil", design = dc)
evaluation <- assess_ine(test, params = default_params_ine, class(test))


# Defaults params for INE Chile for proportion
test <-  create_prop("desocupado", domains =  "region", design = dc_ene, deff = T, ess = T)
evaluation <- assess_ine(test, params = default_params_ine, class(test))


#################
# assess CEPAL
#################
default_params_cepal = list(df = 9, n = 100, cv_cepal = 0.2, ess = 140, unweighted = 50, log_cv = 0.175)
test <-  create_mean("gastot_hd", domains =  "zona+sexo+ecivil", design = dc, deff = T, ess = T, unweighted = T)
evaluation <- assess_cepal(test, params = default_params_cepal, class = class(test))


# Defaults params for cepal: proportion case
test <-  create_prop("desocupado", domains =  "region", design = dc_ene, deff = T, ess = T, unweighted = T, log_cv = T)
evaluation <- assess_cepal(test, params = default_params_cepal, class(test))





####################
# PUBLISH INE TABLE
####################

#x <- publish_table(evaluation)



