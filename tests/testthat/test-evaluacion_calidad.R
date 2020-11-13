context("test-evaluacion_calidad")

# Diseños muestrales
options(survey.lonely.psu = "certainty")

dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas %>% dplyr::group_by(folio) %>% dplyr::slice(1), weights = ~fe)

dc_ene <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = ene, weights = ~fe)




