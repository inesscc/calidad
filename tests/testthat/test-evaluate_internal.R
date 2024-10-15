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

# Defaults params
default_params_ine = list(df = 9, n = 60, cv_lower_ine = 0.15, cv_upper_ine = 0.3 )
default_params_cepal2020 = list(df = 9, n = 100, cv_cepal = 0.2, ess = 140, unweighted = 50, log_cv = 0.175)
default_params_cepal2023 <- list(df = 9, n = 100, cv_lower_cepal = 0.2, cv_upper_cepal = 0.3, ess = 60, cvlog_max = 0.175, CCNP_b = 50, CCNP_a = 30)

test_that("assess_ine works for mean", {
  test <- create_mean("gastot_hd", domains = "zona+sexo+ecivil", design = dc)
  evaluation <- assess_ine(test, params = default_params_ine, class(test))
  expect_true("label" %in% colnames(evaluation))
  expect_equal(sum(evaluation$label == 'non-reliable'), 4)
})

test_that("assess_ine works for proportion", {
  test <- create_prop("desocupado", domains = "region", design = dc_ene)
  evaluation <- assess_ine(test, params = default_params_ine, class(test))
  expect_true("label" %in% colnames(evaluation))
  expect_equal(sum(evaluation$label == 'reliable'), 16)
})

test_that("assess_cepal2020 works for mean", {
  test <- create_mean("gastot_hd", domains = "zona+sexo+ecivil", design = dc, eclac_input = T)
  evaluation <- assess_cepal2020(test, params = default_params_cepal2020, class = class(test))
  expect_equal(sum(evaluation$label == 'publish'), 10)
})

test_that("assess_cepal2020 works for proportion", {
  test <- create_prop("desocupado", domains = "region", design = dc_ene, eclac_input = TRUE, log_cv = TRUE)
  evaluation <- assess_cepal2020(test, params = default_params_cepal2020, class = class(test))
  expect_true("label" %in% colnames(evaluation))
  expect_equal(sum(evaluation$label == 'supress'), 1)
})

test_that("assess_cepal2023 works for mean", {
  test <- create_mean("gastot_hd", domains = "zona+sexo+ecivil", design = dc, eclac_input = TRUE)
  evaluation <- assess_cepal2023(test, params = default_params_cepal2023, class = class(test))
  expect_equal(sum(evaluation$label == 'non-reliable'), 5)
})

test_that("assess_cepal2023 works for proportion", {
  test <- create_prop("desocupado", domains = "region", design = dc_ene, eclac_input = TRUE)
  evaluation <- assess_cepal2023(test, params = default_params_cepal2023, class = class(test))
  expect_equal(sum(evaluation$label == 'weakly-reliable'), 1)
})

