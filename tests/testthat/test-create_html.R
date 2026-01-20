context("test-create_html")


################################
# DECLARAR DISEÑOS Y DATOS     #
################################

options(survey.lonely.psu = "certainty")

# Diseño EPF
dc <- survey::svydesign(ids = ~varunit,
                        data = epf_personas %>%
                          dplyr::group_by(folio) %>%
                          dplyr::slice(1) %>%
                          dplyr::ungroup() %>%
                          dplyr::mutate(
                            metro = dplyr::if_else(zona == 1, 1, 0),
                            metro_na = dplyr::if_else(dplyr::row_number() <= 10, NA_real_, metro )),
                        strata = ~varstrat,
                        weights = ~fe)

# Diseño ENE
ene_mod <- ene %>%
  dplyr::mutate(desocupado = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0))

dc_ene <- survey::svydesign(ids = ~conglomerado,
                            strata = ~estrato_unico,
                            data = ene_mod,
                            weights = ~fact_cal)

########################
#  Estándar INE        #
########################

test_that("create_html genera tabla correcta para output estándar INE", {

  est_ine <- create_mean("gastot_hd", domains = "zona", design = dc)

  # CASO 1: Evaluación Normal (Esperamos que sea confiable/verde)
  eval_ine_ok <- assess(est_ine)
  html_ok <- create_html(eval_ine_ok)

  expect_true(inherits(html_ok, "knitr_kable"))
  expect_match(as.character(html_ok), "background-color: green")

  # CASO 2: Forzamos fallo NATURALMENTE (Sin hardcodear datos)
  # Le pedimos un n absurdo (10 millones) para que assess declare "insufficient"
  eval_ine_bad <- assess(est_ine, n = 10000000)
  html_bad <- create_html(eval_ine_bad)

  # Verificamos que el reporte haya pintado el n de rojo al detectar la insuficiencia
  expect_match(as.character(html_bad), "color: red")
})

##########################
# Estándar CEPAL 2020    #
##########################

test_that("create_html genera tabla correcta para output CEPAL 2020", {

  est_cepal <- create_prop("desocupado", domains = "region", design = dc_ene, eclac_input = TRUE)
  eval_cepal20 <- assess(est_cepal, scheme = "eclac_2020")

  html_output <- create_html(eval_cepal20)
  html_str <- as.character(html_output)

  expect_true(inherits(html_output, "knitr_kable"))


  expect_match(html_str, "background-color: green")       # 15 publish
  expect_match(html_str, "background-color: red")         # 1 supress
  expect_no_match(html_str, "background-color: yellow")   # 0 review


})

##########################
# Estándar CEPAL 2023    #
##########################

test_that("create_html genera tabla correcta para output CEPAL 2023", {

  est_cepal <- create_prop("desocupado", domains = "region", design = dc_ene, eclac_input = TRUE)
  eval_cepal23 <- assess(est_cepal, scheme = "eclac_2023")

  html_output <- create_html(eval_cepal23)
  html_str <- as.character(html_output)

  expect_match(html_str, "background-color: yellow")    # 1 weakly
  expect_no_match(html_str, "background-color: red")    # 0 supress
  expect_match(html_str, "table-striped")

})

###########################
#  Estándar Económicas    #
###########################

test_that("create_html genera tabla correcta para Encuestas Económicas", {

  dc_ele <- survey::svydesign(ids = ~rol_ficticio,
                              weights = ~fe_transversal,
                              strata = ~estrato,
                              fpc = ~pob,
                              data = ELE7)

  prod_salarial <- suppressWarnings(
    create_prop('VA_2022f',
                denominator = 'REMP_TOTAL',
                domains = 'cod_actividad+cod_tamano',
                design = dc_ele)
  )


  table_n_obj <- ELE7_n_obj %>%
    dplyr::mutate(cod_actividad = cod_actividad_letra,
                  cod_tamano = as.character(cod_tamano)) %>%
    dplyr::select(-cod_actividad_letra)

  eval_ratio <- assess(prod_salarial,
                       scheme = 'chile_economics',
                       domain_info = TRUE,
                       table_n_obj = table_n_obj,
                       ratio_between_0_1 = FALSE)

  html_output <- create_html(eval_ratio)
  html_str <- as.character(html_output)

  expect_match(html_str, "background-color: green")   # 11 non
  expect_match(html_str, "background-color: yellow")  # 38 reliable
  expect_match(html_str, "background-color: red")     # 10 reliable

})

