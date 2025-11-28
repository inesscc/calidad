context("test-create_html")

###################################
# 1. DECLARAR DISEÑOS Y DATOS     #
###################################

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

###################################
# 2. TEST: Estándar INE           #
###################################

test_that("create_html genera tabla correcta para output estándar INE", {

  est_ine <- create_mean("gastot_hd", domains = "zona", design = dc)
  eval_ine <- assess(est_ine)
  # Forzar n bajo para probar color rojo en columna n
  eval_ine$n[1] <- 50
  # Forzar etiqueta eval_n para que coincida con la lógica
  eval_ine$eval_n[1] <- "insufficient sample size"

  html_output <- create_html(eval_ine)
  html_str <- as.character(html_output)

  testthat::expect_true(inherits(html_output, "knitr_kable"))
  testthat::expect_match(html_str, "background-color: green")
  testthat::expect_match(html_str, "color: red")
})

###################################
# 3. TEST: Estándar CEPAL 2020    #
###################################

test_that("create_html genera tabla correcta para output CEPAL 2020", {

  est_cepal <- create_prop("desocupado", domains = "region", design = dc_ene, eclac_input = TRUE, log_cv = TRUE)
  eval_cepal20 <- assess(est_cepal, scheme = "eclac_2020")

  # El objeto eval_cepal20 viene con "publish", "review", etc.
  html_output <- create_html(eval_cepal20)
  html_str <- as.character(html_output)

  testthat::expect_true(inherits(html_output, "knitr_kable"))

  # Validamos que el HTML tenga los colores correctos según la etiqueta original
  if ("publish" %in% eval_cepal20$label) {
    testthat::expect_match(html_str, "background-color: green")
  }

  if ("review" %in% eval_cepal20$label) {
    testthat::expect_match(html_str, "background-color: yellow")
  }

  if ("supress" %in% eval_cepal20$label) {
    testthat::expect_match(html_str, "background-color: red")
  }
})

###################################
# 4. TEST: Estándar CEPAL 2023    #
###################################

test_that("create_html genera tabla correcta para output CEPAL 2023", {

  est_cepal <- create_prop("desocupado", domains = "region", design = dc_ene, eclac_input = TRUE, log_cv = TRUE)
  eval_cepal23 <- assess(est_cepal, scheme = "eclac_2023")

  html_output <- create_html(eval_cepal23)
  html_str <- as.character(html_output)

  # Validamos usando la etiqueta original "weakly-reliable" (con guion)
  # sabiendo que el reporte la limpia y pinta amarillo.
  if ("weakly-reliable" %in% eval_cepal23$label) {
    testthat::expect_match(html_str, "background-color: yellow")
  }

  testthat::expect_match(html_str, "table-striped")
})

###################################
# 5. TEST: Estándar Económico     #
###################################

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

  if ("reliable" %in% eval_ratio$label) {
    testthat::expect_match(html_str, "background-color: green")
  }

  if ("weakly reliable" %in% eval_ratio$label) {
    testthat::expect_match(html_str, "background-color: yellow")
  }

  if ("non-reliable" %in% eval_ratio$label) {
    testthat::expect_match(html_str, "background-color: red")
  }
})
