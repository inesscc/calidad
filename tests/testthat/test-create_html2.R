context("test-create_html")

library(dplyr)
library(survey)
library(kableExtra)

###################################
# 1. DECLARAR DISEÑOS Y MUESTRAs ##
###################################

options(survey.lonely.psu = "certainty")

# --- Diseño EPF (para hogares) ---
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

# --- Diseño ENE (Personas y Empleo) ---
ene_mod <- ene %>%
  dplyr::mutate(desocupado = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0))

dc_ene <- survey::svydesign(ids = ~conglomerado,
                            strata = ~estrato_unico,
                            data = ene_mod,
                            weights = ~fact_cal)

###################################
# 2. TEST: Estándar INE|||||||||||#
###################################

test_that("create_html genera tabla correcta para output estándar INE", {

  est_ine <- create_mean("gastot_hd", domains = "zona", design = dc)
  eval_ine <- assess(est_ine)
  eval_ine$n[1] <- 50 # Para forzar la alerta n < 60

  html_output <- create_html(eval_ine)
  html_str <- as.character(html_output)

  expect_true(inherits(html_output, "knitr_kable"))
  expect_match(html_str, "background-color: green")
  expect_match(html_str, "color: red")

  # Validar formato
  val_real <- eval_ine$stat[2]
  val_fmt <- trimws(format(round(val_real, 0), big.mark = ".", decimal.mark = ",", scientific = F))
  expect_match(html_str, val_fmt, fixed = TRUE)
})

###################################
# 3. TEST: Estándar CEPAL 2020    #
###################################

test_that("create_html genera tabla correcta para output CEPAL 2020", {

  est_cepal <- create_prop("desocupado", domains = "region", design = dc_ene, eclac_input = TRUE, log_cv = TRUE)
  eval_cepal20 <- assess(est_cepal, scheme = "eclac_2020")

  html_output <- create_html(eval_cepal20)
  html_str <- as.character(html_output)

  expect_true(inherits(html_output, "knitr_kable"))

  if("publish" %in% eval_cepal20$label){
    expect_match(html_str, "background-color: green")
  }
})

###################################
# 4. TEST: Estándar CEPAL 2023    #
#ESTO REALMENTE ES SUPER PARECIDO #
###################################

test_that("create_html genera tabla correcta para output CEPAL 2023", {

  est_cepal <- create_prop("desocupado", domains = "region", design = dc_ene, eclac_input = TRUE, log_cv = TRUE)
  eval_cepal23 <- assess(est_cepal, scheme = "eclac_2023")

  html_output <- create_html(eval_cepal23)
  html_str <- as.character(html_output)

  if("weakly-reliable" %in% eval_cepal23$label){
    expect_match(html_str, "background-color: yellow")
  }
  expect_match(html_str, "table-striped")
})

###################################
# 5. TEST: Estándar Económico     #
###################################

test_that("create_html genera tabla correcta para Encuestas Económicas con datos reales (ELE7)", {

  dc_ele <- survey::svydesign(ids = ~rol_ficticio,
                              weights = ~fe_transversal,
                              strata = ~estrato,
                              fpc = ~pob,
                              data = ELE7)

  # Usamos suppressWarnings para evitar ruido en el test por temas de diseño muestral
  #ESTO LO HICE PERO CREO QUE HAY QUE REVISARLO AAA

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

  # --- GENERACIÓN del HTML ---

  html_output <- create_html(eval_ratio)
  html_str <- as.character(html_output)

  # --- VALIDACIONES VARIAS---

  expect_true(inherits(html_output, "knitr_kable"))

  # Validar Colores
  if("publish" %in% eval_ratio$label){
    expect_match(html_str, "background-color: green")
  }
  if("review" %in% eval_ratio$label){
    expect_match(html_str, "background-color: yellow")
  }
  if("supress" %in% eval_ratio$label){
    expect_match(html_str, "background-color: red")
  }

  # Validar Formato Numérico
  val_real <- eval_ratio$stat[1]
  val_fmt <- trimws(format(round(val_real, 2), big.mark = ".", decimal.mark = ",", scientific = FALSE))
  expect_match(html_str, val_fmt, fixed = TRUE)
})
