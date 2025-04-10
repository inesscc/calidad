context("test-assess_external")

# Diseños muestrales
options(survey.lonely.psu = "certainty")

dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat,
                        data = epf_personas %>% dplyr::group_by(folio) %>% dplyr::slice(1),
                        weights = ~fe)

dc_ene <- survey::svydesign(ids = ~conglomerado, strata = ~estrato_unico, data = ene %>%
                              dplyr::mutate(mujer = dplyr::if_else(sexo == 2, 1, 0),
                                            hombre = dplyr::if_else(sexo == 1, 1, 0),
                                            desocupado = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0),
                                            ocupado = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0)
                              ),
                            weights = ~fact_cal)

############
# assess #
############

# National level with denominator
expect_error(create_prop(var = "mujer", denominator = "hombre", design = dc_ene, eclac_input = T),
             "eclac approach is not allowed with denominator")

### eval scheme in ratios
test_that('test ratio scheme eclac_2020',
          expect_error(create_prop(var = "mujer", denominator = "hombre", design = dc_ene, eclac_input = T, scheme = 'eclac_2020'),
             "eclac approach is not allowed with denominator"))

test_that('test ratio scheme eclac_2023',
          expect_warning(create_prop(var = "mujer", denominator = "hombre", design = dc_ene, eclac_input = T, scheme = 'eclac_2023'))
          )


# INE Chile Standard for mean
test1 <- create_mean("gastot_hd", domains = "zona+sexo+ecivil", design = dc, deff = TRUE, ess = TRUE, unweighted = TRUE)
test <- assess(test1, publish = TRUE)

# INE Chile Standard for proportion
test2 <- create_prop("desocupado", domains = "region+sexo", design = dc_ene, eclac_input = TRUE)

x <- survey::svyby(~desocupado, by = ~region+sexo, design = dc_ene, FUN = survey::svymean)
test_cv <- survey::cv(x)

test_that("cv calculado correctamente", {
  expect_equal(sum(test2$cv == test_cv), length(test_cv))
})


## INE Chile standard for ratios over 1:
test_ratio <- create_prop(var = "mujer", denominator = "hombre", domains = "ocupado", design = dc_ene)

test_that("sin especificar ratio_between_0_1, cuando efectivamente es ratio>1", {  # warning
  expect_warning(assess(test_ratio))
})

test_that("especificando ratio_between_0_1", {
  expect_contains(names(assess(test_ratio, ratio_between_0_1 = FALSE)), 'eval_cv')
})

test_that("especificando ratio_between_0_1 por estimador", {
  expect_contains(names(assess(test_ratio %>% dplyr::filter(stat<1), ratio_between_0_1 = FALSE)), 'eval_cv')
})


## ECLAC standard for ratios over 1:

test_that('test log_cv for ratio over 1',
          expect_warning(create_prop(var = "mujer", denominator = "hombre", domains = "ocupado", design = dc_ene,
                                     eclac_input = TRUE, scheme = 'eclac_2023')))

expect_warning(test_ratio <- create_prop(var = "mujer", denominator = "hombre", domains = "ocupado", design = dc_ene,
                                         eclac_input = TRUE, scheme = 'eclac_2023'))


test_that("sin especificar ratio_between_0_1, cuando efectivamente es ratio>1", {  # warning
  expect_warning(assess(test_ratio, scheme = 'eclac_2023'))
})

test_that("especificando ratio_between_0_1", {
  expect_contains(names(assess(test_ratio, scheme = 'eclac_2023', ratio_between_0_1 = FALSE)), 'eval_cv')
})


test_that("especificando mal ratio_between_0_1", {
  expect_warning(assess(test_ratio, scheme = 'eclac_2023', ratio_between_0_1 = TRUE))
})

test_that('test con eclac_2020',
           expect_error(assess(test_ratio, scheme = 'eclac_2020', ratio_between_0_1 = FALSE)))


# INE Chile Standard for size
test3 <- create_size("desocupado", domains = "region", design = dc_ene)
test <- assess(test3, publish = TRUE)

test_that("Ningún valor de deff es infinito", {
  expect_equal(sum(test3$deff == Inf), 0)
})

# INE Chile Standard for total
test4 <- create_total("gastot_hd", domains = "zona", design = dc, deff = TRUE, ess = TRUE, unweighted = TRUE)
test_ine <- assess(test4, publish = TRUE)

# CEPAL 2020 standard with default parameters
test <- assess(test1, scheme = "eclac_2020")
test <- assess(test2, scheme = "eclac_2020")

test_that('ess must be used',
          expect_error(assess(test3, scheme = "eclac_2020")))

test <- assess(test4, scheme = "eclac_2020")

# Proportion without log_cv
test2_sin_log <- create_prop("desocupado", domains = "region+sexo", design = dc_ene, deff = TRUE, ess = TRUE, unweighted = TRUE )

### cepal 2020
test_that("log_cv must be used!",
          expect_error(assess(test2_sin_log, scheme = "eclac_2020"), "log_cv must be used!"))

eclac <- create_size("desocupado", domains = "region", design = dc_ene, eclac_input = T,
                     unweighted = TRUE, df_type = "eclac")

test <- assess(eclac, scheme = "eclac_2020", unweighted = 150)
#print(test$label)
test_that("se caigan estimaciones por conteo no ponderado en size", {
  expect_equal(sum(test$label == "supress"), 9)
})

# CEPAL 2020 standard with custom parameters
test <- assess(test1, scheme = "eclac_2020", unweighted = 500)
test <- assess(test1, scheme = "eclac_2020", ess = 200)
test <- assess(test2, scheme = "eclac_2020", ess = 200, df = 127)

test_that("NA in label variable", {
  expect_equal(sum(is.na(test$label) == FALSE), dim(test)[1])
})

# CEPAL_2023 standard with default parameters
test <- assess(test1, scheme = "eclac_2023")
test <- assess(test2, scheme = "eclac_2023")

test_that('ess must be used',
          expect_error(assess(test3, scheme = "eclac_2023")))

test <- assess(test4, scheme = "eclac_2023")

# Proportion without log_cv for CEPAL_2023
expect_error(assess(test2_sin_log, scheme = "eclac_2023"),
             "log_cv must be used!")

eclac_2023 <- create_size("desocupado", domains = "region", design = dc_ene, eclac_input = T,
                          unweighted = TRUE, df_type = "eclac")

test <- assess(eclac_2023, scheme = "eclac_2023", unweighted = 150)

#print(test$label)

test_that("se caigan estimaciones por conteo no ponderado en size para eclac_2023", {
  expect_equal(sum(test$label == "reliable"), 15)
})

# CEPAL_2023 standard with custom parameters
test <- assess(test1, scheme = "eclac_2023", unweighted = 500)
test <- assess(test1, scheme = "eclac_2023", ess = 200)
test <- assess(test2, scheme = "eclac_2023", ess = 200, df = 127)

test_that("NA in label variable para eclac_2023", {
  expect_equal(sum(is.na(test$label) == FALSE), dim(test)[1])
})

# html output
out1 <- create_html(test)
out2 <- create_html(test_ine)

######################

library(dplyr)
library(purrr)

# Creamos un dataframe de ejemplo

### como no tiene clase def realizara la evaluacion como proporcion
data <- data.frame(
  n = c(80, 150, 500, 120),
  df = c(10, 8, 8, 10),
  cv = c(0.12, 0.25, 0.35, 0.2),
  ess = c(70, 130, 45, 110),
  unweighted = c(60, 110, 40, 100),
  stat = c(0.4, 0.8, 0.93, 0.6),
  se = c(0.02, 0.03, 0.04, 0.025),
  deff = c(1.1, 0.9, 0.4, 1.2),
  log_cv = c(0.05, 0.07, 0.09, 0.06)
)


params_ine = list(df = 9, n = 60, cv_lower_ine = 0.15, cv_upper_ine = 0.3 )
params_cepal2020 = list(df = 9, n = 100, cv_cepal = 0.2, ess = 140, unweighted = 50, log_cv = 0.175)
params_cepal2023 <- list(df = 9, n = 100, cv_lower_cepal = 0.2, cv_upper_cepal = 0.3, ess = 60, cvlog_max = 0.175, CCNP_b = 50, CCNP_a = 30)


# Función a testear
evaluate <- function(data, params_ine, params_cepal2020, params_cepal2023, indicator_type) {
  if (indicator_type %in% c("mean", "size", "total")) {
    evaluation_ine <- assess_ine(data, params_ine)
    evaluation_cepal2020 <- assess_cepal2020(data, params_cepal2020)
    evaluation_cepal2023 <- assess_cepal2023(data, params_cepal2023, domain_info = FALSE)
    evaluation_cepal2023_2 <- assess_cepal2023(data, params_cepal2023, domain_info = TRUE)
  } else {
    evaluation_ine <- assess_ine(data, params_ine)
    evaluation_cepal2020 <- assess_cepal2020(data, params_cepal2020)
    evaluation_cepal2023 <- assess_cepal2023(data, params_cepal2023, domain_info = FALSE)
    evaluation_cepal2023_2 <- assess_cepal2023(data, params_cepal2023, domain_info = TRUE)
  }

  publication_ine <- publish_table(evaluation_ine)
  publication_cepal2020 <- publish_table(evaluation_cepal2020)
  publication_cepal2023 <- publish_table(evaluation_cepal2023)
  publication_cepal2023_2 <- publish_table(evaluation_cepal2023_2)

  list(
    ine = publication_ine,
    cepal2020 = publication_cepal2020,
    cepal2023_false = publication_cepal2023,
    cepal2023_true = publication_cepal2023_2
  )
}

# Test
test_that("evaluate function works correctly for mean", {
  result <- evaluate(data, params_ine, params_cepal2020, params_cepal2023, "mean")

  expect_true(!is.null(result$ine))
  expect_true(!is.null(result$cepal2020))
  expect_true(!is.null(result$cepal2023_false))
  expect_true(!is.null(result$cepal2023_true))
})

test_that("evaluate function works correctly for proportion", {
  result <- evaluate(data, params_ine, params_cepal2020, params_cepal2023, "proportion")

  expect_true(!is.null(result$ine))
  expect_true(!is.null(result$cepal2020))
  expect_true(!is.null(result$cepal2023_false))
  expect_true(!is.null(result$cepal2023_true))
})



#Domain info and low df
test_that("assess function works correctly with domain_info = TRUE for eclac_2023", {
  result <- assess(data, publish = FALSE, scheme = "eclac_2023", domain_info = TRUE, low_df_justified = TRUE)

  # Verificamos que todas las etiquetas están en el conjunto esperado
  expect_true(all(result$label == c("reliable", "reliable", "non-reliable", 'reliable')))

})


test_that("assess function works correctly with domain_info = FALSE for eclac_2023 and low_df_justified = TRUE", {
  result <- assess(data, publish = FALSE, scheme = "eclac_2023", domain_info = FALSE, low_df_justified = TRUE)

  # Verificamos que todas las etiquetas están en el conjunto esperado
  expect_true(all(result$label == c("reliable", "non-reliable", "non-reliable", 'reliable')))

})

test_that("assess function works correctly with low_df_justified = FALSE for eclac_2023 and domain_info = TRUE", {
  result <- assess(data, publish = FALSE, scheme = "eclac_2023", domain_info = TRUE, low_df_justified = FALSE)

  # Verificamos que todas las etiquetas están en el conjunto esperado
  expect_true(all(result$label == c("reliable", "non-reliable", "non-reliable", 'reliable')))

})




################################
# Chile Economic Survey Standard

dc_ele_t <- svydesign(ids = ~rol_ficticio,
                      weights = ~fe_transversal,
                      strata = ~estrato,
                      fpc = ~pob,             # correccion por poblacion finita
                      data = ELE7)

## prod salarial -> Ingreso Operacional total
prod_salarial <- create_prop('VA_2022f',
                             denominator = 'REMP_TOTAL',
                             domains = 'cod_actividad+cod_tamano',
                             design = dc_ele_t)

# test check table_n_obj

## sin indicar table_n_obj
test_that('test table_n_obj == NULL',
          expect_warning(assess(prod_salarial, scheme = 'chile_economics', domain_info = T))
          )

## diferentes tipos de columnas para merge table y table_n_obj
test_that('test different df types',
          expect_error(assess(prod_salarial, scheme = 'chile_economics', domain_info = T, table_n_obj = ELE7_n_obj)))


n_obj_ELE2 <- ELE7_n_obj %>%
  mutate(cod_actividad = cod_actividad_letra,
         cod_tamano = as.character(cod_tamano)) %>%
  select(-cod_actividad_letra)

## diferente numero de filas entre table y table_n_obj
test_that('test different number of rows',
          expect_error(assess(prod_salarial, scheme = 'chile_economics', domain_info = T,
                              table_n_obj = n_obj_ELE2 %>%
                                slice(1:40))
                       ))

## diferente table_n_obj con NAs
test_that('test different number of rows',
          expect_error(assess(prod_salarial, scheme = 'chile_economics', domain_info = T,
                              table_n_obj = n_obj_ELE2 %>%
                                mutate(n_obj= ifelse(n<30),NA, n_obj))
          ))

### uniendo n_obj a tabla
prod_salarial2 <- prod_salarial %>%
  left_join(n_obj_ELE2, by = c('cod_tamano', 'cod_actividad'))

## dos mensajes por separado
### 1ero
test_that('test message',expect_message(assess(prod_salarial2, scheme = 'chile_economics', domain_info = T, ratio_between_0_1 = FALSE),
                                        'n_obj missing in table_n_obj object'))

### 2do
test_that('test message',expect_message(assess(prod_salarial2, scheme = 'chile_economics', domain_info = T, ratio_between_0_1 = FALSE),
                                        'n_obj in table!'))

## revision de resultados equivalentes cuando n_obj esta en tabla o en table_n_obj
test_that('test equal n_obj in table and n_obj in table_n_obj',
          expect_equal(assess(prod_salarial2, scheme = 'chile_economics', domain_info = T, ratio_between_0_1 = FALSE),
                       assess(prod_salarial, scheme = 'chile_economics', domain_info = T, table_n_obj = n_obj_ELE2, ratio_between_0_1 = FALSE )))


## test resultado flujo para ratio
test_that('test total reliable in prod salarial',
          expect_equal(assess(prod_salarial, scheme = 'chile_economics', domain_info = T,
                              table_n_obj = n_obj_ELE2, ratio_between_0_1 = FALSE) %>% filter(label == 'reliable') %>% nrow(),
                       38))


## Test para evaluar si el ratio esta entre 0 y 1, en este caso le pusimos que esta entre 0 y 1 cuando en realidad se toman valores fuera a ese intervalo, se espera que arroje error:
test_that('test para evaluar ratio between 0 y 1',
          expect_warning(assess(prod_salarial,
                                scheme = 'chile_economics',
                                domain_info = T,
                                table_n_obj = n_obj_ELE2,
                                ratio_between_0_1 = TRUE))
          )


## test para evaluar NAs en table_n_obj
table_n_obj_NAS <- n_obj_ELE2 %>%
  mutate(n_obj = ifelse(cod_actividad == 'K' & cod_tamano == '1', NA, n_obj))

table_n_obj_NAS2 <- prod_salarial %>%
  filter(n>30) %>%
  left_join(table_n_obj_NAS, by = c('cod_actividad', 'cod_tamano')) %>%
  select(cod_actividad, cod_tamano, n_obj)


test_that('test  NA values in n_obj column',
          expect_warning(assess(prod_salarial %>% filter(n>30),
                                scheme = 'chile_economics', domain_info = TRUE,
                                table_n_obj = table_n_obj_NAS2, ratio_between_0_1 = FALSE),
                         'Oops! NA values found in n_obj column. The process will skip the sample recovery verification for those rows'))


test_that('test  NA values in n_obj column',
          expect_error(assess(prod_salarial,
                              scheme = 'chile_economics', domain_info = TRUE,
                              table_n_obj = table_n_obj_NAS, ratio_between_0_1 = FALSE),
                       'Oops! NA values found in n_obj column and some rows where n < 30. Please review your data.'))


