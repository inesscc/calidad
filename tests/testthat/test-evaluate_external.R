context("test-assess_external")

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
# assess #
############

# National level with denominator
expect_error(create_prop(var = "mujer", denominator = "hombre", design = dc_ene, eclac_input = T),
             "eclac approach is not allowed with denominator")

# INE Chile Standard for mean
test1 <- create_mean("gastot_hd", domains = "zona+sexo+ecivil", design = dc, deff = TRUE, ess = TRUE, unweighted = TRUE)
test <- assess(test1, publish = TRUE)

# INE Chile Standard for proportion
test2 <- create_prop("desocupado", domains = "region+sexo", design = dc_ene, deff = TRUE, ess = TRUE, log_cv = TRUE, unweighted = TRUE)

x <- survey::svyby(~desocupado, by = ~region+sexo, design = dc_ene, FUN = survey::svymean)
test_cv <- survey::cv(x)

test_that("cv calculado correctamente", {
  expect_equal(sum(test2$cv == test_cv), length(test_cv))
})

test2_sin_log <- create_prop("desocupado", domains = "region+sexo", design = dc_ene, deff = TRUE, ess = TRUE, log_cv = FALSE, unweighted = TRUE)
test <- assess(test2_sin_log)

# INE Chile Standard for size
test3 <- create_size("desocupado", domains = "region", design = dc_ene, deff = TRUE, ess = TRUE, unweighted = TRUE)
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
test <- assess(test3, scheme = "eclac_2020")
test <- assess(test4, scheme = "eclac_2020")

# Proportion without log_cv
expect_error(assess(test2_sin_log, scheme = "eclac_2020"),
             "log_cv must be used!")

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
test <- assess(test3, scheme = "eclac_2023")
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
data <- data.frame(
  n = c(80, 150, 500, 120),
  df = c(10, 9, 8, 10),
  cv = c(0.12, 0.25, 0.35, 0.2),
  ess = c(70, 130, 45, 110),
  unweighted = c(60, 110, 40, 100),
  stat = c(0.4, 0.8, 1.2, 0.6),
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


#Domain info
test_that("assess function works correctly with domain_info = TRUE for eclac_2023", {
  result <- assess(data, publish = FALSE, scheme = "eclac_2023", domain_info = TRUE)

  # Verificamos que todas las etiquetas están en el conjunto esperado
  expect_true(all(result$label == c("reliable", "reliable", "non-reliable", 'reliable')))

})


test_that("assess function works correctly with domain_info = FALSE for eclac_2023", {
  result <- assess(data, publish = FALSE, scheme = "eclac_2023", domain_info = FALSE)

  # Verificamos que todas las etiquetas están en el conjunto esperado
  expect_true(all(result$label == c("reliable", "non-reliable", "non-reliable", 'reliable')))

})



