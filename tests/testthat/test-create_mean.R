context("test-create_mean")

####################
# DECLARAR DISEÑOS #
####################

options(survey.lonely.psu = "certainty")

# Diseño complejo con varstrat y varunit
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

# Diseño sin varunit
dc_sin_varunit <- survey::svydesign(ids = ~1,
                                    data = epf_personas %>%
                                      dplyr::group_by(folio) %>%
                                      dplyr::slice(1) %>%
                                      dplyr::ungroup() %>%
                                      dplyr::mutate(
                                        metro = dplyr::if_else(zona == 1, 1, 0),
                                        metro_na = dplyr::if_else(dplyr::row_number() <= 10, NA_real_, metro )),
                                    weights = ~fe)

#####################
# PROBAR NA EN SUBPOP
#####################

expect_error(create_mean("gastot_hd", domains =  "sexo", subpop = "metro_na", design = dc),
             "subpop contains NAs!")

#######################
# PROBAR VALOR DE MEDIA
#######################

# Testear la media sin desagregación para cada valor de eclac_input

test1_ine <-  create_mean("gastot_hd", design = dc, eclac_input = "chile")
test1_eclac <-  create_mean("gastot_hd", design = dc, eclac_input = "eclac_2020")
test1_eclac_2023 <-  create_mean("gastot_hd", design = dc, eclac_input = "eclac_2023")

test_that("Insumo media para chile", {
  expect_equal(round(test1_ine$stat), 1121925)
})

test_that("Insumo media para eclac", {
  expect_equal(round(test1_eclac$stat), 1121925)
})

test_that("Insumo media para eclac_2023", {
  expect_equal(round(test1_eclac_2023$stat), 1121925)
})

# Testear la media con desagregación para cada valor de eclac_input
test2_ine <-  create_mean("gastot_hd", domains =  "zona", design = dc, eclac_input = "chile")
test2_eclac <-  create_mean("gastot_hd", domains =  "zona", design = dc, eclac_input = "eclac_2020")
test2_eclac_2023 <-  create_mean("gastot_hd", domains =  "zona", design = dc, eclac_input = "eclac_2023")

test_that("Insumo media zona para chile", {
  expect_equal(round(test2_ine$stat), c(1243155, 969048))
})

test_that("Insumo media zona para eclac", {
  expect_equal(round(test2_eclac$stat), c(1243155, 969048))
})

test_that("Insumo media zona para eclac_2023", {
  expect_equal(round(test2_eclac_2023$stat), c(1243155, 969048))
})

#################################
# Testear los grados de libertad
#################################
df_ine <-  create_mean("gastot_hd", domains =  "zona+sexo", design = dc, eclac_input = "chile")
df_eclac <-  create_mean("gastot_hd", domains =  "zona+sexo", design = dc, eclac_input = "eclac_2020")
df_eclac_2023 <-  create_mean("gastot_hd", domains =  "zona+sexo", design = dc, eclac_input = "eclac_2023")

true_upm <- dc$variables %>%
  dplyr::group_by(sexo, zona, varunit) %>%
  dplyr::mutate(upm = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(sexo, zona) %>%
  dplyr::summarise(upm = sum(upm))

true_strata <- dc$variables %>%
  dplyr::group_by(sexo, zona, varstrat) %>%
  dplyr::mutate(strata = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(sexo, zona) %>%
  dplyr::summarise(strata = sum(strata))

true_df <- true_upm %>%
  dplyr::left_join(true_strata, by = c("sexo", "zona")) %>%
  dplyr::mutate(df = upm - strata)

test_that("conteo df diseño complejo para chile", {
  expect_equal(true_df$df, df_ine$df)
})

test_that("conteo df diseño complejo para eclac", {
  expect_equal(true_df$df, df_eclac$df)
})

test_that("conteo df diseño complejo para eclac_2023", {
  expect_equal(true_df$df, df_eclac_2023$df)
})

######################
# Confidence intervals
######################

df_ci_ine <-  create_mean("gastot_hd", domains =  "zona+sexo", design = dc, ci = TRUE, eclac_input = "chile")
df_ci_eclac <-  create_mean("gastot_hd", domains =  "zona+sexo", design = dc, ci = TRUE, eclac_input = "eclac_2020")
df_ci_eclac_2023 <-  create_mean("gastot_hd", domains =  "zona+sexo", design = dc, ci = TRUE, eclac_input = "eclac_2023")

############################################
# Probar deff y tamaño de muestra efectivo #
############################################

test2_ine <-  create_mean("gastot_hd", design = dc, eclac_input = "chile")
test2_ine <-  create_mean("gastot_hd", domains =  "zona+sexo", design = dc, deff = FALSE, rm.na = FALSE, eclac_input = "chile")

test2_eclac <-  create_mean("gastot_hd", design = dc, eclac_input = "eclac_2020")
test2_eclac <-  create_mean("gastot_hd", domains =  "zona+sexo", design = dc, deff = FALSE, rm.na = FALSE, eclac_input = "eclac_2020")

test2_eclac_2023 <-  create_mean("gastot_hd", design = dc, eclac_input = "eclac_2023")
test2_eclac_2023 <-  create_mean("gastot_hd", domains =  "zona+sexo", design = dc, deff = FALSE, rm.na = FALSE, eclac_input = "eclac_2023")

expect_warning(create_mean("gastot_hd", domains =  "zona+sexo", design = dc, ess = TRUE, eclac_input = "chile"),
               "to get effective sample size use deff = T")

expect_warning(create_mean("gastot_hd", domains =  "zona+sexo", design = dc, ess = TRUE, eclac_input = "eclac_2020"),
               "to get effective sample size use deff = T")

expect_warning(create_mean("gastot_hd", domains =  "zona+sexo", design = dc, ess = TRUE, eclac_input = "eclac_2023"),
               "to get effective sample size use deff = T")

all_ine <- create_mean("gastot_hd", domains = "zona+sexo", design = dc, ci = TRUE, ess = TRUE, deff = TRUE, rm.na = TRUE, unweighted = TRUE, rel_error = TRUE, eclac_input = "chile")
all_eclac <- create_mean("gastot_hd", domains = "zona+sexo", design = dc, ci = TRUE, ess = TRUE, deff = TRUE, rm.na = TRUE, unweighted = TRUE, rel_error = TRUE, eclac_input = "eclac_2020")
all_eclac_2023 <- create_mean("gastot_hd", domains = "zona+sexo", design = dc, ci = TRUE, ess = TRUE, deff = TRUE, rm.na = TRUE, unweighted = TRUE, rel_error = TRUE, eclac_input = "eclac_2023")

# Check column names
waited_output <- c("stat", "se", "n", "cv", "deff", "lower", "upper", "relative_error", "ess", "unweighted")

test_that("suma del gasto nivel nacional para chile", {
  expect_equal(sum(names(all_ine) %in% waited_output), length(waited_output))
})

test_that("suma del gasto nivel nacional para eclac", {
  expect_equal(sum(names(all_eclac) %in% waited_output), length(waited_output))
})

test_that("suma del gasto nivel nacional para eclac_2023", {
  expect_equal(sum(names(all_eclac_2023) %in% waited_output), length(waited_output))
})


