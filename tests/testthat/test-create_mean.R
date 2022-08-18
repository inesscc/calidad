
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


# Testear la media sin desagregación

test1 <-  create_mean("gastot_hd", design = dc)

test_that("Insumo media", {
  expect_equal(round(test1$stat), 1121925)
})


# Testear la media con desagregación
test2 <-  create_mean("gastot_hd", domains =  "zona", design = dc)

test_that("Insumo media zona", {
  expect_equal(round(test2$stat), c(1243155, 969048))
})


#################################
# Testear los grados de libertad
#################################
df <-  create_mean("gastot_hd", domains =  "zona+sexo", design = dc)

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

test_that("conteo df diseño complejo", {
  expect_equal(true_df$df, df$df)
})


######################
# COnfidence intervals
######################

df <-  create_mean("gastot_hd", domains =  "zona+sexo", design = dc, ci = T)


############################################
# Probar deff y tamaño de muestra efectivo #
############################################

test2 <-  create_mean("gastot_hd", design = dc)
test2 <-  create_mean("gastot_hd", domains =  "zona+sexo", design = dc, deff = F, rm.na = F)

expect_warning(create_mean("gastot_hd", domains =  "zona+sexo", design = dc, ess = T),
               "to get effective sample size use deff = T")


all <- create_mean("gastot_hd", domains = "zona+sexo", design = dc, ci = T, ess = T, deff = T, rm.na = T, unweighted = T, rel_error = T)

# Check column names
waited_output <- c("stat", "se", "n", "cv", "deff", "lower", "upper", "relative_error", "ess", "unweighted")

test_that("suma del gasto nivel nacional", {
  expect_equal(sum(names(all) %in% waited_output), length(waited_output))
})
