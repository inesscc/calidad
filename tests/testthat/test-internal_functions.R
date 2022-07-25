
context("test-internal_functions")


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
                            metro_na = dplyr::if_else(dplyr::row_number() <= 10, NA_real_, metro ),
                            desocupado = dplyr::if_else(ocupado == 1, 0, 1)
                            ),
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
                                        metro_na = dplyr::if_else(dplyr::row_number() <= 10, NA_real_, metro ),
                                        desocupado = dplyr::if_else(ocupado == 1, 0, 1)
                                        ),
                                    weights = ~fe)

# Diseño ene
ene <- ene %>%
  dplyr::mutate(fdt = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 9, 1, 0),
                ocupado = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0),
                desocupado = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0),
                hombre = dplyr::if_else(sexo == 1, 1, 0),
                mujer = dplyr::if_else(sexo == 2, 1, 0)
                )

dc_ene <- survey::svydesign(ids = ~conglomerado, strata = ~estrato_unico, data = ene, weights = ~fact_cal)


#####################
# GET_SAMPLE_SIZE
#####################

# Desagregación en el caso normal
agrupacion <- c("sexo", "zona")
n <- get_sample_size(dc$variables, agrupacion)
true_n <- dc$variables %>%
  dplyr::group_by(sexo, zona) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::ungroup()


test_that("conteo n agrupado", {
  expect_equal(n$n, true_n$n)
})

# Desagregación en el caso especial de size INE
agrupacion <- c("sexo", "zona", "ocupado")
n <- get_sample_size(dc$variables, agrupacion, df_type = "ine")

true_n <- dc$variables %>%
  dplyr::group_by(sexo, zona, ocupado) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(ocupado == 1)


test_that("conteo n agrupado caso especial", {
  expect_equal(n$n, true_n$n)
})


# Sin desagregación en el caso normal
agrupacion <- NULL
n <- get_sample_size(dc$variables, agrupacion)

true_n <- nrow(dc$variables)

test_that("conteo n sin agrupar", {
  expect_equal(n$n[1], true_n)
})

# Sin desagregación en el caso especial size-INE
domains <- NULL
agrupacion <- c( "ocupado")
agrupacion <- c(domains, agrupacion)
n <- get_sample_size(dc$variables, agrupacion, df_type = "ine" )

true_n <- dc$variables %>%
  dplyr::filter(ocupado == 1) %>%
  nrow()

test_that("conteo n sin agrupar", {
  expect_equal(n$n[1], true_n)
})



#####################
# PROBAR GET_DF
#####################

# Con diseño complejo caso normal y dominios
agrupacion <- c("sexo", "zona")
df <- get_df(dc, agrupacion)

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

# Con diseño complejo caso ine-size y dominios
agrupacion <- c("sexo", "zona", "ocupado")
df <- get_df(dc, agrupacion, df_type = "ine")

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


# Con diseño complejo caso normal SIN dominios
agrupacion <- NULL
df <- get_df(dc, agrupacion)

true_upm <- length(unique(dc$variables$varunit))
true_strata <- length(unique(dc$variables$varstrat))

true_df <- true_upm - true_strata

test_that("conteo df sin dominios", {
  expect_equal(true_df, df[[1]])
})

# Con diseño complejo caso especial ine-size sin dominios
agrupacion <- NULL
var <- "desocupado"
dc_filtered <-  dc_ene[dc_ene$variables[["fdt"]] == 1]
dc_filtered <- standardize_design_variables(dc_filtered)

agrupacion <- c(agrupacion, var)
df <- get_df(dc_filtered, agrupacion, df_type = "ine")

true_upm <- length(unique(dc_filtered$variables$conglomerado[dc_filtered$variables$desocupado == 1]))
true_strata <- length(unique(dc_filtered$variables$estrato_unico[dc_filtered$variables$desocupado == 1]))

true_df <- true_upm - true_strata

test_that("conteo df sin dominios caso especial", {
  expect_equal(true_df, df %>% dplyr::pull(df))
})


# Sin diseño complejo
df <- get_df(dc_sin_varunit, agrupacion)

test_that("conteo df sin diseño complejo", {
  expect_equal(df$df[1], NA)
})


