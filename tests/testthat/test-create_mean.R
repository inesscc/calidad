
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
# PROBAR CALCULAR_N
#####################

# Con desagregación

agrupacion <- c("sexo", "zona")
agrupacion <- NULL
n <- calcular_n(dc$variables, agrupacion)

test_that("conteo n agrupado", {
  expect_equal(n$n, true_n$contar)
})

# Sin desagregación
agrupacion <- NULL
n <- calcular_n(dc$variables, agrupacion)
true_n <- nrow(dc$variables)

test_that("conteo n sin agrupar", {
  expect_equal(n$n[1], true_n)
})


#####################
# PROBAR GET_DF
#####################

# Con diseño complejo
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

# Sin diseño complejo
df <- get_df(dc_sin_varunit, agrupacion)

test_that("conteo df sin diseño complejo", {
  expect_equal(df$df[1], NA)
})





#####################
# PROBAR NA EN SUBPOP
#####################

expect_error(create_mean("gastot_hd", dominios =  "sexo", subpop = "metro_na", disenio = dc),
             "subpop contains NAs!")


#######################
# PROBAR VALOR DE MEDIA
#######################


# Testear la media sin desagregación

test1 <-  create_mean("gastot_hd", disenio = dc)

test_that("Insumo media", {
  expect_equal(round(test1$stat), 1121925)
})


# Testear la media con desagregación
test2 <-  create_mean("gastot_hd", dominios =  "zona", disenio = dc)

test_that("Insumo media zona", {
  expect_equal(round(test2$stat), c(1243155, 969048))
})



############################################
# Probar deff y tamaño de muestra efectivo #
############################################

test2 <-  create_mean("gastot_hd", disenio = dc)
test2 <-  create_mean("gastot_hd", dominios =  "zona+sexo", disenio = dc, deff = F, rm.na = F)
test2 <-  create_mean("gastot_hd", dominios =  "zona+sexo", disenio = dc, ess = T)




