


context("test-create_total")


####################
# DECLARAR DISEÑOS #
# ##################


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


##############################
# Testear estimación
##############################

# Probar suma con desagregación
suma <- create_total("gastot_hd", dominios = "zona", disenio = dc) %>%
  dplyr::pull(stat)

# valor real de la suma
suma_real <- dc$variables %>%
  dplyr::group_by(zona) %>%
  dplyr::summarise(stat = sum(fe * gastot_hd), 3  )   %>%
  dplyr::pull(stat)

test_that("suma del gasto", {
  expect_equal(suma, suma_real)
})

# Probar sin diseño complejo
suma <- create_total("gastot_hd", dominios = "zona", disenio = dc_sin_varunit) %>%
  dplyr::pull(stat)

test_that("suma del gasto sin ", {
  expect_equal(suma, suma_real)
})


##############################
# Testear grados de libertad
##############################







