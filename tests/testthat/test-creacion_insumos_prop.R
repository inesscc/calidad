
context("test-creacion_insumos_prop")

dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)

##############################
# PROPORCIÓN SIN DESAGREGACIÓN
##############################

# Testear la proporción sin desagregación
test1 <-  crear_insumos_prop(ocupado_int, disenio = dc)
test_that("Insumo proporción", {
  expect_equal(round(test1$objetivo, 3), unname(round(survey::svymean(x = ~ocupado_int, dc)[1], 3)))
})


##############################
# PROPORCIÓN CON DESAGREGACIÓN
##############################

# Testear grados de libertad con desagregación
test2 <-  crear_insumos_prop(ocupado_int, sexo+zona, disenio = dc) %>%
  dplyr::filter(sexo == 2 & zona == 1) %>%
  dplyr::select(gl) %>%
  dplyr::pull()

insumo <- epf_personas %>%
  dplyr::filter(sexo == 2 & zona == 1 & ocupado_int == 1)

gl <- length(unique(insumo$varunit)) - length(unique(insumo$varstrat))

test_that("gl proporción desagregado", {
  expect_equal(test2, gl)
})

# Testear tamaño muestral con desagregación
test3 <-  crear_insumos_prop(ocupado_int, sexo+zona+ecivil, disenio = dc) %>%
  dplyr::filter(sexo == 1 & zona == 1 & ecivil == 2) %>%
  dplyr::select(n) %>%
  dplyr::pull()

n <- epf_personas %>%
  dplyr::filter(sexo == 1 & zona == 1 & ecivil == 2, ocupado_int == 1) %>%
  dplyr::count() %>%
  dplyr::pull()

test_that("tamaño muestral proporción desagregado", {
  expect_equal(test3, n)
})

