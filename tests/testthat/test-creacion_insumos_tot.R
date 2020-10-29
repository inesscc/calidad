context("test-creacion_insumos_tot")

dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
options(survey.lonely.psu = "certainty")

######################
# Nombres de la tabla #
######################

dc_ene <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = ene,
                            weights = ~fe)

test <-  crear_insumos_tot(ocupado,  disenio = dc_ene)

nombres_obtenidos <-  names(test)

test_that("nombres tabla agregado", {
  expect_equal(nombres_obtenidos, c("variable", "total", "se", "n", "gl", "coef_var"))
})

#############################
# TOTALES SIN DESAGREGACIÓN #
#############################

# Testear tamaño muestral sin desagregación
dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)

test <-  crear_insumos_tot(ocupado, disenio = dc)

n <- epf_personas %>%
  dplyr::group_by(ocupado) %>%
  dplyr::count() %>%
  dplyr::filter(ocupado == 1) %>%
  dplyr::pull(n)


test_that("tamaño muestral agregado", {
  expect_equal(test$n[2], n)
})

# Testear gl  sin desagregación
insumo <- epf_personas %>%
  dplyr::filter(ocupado == 1)

gl <- length(unique(insumo$varunit)) - length(unique(insumo$varstrat))

test_that("gl agregado", {
  expect_equal(test$gl[2], gl)
})

# Testear totales sin desagregación, utilizando la ENE
test <-  crear_insumos_tot(ocupado,  disenio = dc_ene) %>%
  dplyr::filter(variable == "ocupado1")

test_that("totales agregado", {
  expect_equal(round(test$total[1] / 1000, 2), 8942.42)
})



#############################
# TOTALES CON DESAGREGACIÓN #
#############################

# Testear los nombres de la tabla
test <-  crear_insumos_tot(ocupado, zona+sexo, disenio = dc)
test_that("nombres desagregado", {
  expect_equal(names(test), c("zona", "sexo", "ocupado", "se", "n", "gl", "coef_var"))
})

# Testear totales con desagregación, utilizando la ENE
test <-  crear_insumos_tot(ocupado, sexo,  disenio = dc_ene) %>%
  dplyr::filter(sexo == 1)

test_that("totales desagregado", {
  expect_equal(round(test$ocupado[1] / 1000, 2),  5198.32 )
})



# Testear tamaño muestral con desagregación
test <-  crear_insumos_tot(ocupado, zona+sexo, disenio = dc) %>%
  dplyr::filter(zona == 1 & sexo == 1) %>%
  dplyr::pull(n)

n <- epf_personas %>%
  dplyr::group_by(ocupado, zona, sexo) %>%
  dplyr::count() %>%
  dplyr::filter(ocupado == 1 & zona == 1 & sexo == 1) %>%
  dplyr::pull(n)


test_that("tamaño muestral desagregado", {
  expect_equal(test, n)
})


# Testear gl  con desagregación
test <-  crear_insumos_tot(ocupado, zona+sexo, disenio = dc) %>%
  dplyr::filter(zona == 1 & sexo == 1) %>%
  dplyr::pull(gl)

insumo <- epf_personas %>%
  dplyr::filter(ocupado == 1 & zona == 1 & sexo == 1)

gl <- length(unique(insumo$varunit)) - length(unique(insumo$varstrat))

test_that("gl desagregado", {
  expect_equal(test, gl)
})
