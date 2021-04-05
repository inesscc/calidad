context("test-create_tot")

ene <- ene %>%
  dplyr::mutate(fdt = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 9, 1, 0),
                ocupado = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0),
                desocupado = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0))


dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
options(survey.lonely.psu = "certainty")

dc_ene <- survey::svydesign(ids = ~conglomerado, strata = ~estrato_unico, data = ene,
                            weights = ~fact_cal)




######################
# Nombres de la tabla #
######################


test <-  create_tot(ocupado,  disenio = dc_ene)

nombres_obtenidos <-  names(test)

test_that("nombres tabla agregado", {
  expect_equal(nombres_obtenidos, c("variable", "total", "se", "n", "gl", "coef_var"))
})

#############################
# TOTALES SIN DESAGREGACIÓN #
#############################

# Testear tamaño muestral sin desagregación usando EPF
dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)

test <-  create_tot(ocupado, disenio = dc)

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
test <-  create_tot(ocupado,  disenio = dc_ene) %>%
  dplyr::filter(variable == "ocupado1")

test_that("totales agregado", {
  expect_equal(round(test$total[1] / 1000, 2), 8942.42)
})





#############################
# TOTALES CON DESAGREGACIÓN #
#############################

# Testear los nombres de la tabla
test <-  create_tot(ocupado, dominios =  zona+sexo, disenio = dc)
test_that("nombres desagregado", {
  expect_equal(names(test), c("zona", "sexo", "total", "se", "n", "gl", "coef_var"))
})

# Testear totales con desagregación, utilizando la ENE
test <-  create_tot(ocupado, dominios = sexo,  disenio = dc_ene) %>%
  dplyr::filter(sexo == 1)

test_that("totales desagregado", {
  expect_equal(round(test$total[1] / 1000, 2),  5198.32 )
})


# Testear tamaño muestral con desagregación
test <-  create_tot(ocupado, dominios = zona+sexo, disenio = dc) %>%
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


# Testear gl con desagregación
test <-  create_tot(ocupado, dominios = zona+sexo, disenio = dc) %>%
  dplyr::filter(zona == 1 & sexo == 1) %>%
  dplyr::pull(gl)

insumo <- epf_personas %>%
  dplyr::filter(ocupado == 1 & zona == 1 & sexo == 1)

gl <- length(unique(insumo$varunit)) - length(unique(insumo$varstrat))

test_that("gl desagregado", {
  expect_equal(test, gl)
})
