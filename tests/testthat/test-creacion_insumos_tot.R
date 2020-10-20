context("test-creacion_insumos_tot")

dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)

#############################
# TOTALES SIN DESAGREGACIÓN #
#############################

# Testear tamaño muestral sin desagregación
test <-  crear_insumos_tot(ocupado_int, disenio = dc)

n <- epf_personas %>%
  dplyr::group_by(ocupado_int) %>%
  dplyr::count() %>%
  dplyr::filter(ocupado_int == 1) %>%
  dplyr::pull(n)


test_that("tamaño muestral agregado", {
  expect_equal(test$n[2], n)
})

# Testear gl  sin desagregación
insumo <- epf_personas %>%
  dplyr::filter(ocupado_int == 1)

gl <- length(unique(insumo$varunit)) - length(unique(insumo$varstrat))

test_that("gl agregado", {
  expect_equal(test$gl[2], gl)
})

# Testear totales sin desagregación, utilizando la ENE

#dc_ene <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = ene %>% mutate(ocupado2 = if_else(ocupado == 1, 1, 2)),
 #                           weights = ~fe)
#test <-  crear_insumos_tot(ocupado2, fdt, disenio = dc_ene)

#svytotal(~ocupado, dc_ene)
#svyby(formula = ~ocupado, by = ~fdt, design = dc_ene, FUN = svytotal )

#test_that("gl agregado", {
#  expect_equal(test$gl[2], gl)
#})



#############################
# TOTALES CON DESAGREGACIÓN #
#############################

# Testear los nombres de la tabla
test <-  crear_insumos_tot(ocupado_int, zona+sexo, disenio = dc)
names(test)
test_that("nombres desagregado", {
  expect_equal(names(test), c("zona", "sexo", "ocupado_int", "total", "se", "gl", "n", "coef_var"))
})

# Testear tamaño muestral con desagregación
test <-  crear_insumos_tot(ocupado_int, zona+sexo, disenio = dc) %>%
  dplyr::filter(ocupado_int == 1 & zona == 1 & sexo == 1) %>%
  dplyr::pull(n)

n <- epf_personas %>%
  dplyr::group_by(ocupado_int, zona, sexo) %>%
  dplyr::count() %>%
  dplyr::filter(ocupado_int == 1 & zona == 1 & sexo == 1) %>%
  dplyr::pull(n)


test_that("tamaño muestral desagregado", {
  expect_equal(test, n)
})


# Testear gl  con desagregación
test <-  crear_insumos_tot(ocupado_int, zona+sexo, disenio = dc) %>%
  dplyr::filter(ocupado_int == 1 & zona == 1 & sexo == 1) %>%
  dplyr::pull(gl)

insumo <- epf_personas %>%
  dplyr::filter(ocupado_int == 1 & zona == 1 & sexo == 1)

gl <- length(unique(insumo$varunit)) - length(unique(insumo$varstrat))

test_that("gl desagregado", {
  expect_equal(test, gl)
})
