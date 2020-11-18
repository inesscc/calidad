
context("test-creacion_insumos_prop")

# Diseños muestrales

options(survey.lonely.psu = "certainty")

dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
dc_ene <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = ene, weights = ~fe)
enusc <-  readRDS("C:/Users/klehm/Downloads/bkish_2019.rds")

enusc <- enusc %>%
  dplyr::rename(varunit = Conglomerado,
         varstrat = VarStrat) %>%
  dplyr::mutate_at(.vars =  dplyr::vars(rph_sexo), .funs =  as.numeric)

dc_enusc <- svydesign(ids = ~varunit, strata = ~varstrat, data = enusc, weights = ~Fact_Pers)

##############################
# PROPORCIÓN SIN DESAGREGACIÓN
##############################

# Testear la proporción sin desagregación
test1 <-  crear_insumos_prop(ocupado, disenio = dc)
test_that("Insumo proporción", {
  expect_equal(round(test1$objetivo, 3), unname(round(survey::svymean(x = ~ocupado, dc)[1], 3)))
})


##############################
# PROPORCIÓN CON DESAGREGACIÓN
##############################

# Testear la proporción con desagregación con datos de la ENE
test <-  crear_insumos_prop(desocupado, fdt+sexo, disenio = dc_ene) %>%
  dplyr::filter(fdt == 1 & sexo == 1) %>%
  dplyr::pull(objetivo) * 100

test_that("Proporción desagregada", {
  expect_equal(round(test, 1), 7.1)
})


# Testear grados de libertad con desagregación EPF
test2 <-  crear_insumos_prop(ocupado, sexo+zona, disenio = dc) %>%
  dplyr::filter(sexo == 2 & zona == 1) %>%
  dplyr::select(gl) %>%
  dplyr::pull()

insumo <- epf_personas %>%
  dplyr::filter(sexo == 2 & zona == 1)

gl <- length(unique(insumo$varunit)) - length(unique(insumo$varstrat))

test_that("gl proporción desagregado", {
  expect_equal(test2, gl)
})

# Testear tamaño muestral con desagregación EPF
test3 <-  crear_insumos_prop(ocupado, sexo+zona+ecivil, disenio = dc) %>%
  dplyr::filter(sexo == 1 & zona == 1 & ecivil == 2) %>%
  dplyr::select(n) %>%
  dplyr::pull()

n <- epf_personas %>%
  dplyr::filter(sexo == 1 & zona == 1 & ecivil == 2) %>%
  dplyr::count() %>%
  dplyr::pull()

test_that("tamaño muestral proporción desagregado", {
  expect_equal(test3, n)
})


# Testear grados de libertad con desagregación ENE
test4 <-  crear_insumos_prop(desocupado, sexo+region, disenio = dc_ene) %>%
  dplyr::filter(sexo == 2 & region == 1) %>%
  dplyr::select(gl) %>%
  dplyr::pull()

insumo <- ene %>%
  dplyr::filter(sexo == 2 & region == 1)

gl <- length(unique(insumo$varunit)) - length(unique(insumo$varstrat))

test_that("gl proporción desagregado ene", {
  expect_equal(test4, gl)
})


################
# PRUEBAS ENUSC#
################

# Tamaño muestral  desagregado ENUSC
insumos_prop <- crear_insumos_prop(VP_DC, enc_region16FIX+rph_sexo, disenio = dc_enusc) %>%
  evaluacion_calidad_prop()

test <- insumos_prop %>%
  dplyr::filter(rph_sexo == 1 & enc_region16FIX  == "Arica y Parinacota") %>%
  dplyr::pull(n)

n <- enusc %>%
  dplyr::filter(rph_sexo == 1 & enc_region16FIX  == "Arica y Parinacota") %>%
  nrow()

test_that("n proporción desagregado enusc", {
  expect_equal(test, n)
})



