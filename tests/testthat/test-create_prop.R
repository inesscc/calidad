
context("test-create_prop")

options(survey.lonely.psu = "certainty")

# Cargar base ENUSC
#enusc <-  readRDS("C:/Users/klehm/Downloads/bkish_2019.rds")

# Diseños muestrales

ene <- ene %>%
  dplyr::mutate(fdt = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 9, 1, 0),
         ocupado = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0),
         desocupado = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0))


dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
dc_ene <- survey::svydesign(ids = ~conglomerado, strata = ~estrato_unico, data = ene %>%
                              dplyr::mutate(desocupado2 = dplyr::if_else(desocupado == 1 & fdt == 1, 1, 0)),
                              weights = ~fact_cal)


# enusc <- enusc %>%
#   dplyr::rename(varunit = Conglomerado,
#          varstrat = VarStrat) %>%
#   dplyr::mutate_at(.vars =  dplyr::vars(rph_sexo), .funs =  as.numeric)

#dc_enusc <- svydesign(ids = ~varunit, strata = ~varstrat, data = enusc, weights = ~Fact_Pers)

##############################
# PROPORCIÓN SIN DESAGREGACIÓN
##############################

# Testear la proporción sin desagregación
test1 <-  create_prop(ocupado, disenio = dc)
test_that("Insumo proporción", {
  expect_equal(round(test1$objetivo, 3), unname(round(survey::svymean(x = ~ocupado, dc)[1], 3)))
})


# Probar strings
# anidar <-  function(var,denominador = NULL, dominios = NULL, subpop = NULL, disenio, ci = F){
#   create_prop(var, denominador, dominios, subpop, disenio,ci, anidar = T)
# }
#
# anidar(var = "ocupado", disenio = dc, ci = T)


##############################
# PROPORCIÓN CON DESAGREGACIÓN
##############################

# Testear la proporción con desagregación con datos de la ENE
test <-  create_prop(desocupado, dominios =  fdt+sexo, disenio = dc_ene) %>%
  dplyr::filter(fdt == 1 & sexo == 1) %>%
  dplyr::pull(objetivo) * 100

test_that("Proporción desagregada", {
  expect_equal(round(test, 1), 7.1)
})


test <-  create_prop(desocupado, dominios =   fdt+sexo+region, disenio = dc_ene) %>%
  dplyr::filter(fdt == 1 & sexo == 2 & region == 1) %>%
  dplyr::pull(objetivo) * 100

#test_that("Proporción desagregada", {
#  expect_equal(round(test, 1), 7.1)
#})

#ene %>%
#  mutate(numerador = if_else(desocupado == 1 & fdt == 1 & region == 1 & sexo == 2, fe, 0),
#         denominador = if_else(fdt == 1 & region == 1 & sexo == 2, fe, 0)) %>%
#  summarise(tasa = sum(numerador)/sum(denominador))



# Testear grados de libertad con desagregación EPF
test2 <-  create_prop(ocupado, dominios =   sexo+zona, disenio = dc) %>%
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
test3 <-  create_prop(ocupado, dominios = sexo+zona+ecivil, disenio = dc) %>%
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
test4 <-  create_prop(desocupado, dominios =  sexo+region, disenio = dc_ene) %>%
  dplyr::filter(sexo == 2 & region == 1) %>%
  dplyr::select(gl) %>%
  dplyr::pull()

insumo <- ene %>%
  dplyr::filter(sexo == 2 & region == 1)

gl <- length(unique(insumo$conglomerado)) - length(unique(insumo$estrato_unico))

test_that("gl proporción desagregado ene", {
  expect_equal(test4, gl)
})


################
# PRUEBAS ENUSC#
################

# Tamaño muestral  desagregado ENUSC
# insumos_prop <- create_prop(VP_DC, enc_region16FIX+rph_sexo, disenio = dc_enusc) %>%
#   evaluar_calidad_prop()
#
# test <- insumos_prop %>%
#   dplyr::filter(rph_sexo == 1 & enc_region16FIX  == "Arica y Parinacota") %>%
#   dplyr::pull(n)
#
# n <- enusc %>%
#   dplyr::filter(rph_sexo == 1 & enc_region16FIX  == "Arica y Parinacota") %>%
#   nrow()
#
# test_that("n proporción desagregado enusc", {
#   expect_equal(test, n)
# })
#


