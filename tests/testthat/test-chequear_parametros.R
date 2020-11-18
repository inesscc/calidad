
context("test-chequear_parametros")

# Diseños muestrales
options(survey.lonely.psu = "certainty")
enusc <-  readRDS("C:/Users/klehm/Downloads/bkish_2019.rds")

# Diseño muestral con nombres erróneos de variable
dc_enusc <- svydesign(ids = ~Conglomerado, strata = ~VarStrat, data = enusc, weights = ~Fact_Pers)

# Hacer el test con otros nombres de diseño muestral
test_that("chequear variables disenio", {
  expect_error(crear_insumos_prop(VP_DC, enc_region16FIX+rph_sexo, disenio = dc_enusc))
})
