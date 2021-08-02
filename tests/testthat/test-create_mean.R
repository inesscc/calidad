
context("test-create_mean")

# Diseños muestrales
options(survey.lonely.psu = "certainty")

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

#####################
# PROBAR NA EN SUBPOP
#####################

expect_error(create_mean(gastot_hd, dominios =  sexo, subpop = metro_na, disenio = dc),
             "subpop contains NAs!")


##############################
# MEDIA SIN DESAGREGACIÓN
##############################


# Testear la media sin desagregación

test1 <-  create_mean(gastot_hd, disenio = dc)

test_that("Insumo media", {
  expect_equal(round(test1$mean), 1121925)
})


# Testear la media con desagregación
test2 <-  create_mean(gastot_hd, dominios =  zona, disenio = dc)

test_that("Insumo media zona", {
  expect_equal(round(test2$mean), c(1243155, 969048))
})

# Testear grados de libertad con desagregación
test3 <-  create_mean(gastot_hd, zona, disenio = dc) %>%
  dplyr::filter(zona == 1) %>%
  dplyr::select(gl) %>%
  dplyr::pull()

insumo <- epf_personas %>%
  dplyr::filter(zona == 1)

gl <- length(unique(insumo$varunit)) - length(unique(insumo$varstrat))

test_that("gl media desagregado", {
  expect_equal(test3, gl)
})

# Testear tamaño muestral desagregado
test4 <-  create_mean(gastot_hd, zona, disenio = dc) %>%
  dplyr::filter(zona == 2) %>%
  dplyr::select(n) %>%
  dplyr::pull()

n <- epf_personas %>%
  dplyr::group_by(folio) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::filter(zona == 2) %>%
  dplyr::count(zona) %>%
  dplyr::pull()

test_that("tamaño muestral media desagregada", {
  expect_equal(test4, n)
})


############################################
# Probar deff y tamaño de muestra efectivo #
############################################

test2 <-  create_mean(gastot_hd, disenio = dc)
test2 <-  create_mean(gastot_hd, dominios =  zona, disenio = dc, deff = T)
test2 <-  create_mean(gastot_hd, dominios =  zona+sexo, disenio = dc, ess = T)



#epf <- read_dta("C:/Users/klehm/Instituto Nacional de Estadisticas/Capacitación INE - General/capacitaciones previas/Capacicación Socialbit SA-FPC/Clase_3_dplyr_loops/Clase Vero Tidyverse/BASE_PERSONAS_VIII_EPF.dta")

#names(epf) <- tolower(names(epf))

#epf <- epf %>%
#  dplyr::mutate(ocupado = dplyr::if_else(cae == 1, 1, 0)) %>%
#  dplyr::mutate_at(.vars = dplyr::vars(sexo, zona, ecivil), .funs = list(as.numeric )) %>%
#  dplyr::select("folio", "sexo", "zona", "ecivil", "fe", "varunit", "varstrat", "gastot_hd", "ocupado") %>%
 # as.data.frame()

#epf_personas <- epf

#save(epf_personas, file = "data/epf_personas.RData")



#ene <- read_delim("C:/Users/klehm/Downloads/ene-2020-02-efm (2).csv", delim = ";")
#ene <- read_dta("C:/Users/klehm/Downloads/ene-2020-02-efm.dta")

#ene <- ene %>%
#  mutate(fdt = if_else(cae_especifico >= 1 & cae_especifico <= 9, 1, 0),
#         pet = if_else(edad >= 15, 1, 0),
#         ocupado = if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0),
#         desocupado = if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0)) %>%
#  select(sexo, region, cae_especifico, fe = fact_cal,
#         varunit = conglomerado,
#         varstrat = estrato_unico,
#         fdt, ocupado,desocupado, edad, pet) %>%
#  mutate_at(.vars = vars(sexo, region, cae_especifico), .funs = as.numeric) %>%
#  filter(!is.na(fe)) %>%
#  as.data.frame()
#save(ene, file = "data/ene.RData")

