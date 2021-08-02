
context("test-create_prop")

options(survey.lonely.psu = "certainty")


# Diseños muestrales

ene <- ene %>%
  dplyr::mutate(fdt = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 9, 1, 0),
         ocupado = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0),
         desocupado = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0),
         hombre = dplyr::if_else(sexo == 1, 1, 0),
         mujer = dplyr::if_else(sexo == 2, 1, 0),
         metro = dplyr::if_else(region == 13, 1, 0),
         sexo2 = factor(dplyr::if_else(sexo == 1, "hombre", "mújer")),
         region2 = dplyr::case_when(
           region == 1  ~ "Tarapacá",
           region == 2 ~ "Antofagasta",
           region == 3 ~ "Atacama",
           region == 4 ~ "Coquimbo",
           region == 5 ~ "Valparaíso",
           region == 6 ~ "O'Higgins",
           region == 7 ~ "Maule",
           region == 8 ~ "Bíobío",
           region == 9 ~ "Araucanía",
           region == 10 ~ "Los Lagos",
           region == 11 ~ "Aysén",
           region == 12 ~ "Mgallanes",
           region == 13 ~ "Metropolitana",
           region == 14 ~ "Los Ríos",
           region == 15 ~ "Arica",
           region == 16 ~ "Ñuble",
         ),
         region2 = haven::labelled(region2))



dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas %>%
                          dplyr::mutate(gasto_ocup = dplyr::if_else(ocupado == 1, gastot_hd, 0)), weights = ~fe)
dc_ene <- survey::svydesign(ids = ~conglomerado, strata = ~estrato_unico, data = ene %>%
                              dplyr::mutate(desocupado2 = dplyr::if_else(desocupado == 1 & fdt == 1, 1, 0),
                                            fdt_na = dplyr::if_else(dplyr::row_number() <= 10, NA_real_, fdt ) ),
                              weights = ~fact_cal)





  #create_prop(var = fdt, dominios = sexo, disenio = dc_ene)

# enusc <- enusc %>%
#   dplyr::rename(varunit = Conglomerado,
#          varstrat = VarStrat) %>%
#   dplyr::mutate_at(.vars =  dplyr::vars(rph_sexo), .funs =  as.numeric)

#dc_enusc <- svydesign(ids = ~varunit, strata = ~varstrat, data = enusc, weights = ~Fact_Pers)


#####################
# PROBAR NA EN SUBPOP
#####################


expect_error(create_prop(desocupado, dominios =  sexo, subpop = fdt_na, disenio = dc_ene),
             "subpop contains NAs!")


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


# Testear tamaño muestral con modalidad ratio invertido

n <- ene %>%
  dplyr::group_by(sexo, ocupado) %>%
  dplyr::summarise(contar = dplyr::n()) %>%
  dplyr::group_by(ocupado) %>%
  dplyr::summarise(n = sum(contar))

test <-  create_prop(var = mujer, denominador = hombre, dominios = ocupado, disenio = dc_ene)

nrow(dc_ene$variables)
test_that("gl proporción desagregado ene", {
  expect_equal(n[2, 2] %>% dplyr::pull(), test[2, 6])
})

# Testear grados de libertad con modalidad ratio invertido

test <-  create_prop(var = mujer, denominador = hombre, dominios = ocupado+metro, disenio = dc_ene)

gl <- ene %>%
  dplyr::group_by(ocupado, metro, conglomerado) %>%
  dplyr::mutate(n_varunit = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(ocupado, estrato_unico) %>%
  dplyr::mutate(n_varstrat = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(ocupado, metro, sexo) %>%
  dplyr::summarise(n_varunit = sum(n_varunit),
                   n_varstrat = sum(n_varstrat)) %>%
  dplyr::mutate(gl = n_varunit - n_varstrat) %>%
  dplyr::group_by(ocupado, metro) %>%
  dplyr::summarise(gl = sum(gl))

test_that("gl proporción desagregado ene", {
  expect_equal(gl[1, 3] %>% dplyr::pull(), test[1, 6])
})


# Testear grados de libertad con modalidad ratio normal

test <-  create_prop(var = gasto_ocup, denominador = gastot_hd, dominios = zona, disenio = dc)

gl <- epf_personas %>%
  dplyr::mutate(gasto_ocup = dplyr::if_else(ocupado == 1, gastot_hd, 0)) %>%
  dplyr::group_by(zona, varunit) %>%
  dplyr::mutate(n_varunit = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(zona, varstrat) %>%
  dplyr::mutate(n_varstrat = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(zona) %>%
  dplyr::summarise(n_varunit = sum(n_varunit),
                   n_varstrat = sum(n_varstrat)) %>%
  dplyr::mutate(gl = n_varunit - n_varstrat)

test_that("gl proporción desagregado ene", {
  expect_equal(gl[1, 4] %>% dplyr::pull(), test[1, 5])
})

############################################
# Probar deff y tamaño de muestra efectivo #
############################################

test2 <-  create_prop(desocupado, disenio = dc_ene)
test2 <-  create_prop(desocupado, dominios =  region, disenio = dc_ene)
test2 <-  create_prop(desocupado, dominios =  region, subpop = fdt, disenio = dc_ene)
test2 <-  create_prop(desocupado, dominios =  region+sexo, disenio = dc_ene, ess = T)


#########################
# Probar cv logarítmico #
#########################

test2 <-  create_prop(desocupado, disenio = dc_ene, log_cv = T)
test2 <-  create_prop(desocupado, dominios =  region, disenio = dc_ene, log_cv = T)
test2 <-  create_prop(desocupado, dominios =  region, subpop = fdt, disenio = dc_ene, log_cv = T)
test2 <-  create_prop(desocupado, dominios =  region+sexo, disenio = dc_ene, log_cv = T)



################
# PRUEBAS ENUSC#
################

### Tamaño muestral  desagregado ENUSC

# dc_enusc <- svydesign(ids = ~Conglomerado, strata = ~VarStrat,
#                      data = enusc, weights = ~Fact_Pers)
#
# options(survey.lonely.psu = "certainty")
#
#   insumos_prop <- create_prop(VP_DC, dominios =  enc_region+rph_sexo, disenio = dc_enusc) %>%
#     evaluate_prop()
#
#   test <- insumos_prop %>%
#     dplyr::filter(rph_sexo == 1 & enc_region  == 1) %>%
#     dplyr::pull(n)
#
#   n <- enusc %>%
#     dplyr::filter(rph_sexo == 1 & enc_region  == 1) %>%
#     nrow()
#
#   test_that("n proporción desagregado enusc", {
#     expect_equal(test, n)
#   })

# #### testeo de create_ratio_internal #
#
# insumos_ratio <- create_prop(var = muj_insg_taxi, denominador = hom_insg_taxi, dominios =  enc_region+rph_sexo, disenio = dc_enusc) %>%
#   evaluate_prop()
#
# test <- insumos_ratio %>%
#   dplyr::filter(rph_sexo == 2 & enc_region  == 12) %>%
#   dplyr::pull(n)
#
# n <- enusc %>%
#   dplyr::filter(rph_sexo == 2 & enc_region  == 12 & (muj_insg_taxi == 1 | hom_insg_taxi == 1)) %>%
#   nrow()
#
# test_that("n proporción desagregado enusc", {
#   expect_equal(test, n)
# })


