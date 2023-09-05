
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
         region2 = haven::labelled(region2)) %>%
  dplyr::mutate(desocup = region)

### generamos una variable falsa para probar error específico
ene$ext = 0
ene$ext[1:round(nrow(calidad::ene)*0.01)] = 1

ene = ene %>%
  dplyr::mutate(fdtx = dplyr::case_when(ext == 1 ~ fdt, TRUE ~ 0))

dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas %>%
                          dplyr::mutate(gasto_ocup = dplyr::if_else(ocupado == 1, gastot_hd, 0)), weights = ~fe)

dc_ene <- survey::svydesign(ids = ~conglomerado, strata = ~estrato_unico, data = ene %>%
                              dplyr::mutate(desocupado2 = dplyr::if_else(desocupado == 1 & fdt == 1, 1, 0),
                                            fdt_na = dplyr::if_else(dplyr::row_number() <= 10, NA_real_, fdt ) ) %>%
                              dplyr::mutate(SEXO_TEST = sexo)
                              ,
                              weights = ~fact_cal)



#####################
# PROBAR NA EN SUBPOP
#####################


expect_error(create_prop("desocupado", domains =  "sexo", subpop = "fdt_na", design = dc_ene),
             "subpop contains NAs!")


##############################
# PROPORCIÓN SIN DESAGREGACIÓN
##############################

# Testear la proporción sin desagregación
test1 <-  create_prop("ocupado", design = dc)
test_that("Insumo proporción", {
  expect_equal(round(test1$stat, 3), unname(round(survey::svymean(x = ~ocupado, dc)[1], 3)))
})


# Probar strings
# anidar <-  function(var,denominator = NULL, domains = NULL, subpop = NULL, design, ci = F){
#   create_prop(var, denominator, domains, subpop, design,ci, anidar = T)
# }
#
# anidar(var = "ocupado", design = dc, ci = T)


##############################
# PROBAR VARIABLES EN MAYÚSCULA
##############################

test <-  create_prop("desocupado", domains =  "fdt+SEXO_TEST", design = dc_ene)

test1 <- test %>%
  dplyr::filter(fdt == 1 & sexo_test == 1) %>%
  dplyr::pull(stat) * 100

test_that("Proporción desagregada", {
  expect_equal(round(test1, 1), 7.1)
})

# probar listado de nombres
test_that("Proporción desagregada", {
  expect_equal(names(test), c("fdt", "sexo_test", "stat", "se", "df", "n", "cv"))
})



##############################
# PROPORCIÓN CON DESAGREGACIÓN
##############################

# Testear la proporción con desagregación con datos de la ENE
test <-  create_prop("desocupado", domains =  "fdt+sexo", design = dc_ene) %>%
  dplyr::filter(fdt == 1 & sexo == 1) %>%
  dplyr::pull(stat) * 100

test_that("Proporción desagregada", {
  expect_equal(round(test, 1), 7.1)
})

# Testear grados de libertad con desagregación EPF
test2 <-  create_prop("ocupado", domains =   "sexo+zona", design = dc) %>%
  dplyr::filter(sexo == 2 & zona == 1) %>%
  dplyr::select(df) %>%
  dplyr::pull()

insumo <- epf_personas %>%
  dplyr::filter(sexo == 2 & zona == 1)

gl <- length(unique(insumo$varunit)) - length(unique(insumo$varstrat))

test_that("gl proporción desagregado", {
  expect_equal(test2, gl)
})

# Testear tamaño muestral con desagregación EPF
test3 <-  create_prop("ocupado", domains = "sexo+zona+ecivil", design = dc) %>%
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
test4 <-  create_prop("desocupado", domains =  "sexo+region", design = dc_ene) %>%
  dplyr::filter(sexo == 2 & region == 1) %>%
  dplyr::select(df) %>%
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

test <-  create_prop(var = "mujer", denominator = "hombre", domains = "ocupado", design = dc_ene)

test_that("gl proporción desagregado ene", {
  expect_equal(n %>% dplyr::pull(n), test %>% dplyr::pull(n))
})

# Testear grados de libertad con modalidad ratio invertido

test <-  create_prop(var = "mujer", denominator = "hombre", domains = "ocupado+metro", design = dc_ene)

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
  dplyr::summarise(gl = sum(gl)) %>%
  dplyr::arrange(ocupado, metro)

test_that("gl proporción desagregado ene", {
  expect_equal(gl %>% dplyr::pull(gl), test %>% dplyr::arrange(ocupado, metro) %>%  dplyr::pull(df))
})


# Testear grados de libertad con modalidad ratio normal
test <-  create_prop(var = "gasto_ocup", denominator = "gastot_hd", domains = "zona", design = dc)

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
  expect_equal(gl %>% dplyr::pull(gl), test %>% dplyr::pull(df) )
})

############################################
# Probar deff y tamaño de muestra efectivo #
############################################

test2 <-  create_prop("desocupado", design = dc_ene)
test2 <-  create_prop("desocupado", domains =  "region", design = dc_ene)
test2 <-  create_prop("desocupado", domains =  "region", subpop = "fdt", design = dc_ene)

expect_warning(create_prop("desocupado", domains =  "region+sexo", design = dc_ene, ess = T),
               "to get effective sample size use deff = T")


#########################
# Probar cv logarítmico #
#########################

test2 <-  create_prop("desocupado", design = dc_ene, log_cv = T)
test2 <-  create_prop("desocupado", domains =  "region", design = dc_ene, log_cv = T)
test2 <-  create_prop("desocupado", domains =  "region", subpop = "fdt", design = dc_ene, log_cv = T)
test2 <-  create_prop("desocupado", domains =  "region+sexo", design = dc_ene, log_cv = T)

##########################################
# Probar alcance de nobres entre variables
###########################################

create_prop(var = "desocupado", domains = "sexo+region", design = dc_ene)

######################################################
# comparamos valores entre create_prop y create_mean #
######################################################

test_prop <- create_prop("desocupado", design = dc_ene)
test_media <- suppressWarnings({create_mean("desocupado", design = dc_ene)})


test_that("estadisticos similares entre prop y mean", {
  expect_equal(test_prop %>% dplyr::select(stat) %>% dplyr::pull(), test_media %>% dplyr::select(stat) %>% dplyr::pull())
  expect_equal(test_prop %>% dplyr::select(se) %>% dplyr::pull(), test_media %>% dplyr::select(se) %>% dplyr::pull())
  expect_equal(test_prop %>% dplyr::select(df) %>% dplyr::pull(), test_media %>% dplyr::select(df) %>% dplyr::pull())
  expect_equal(test_prop %>% dplyr::select(n) %>% dplyr::pull(), test_media %>% dplyr::select(n) %>% dplyr::pull())
  expect_equal(test_prop %>% dplyr::select(cv) %>% dplyr::pull(), test_media %>% dplyr::select(cv) %>% dplyr::pull())
})

#######################################################
##### testing outputs names                       #####
#######################################################

nombre_error <- create_prop(var = "desocup",
                            denominator = "fdt",
                            domains = "ext",
                            subpop = "fdtx",
                            design = dc_ene) %>% names()

nombre_error

nombre_bien <- create_prop(var = "desocup",
                           denominator = "fdt",
                           #domains = "ext",
                           subpop = "ext",
                           design = dc_ene) %>% names

test_that("comparando nombres", {
  expect_equal(all(nombre_bien %in% nombre_error), T)
})


