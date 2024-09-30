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
                sexo2 = factor(dplyr::if_else(sexo == 1, "hombre", "mujer")),
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
                  region == 12 ~ "Magallanes",
                  region == 13 ~ "Metropolitana",
                  region == 14 ~ "Los Ríos",
                  region == 15 ~ "Arica",
                  region == 16 ~ "Ñuble"
                ),
                region2 = haven::labelled(region2)) %>%
  dplyr::mutate(desocup = region)

### generamos una variable falsa para probar error específico
ene$ext = 0
ene$ext[1:round(nrow(ene) * 0.01)] = 1

ene = ene %>%
  dplyr::mutate(fdtx = dplyr::case_when(ext == 1 ~ fdt, TRUE ~ 0))

dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas %>%
                          dplyr::mutate(gasto_ocup = dplyr::if_else(ocupado == 1, gastot_hd, 0)), weights = ~fe)

dc_ene <- survey::svydesign(ids = ~conglomerado,
                            strata = ~estrato_unico,
                            data = ene %>%
                              dplyr::mutate(desocupado2 = dplyr::if_else(desocupado == 1 & fdt == 1, 1, 0),
                                            fdt_na = dplyr::if_else(dplyr::row_number() <= 10, NA_real_, fdt)) %>%
                              dplyr::mutate(SEXO_TEST = sexo),
                            weights = ~fact_cal)

#####################
# PROBAR NA EN SUBPOP
#####################

for (eclac_option in c("chile", "eclac_2020", "eclac_2023")) {
  expect_error(create_prop("desocupado", domains = "sexo", subpop = "fdt_na", design = dc_ene, eclac_input = eclac_option),
               "subpop contains NAs!")
}

##############################
# PROPORCIÓN SIN DESAGREGACIÓN
##############################

# Testear la proporción sin desagregación
for (eclac_option in c("chile", "eclac_2020", "eclac_2023")) {
  test1 <- create_prop("ocupado", design = dc, eclac_input = eclac_option)
  test_that(paste("Insumo proporción con", eclac_option), {
    expect_equal(round(test1$stat, 3), unname(round(survey::svymean(x = ~ocupado, dc)[1], 3)))
  })
}

##############################
# PROBAR VARIABLES EN MAYÚSCULA
##############################

for (eclac_option in c("chile", "eclac_2020", "eclac_2023")) {
  test <- create_prop("desocupado", domains = "fdt+SEXO_TEST", design = dc_ene, eclac_input = eclac_option)

  test1 <- test %>%
    dplyr::filter(fdt == 1 & sexo_test == 1) %>%
    dplyr::pull(stat) * 100

  test_that(paste("Proporción desagregada con", eclac_option), {
    expect_equal(round(test1, 1), 7.1)
  })

  test_that(paste("Proporción desagregada con", eclac_option), {
    expect_equal(names(test), c("fdt", "sexo_test", "stat", "se", "df", "n", "cv"))
  })
}

##############################
# PROPORCIÓN CON DESAGREGACIÓN
##############################

# Testear la proporción con desagregación con datos de la ENE
for (eclac_option in c("chile", "eclac_2020", "eclac_2023")) {
  test <- create_prop("desocupado", domains = "fdt+sexo", design = dc_ene, eclac_input = eclac_option) %>%
    dplyr::filter(fdt == 1 & sexo == 1) %>%
    dplyr::pull(stat) * 100

  test_that(paste("Proporción desagregada con", eclac_option), {
    expect_equal(round(test, 1), 7.1)
  })
}

# Testear grados de libertad con desagregación EPF
for (eclac_option in c("chile", "eclac_2020", "eclac_2023")) {
  test2 <- create_prop("ocupado", domains = "sexo+zona", design = dc, eclac_input = eclac_option) %>%
    dplyr::filter(sexo == 2 & zona == 1) %>%
    dplyr::select(df) %>%
    dplyr::pull()

  insumo <- epf_personas %>%
    dplyr::filter(sexo == 2 & zona == 1)

  gl <- length(unique(insumo$varunit)) - length(unique(insumo$varstrat))

  test_that(paste("gl proporción desagregado con", eclac_option), {
    expect_equal(test2, gl)
  })
}

# Testear tamaño muestral con desagregación EPF
for (eclac_option in c("chile", "eclac_2020", "eclac_2023")) {
  test3 <- create_prop("ocupado", domains = "sexo+zona+ecivil", design = dc, eclac_input = eclac_option) %>%
    dplyr::filter(sexo == 1 & zona == 1 & ecivil == 2) %>%
    dplyr::select(n) %>%
    dplyr::pull()

  n <- epf_personas %>%
    dplyr::filter(sexo == 1 & zona == 1 & ecivil == 2) %>%
    dplyr::count() %>%
    dplyr::pull()

  test_that(paste("tamaño muestral proporción desagregado con", eclac_option), {
    expect_equal(test3, n)
  })
}

# Testear grados de libertad con desagregación ENE
for (eclac_option in c("chile", "eclac_2020", "eclac_2023")) {
  test4 <- create_prop("desocupado", domains = "sexo+region", design = dc_ene, eclac_input = eclac_option) %>%
    dplyr::filter(sexo == 2 & region == 1) %>%
    dplyr::select(df) %>%
    dplyr::pull()

  insumo <- ene %>%
    dplyr::filter(sexo == 2 & region == 1)

  gl <- length(unique(insumo$conglomerado)) - length(unique(insumo$estrato_unico))

  test_that(paste("gl proporción desagregado ene con", eclac_option), {
    expect_equal(test4, gl)
  })
}

# Testear tamaño muestral con modalidad ratio invertido
for (eclac_option in c("chile")) {
  n <- ene %>%
    dplyr::group_by(sexo, ocupado) %>%
    dplyr::summarise(contar = dplyr::n()) %>%
    dplyr::group_by(ocupado) %>%
    dplyr::summarise(n = sum(contar))

  test <- create_prop(var = "mujer", denominator = "hombre", domains = "ocupado", design = dc_ene, eclac_input = eclac_option)

  test_that(paste("gl proporción desagregado ene con", eclac_option), {
    expect_equal(n %>% dplyr::pull(n), test %>% dplyr::pull(n))
  })
}

# Testear grados de libertad con modalidad ratio invertido
for (eclac_option in c("chile")) {
  test <- create_prop(var = "mujer", denominator = "hombre", domains = "ocupado+metro", design = dc_ene, eclac_input = eclac_option)

  gl <- ene %>%
    dplyr::group_by(ocupado, metro, conglomerado) %>%
    dplyr::mutate(n_varunit = dplyr::if_else(dplyr::row_number() == 1, 1, 0)) %>%
    dplyr::group_by(ocupado, estrato_unico) %>%
    dplyr::mutate(n_varstrat = dplyr::if_else(dplyr::row_number() == 1, 1, 0)) %>%
    dplyr::group_by(ocupado, metro, sexo) %>%
    dplyr::summarise(n_varunit = sum(n_varunit),
                     n_varstrat = sum(n_varstrat)) %>%
    dplyr::mutate(gl = n_varunit - n_varstrat) %>%
    dplyr::group_by(ocupado, metro) %>%
    dplyr::summarise(gl = sum(gl)) %>%
    dplyr::arrange(ocupado, metro)

  test_that(paste("gl proporción desagregado ene con", eclac_option), {
    expect_equal(gl %>% dplyr::pull(gl), test %>% dplyr::arrange(ocupado, metro) %>% dplyr::pull(df))
  })
}

# Testear grados de libertad con modalidad ratio normal
for (eclac_option in c("chile")) {
  test <- create_prop(var = "gasto_ocup", denominator = "gastot_hd", domains = "zona", design = dc, eclac_input = eclac_option)

  gl <- epf_personas %>%
    dplyr::mutate(gasto_ocup = dplyr::if_else(ocupado == 1, gastot_hd, 0)) %>%
    dplyr::group_by(zona, varunit) %>%
    dplyr::mutate(n_varunit = dplyr::if_else(dplyr::row_number() == 1, 1, 0)) %>%
    dplyr::group_by(zona, varstrat) %>%
    dplyr::mutate(n_varstrat = dplyr::if_else(dplyr::row_number() == 1, 1, 0)) %>%
    dplyr::group_by(zona) %>%
    dplyr::summarise(n_varunit = sum(n_varunit),
                     n_varstrat = sum(n_varstrat)) %>%
    dplyr::mutate(gl = n_varunit - n_varstrat)

  test_that(paste("gl proporción desagregado con", eclac_option), {
    expect_equal(gl %>% dplyr::pull(gl), test %>% dplyr::pull(df))
  })
}

############################################
# Probar deff y tamaño de muestra efectivo #
############################################

for (eclac_option in c("chile", "eclac_2020", "eclac_2023")) {
  test2 <- create_prop("desocupado", design = dc_ene, eclac_input = eclac_option)
  test2 <- create_prop("desocupado", domains = "region", design = dc_ene, eclac_input = eclac_option)
  test2 <- create_prop("desocupado", domains = "region", subpop = "fdt", design = dc_ene, eclac_input = eclac_option)

  expect_warning(create_prop("desocupado", domains = "region+sexo", design = dc_ene, ess = TRUE, eclac_input = eclac_option),
                 "to get effective sample size use deff = T")
}

#########################
# Probar cv logarítmico #
#########################

for (eclac_option in c("chile", "eclac_2020", "eclac_2023")) {
  test2 <- create_prop("desocupado", design = dc_ene, log_cv = TRUE, eclac_input = eclac_option)
  test2 <- create_prop("desocupado", domains = "region", design = dc_ene, log_cv = TRUE, eclac_input = eclac_option)
  test2 <- create_prop("desocupado", domains = "region", subpop = "fdt", design = dc_ene, log_cv = TRUE, eclac_input = eclac_option)
  test2 <- create_prop("desocupado", domains = "region+sexo", design = dc_ene, log_cv = TRUE, eclac_input = eclac_option)
}

##########################################
# Probar alcance de nombres entre variables
###########################################

for (eclac_option in c("chile", "eclac_2020", "eclac_2023")) {
  create_prop(var = "desocupado", domains = "sexo+region", design = dc_ene, eclac_input = eclac_option)
}

######################################################
# comparamos valores entre create_prop y create_mean #
######################################################

for (eclac_option in c("chile", "eclac_2020", "eclac_2023")) {
  test_prop <- create_prop("desocupado", design = dc_ene, eclac_input = eclac_option)
  test_media <- suppressWarnings({create_mean("desocupado", design = dc_ene, eclac_input = eclac_option)})

  test_that(paste("estadísticos similares entre prop y mean con", eclac_option), {
    expect_equal(test_prop %>% dplyr::select(stat) %>% dplyr::pull(), test_media %>% dplyr::select(stat) %>% dplyr::pull())
    expect_equal(test_prop %>% dplyr::select(se) %>% dplyr::pull(), test_media %>% dplyr::select(se) %>% dplyr::pull())
    expect_equal(test_prop %>% dplyr::select(df) %>% dplyr::pull(), test_media %>% dplyr::select(df) %>% dplyr::pull())
    expect_equal(test_prop %>% dplyr::select(n) %>% dplyr::pull(), test_media %>% dplyr::select(n) %>% dplyr::pull())
    expect_equal(test_prop %>% dplyr::select(cv) %>% dplyr::pull(), test_media %>% dplyr::select(cv) %>% dplyr::pull())
  })
}

#######################################################
##### testing outputs names                       #####
#######################################################

for (eclac_option in c("chile")) {
  nombre_error <- create_prop(var = "desocup",
                              denominator = "fdt",
                              domains = "ext",
                              subpop = "fdtx",
                              design = dc_ene,
                              eclac_input = eclac_option) %>% names()

  nombre_bien <- create_prop(var = "desocup",
                             denominator = "fdt",
                             subpop = "ext",
                             design = dc_ene,
                             eclac_input = eclac_option) %>% names

  test_that(paste("comparando nombres con", eclac_option), {
    expect_equal(all(nombre_bien %in% nombre_error), TRUE)
  })
}

######################################################
# create_prop para variables con valores igual a 0   #
######################################################

for (eclac_option in c("chile")) {
  test_that(paste("comparando estimaciones con filas igual a 0 con", eclac_option), {
    expect_equal(create_prop(var = "fdtx",
                             domains = "desocup",
                             subpop = "fdt",
                             design = dc_ene,
                             eclac_input = eclac_option),

                 create_prop(var = "fdtx",
                             denominator = "fdt",
                             domains = "desocup",
                             subpop = "fdt",
                             design = dc_ene,
                             eclac_input = eclac_option))
  })
}

for (eclac_option in c("eclac_2020", "eclac_2023")) {
  test_that(paste("comparando estimaciones con filas igual a 0 con", eclac_option), {
    expect_equal(create_prop(var = "fdtx",
                             domains = "desocup",
                             subpop = "fdt",
                             design = dc_ene,
                             eclac_input = eclac_option),
                 create_prop(var = "fdtx",
                             domains = "desocup",
                             subpop = "fdt",
                             design = dc_ene,
                             eclac_input = eclac_option))
  })
}



