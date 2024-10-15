context("test-create_size")

options(survey.lonely.psu = "certainty")

# Diseño complejo con varstrat y varunit
dc_epf_hogar <- survey::svydesign(ids = ~varunit,
                                  data = epf_personas %>%
                                    dplyr::group_by(folio) %>%
                                    dplyr::slice(1) %>%
                                    dplyr::ungroup() %>%
                                    dplyr::mutate(
                                      metro = dplyr::if_else(zona == 1, 1, 0),
                                      metro_na = dplyr::if_else(dplyr::row_number() <= 10, NA_real_, metro ),
                                      desocupado = dplyr::if_else(ocupado == 1, 0, 1)
                                    ),
                                  strata = ~varstrat,
                                  weights = ~fe)

# Diseño complejo con varstrat y varunit
dc_epf_personas <- survey::svydesign(ids = ~varunit,
                                     data = epf_personas %>%
                                       dplyr::mutate(
                                         metro = dplyr::if_else(zona == 1, 1, 0),
                                         metro_na = dplyr::if_else(dplyr::row_number() <= 10, NA_real_, metro ),
                                         desocupado = dplyr::if_else(ocupado == 1, 0, 1)
                                       ),
                                     strata = ~varstrat,
                                     weights = ~fe)

# Diseño sin varunit
dc_sin_varunit <- survey::svydesign(ids = ~1,
                                    data = epf_personas %>%
                                      dplyr::group_by(folio) %>%
                                      dplyr::slice(1) %>%
                                      dplyr::ungroup() %>%
                                      dplyr::mutate(
                                        metro = dplyr::if_else(zona == 1, 1, 0),
                                        metro_na = dplyr::if_else(dplyr::row_number() <= 10, NA_real_, metro ),
                                        desocupado = dplyr::if_else(ocupado == 1, 0, 1)
                                      ),
                                    weights = ~fe)

# Diseño ene
ene <- ene %>%
  dplyr::mutate(fdt = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 9, 1, 0),
                ocupado = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0),
                desocupado = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0),
                hombre = dplyr::if_else(sexo == 1, 1, 0),
                mujer = dplyr::if_else(sexo == 2, 1, 0)
  )

dc_ene <- survey::svydesign(ids = ~conglomerado, strata = ~estrato_unico, data = ene, weights = ~fact_cal)

# Diseño enusc
dc_enusc <- survey::svydesign(ids = ~Conglomerado,
                              strata = ~VarStrat,
                              data = enusc %>% dplyr::mutate(enc_region = as.character(enc_region)),
                              weights = ~Fact_Pers)

##########################
# Testear las estimaciones
##########################

### sin desagregación
for (eclac_option in c(FALSE, TRUE)) {
  for (df_option in c("chile", "eclac")) {
    statOwn <- create_size("ocupado", design = dc_epf_personas, eclac_input = eclac_option, df_type = df_option)

    stat <- sum(epf_personas$ocupado * epf_personas$fe)

    test_that(paste("verificación estimación sin desagregación con eclac_input ", eclac_option, "y df_type", df_option), {
      expect_equal(stat, statOwn$stat)
    })
  }
}

### con desagregación
for (eclac_option in c(FALSE, TRUE)) {
  for (df_option in c("chile", "eclac")) {
    statOwn <- create_size("ocupado", domains = "zona", design = dc_epf_personas, eclac_input = eclac_option, df_type = df_option)

    stat <- epf_personas %>%
      dplyr::group_by(ocupado, zona) %>%
      dplyr::summarize(n = sum(ocupado * fe)) %>%
      dplyr::filter(ocupado == 1) %>% dplyr::pull(n)

    test_that(paste("verificación estimación con desagregación con eclac_input", eclac_option, "y df_type", df_option), {
      expect_equal(stat, statOwn$stat)
    })
  }
}

################################
# Testear los grados de libertad
################################

# Calcular con el enfoque chile, ya que la opción por defecto es chile, con desagregación
for (eclac_option in c(FALSE, TRUE)) {
  for (df_option in c("chile", "eclac")) {
    df <- create_size("ocupado", domains = "zona+sexo", design = dc_epf_hogar, eclac_input = eclac_option, df_type = df_option)

    true_upm <- dc_epf_hogar$variables %>%
      dplyr::group_by(sexo, zona, ocupado, varunit) %>%
      dplyr::mutate(upm = dplyr::if_else(dplyr::row_number() == 1, 1, 0)) %>%
      dplyr::group_by(sexo, zona, ocupado) %>%
      dplyr::summarise(upm = sum(upm))

    true_strata <- dc_epf_hogar$variables %>%
      dplyr::group_by(sexo, zona, ocupado, varstrat) %>%
      dplyr::mutate(strata = dplyr::if_else(dplyr::row_number() == 1, 1, 0)) %>%
      dplyr::group_by(sexo, zona, ocupado) %>%
      dplyr::summarise(strata = sum(strata))

    true_df <- true_upm %>%
      dplyr::left_join(true_strata, by = c("sexo", "zona", "ocupado")) %>%
      dplyr::mutate(df = upm - strata) %>%
      dplyr::filter(ocupado == 1)

    test_that(paste("conteo df diseño complejo, versión eclac_input ", eclac_option, "con desagregación y df_type", df_option), {
      expect_equal(true_df$df, df$df)
    })
  }
}

# Calcular con el enfoque sin desagregación
for (eclac_option in c(FALSE, TRUE)) {
  for (df_option in c("chile", "eclac")) {
    df <- create_size("ocupado", design = dc_epf_hogar, eclac_input = eclac_option, df_type = df_option)

    if (df_option == "chile") {
      true_df <- dc_epf_hogar$variables %>%
        dplyr::filter(ocupado == 1) %>%
        dplyr::group_by(varunit) %>%
        dplyr::slice(1) %>%
        nrow() - dc_epf_hogar$variables %>%
        dplyr::filter(ocupado == 1) %>%
        dplyr::group_by(varstrat) %>%
        dplyr::slice(1) %>%
        nrow()
    } else {
      true_df <- dc_epf_hogar$variables %>%
        dplyr::reframe(strata = unique(varunit)) %>%
        nrow() - dc_epf_hogar$variables %>%
        dplyr::reframe(strata = unique(varstrat)) %>%
        nrow()
    }

    test_that(paste("conteo df diseño complejo, versión eclac_input ", eclac_option, "sin desagregación y df_type", df_option), {
      expect_equal(true_df, df$df[1])
    })
  }
}


######################
# Confidence intervals
######################


df_chile <- create_size("ocupado", domains = "zona+sexo", design = dc_epf_hogar, eclac_input = F, df_type = 'chile', ci = T)
test_that("ci cols chile", {
  expect_equal(sum(names(df_chile) %in% c('t', 'lower', 'upper')), length(c('t', 'lower', 'upper')))
})


df_eclac <- create_size("ocupado", domains = "zona+sexo", design = dc_epf_hogar, eclac_input = T, df_type = 'eclac', ci = T)
test_that("ci cols eclac", {
  expect_equal(sum(names(df_eclac) %in% c('t', 'lower', 'upper')), length(c('t', 'lower', 'upper')))
})


