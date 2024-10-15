context("test-create_total")

####################
# DECLARAR DISEÑOS #
####################

options(survey.lonely.psu = "certainty")

# Diseño complejo con varstrat y varunit
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

# Diseño sin varunit
dc_sin_varunit <- survey::svydesign(ids = ~1,
                                    data = epf_personas %>%
                                      dplyr::group_by(folio) %>%
                                      dplyr::slice(1) %>%
                                      dplyr::ungroup() %>%
                                      dplyr::mutate(
                                        metro = dplyr::if_else(zona == 1, 1, 0),
                                        metro_na = dplyr::if_else(dplyr::row_number() <= 10, NA_real_, metro )),
                                    weights = ~fe)


#####################
# Testear estimación
#####################

# Probar suma con desagregación para las tres opciones de eclac_input
for (eclac_option in c(FALSE, TRUE)) {
  suma <- create_total("gastot_hd", domains = "zona", design = dc, eclac_input = eclac_option) %>%
    dplyr::pull(stat)

  # valor real de la suma
  suma_real <- dc$variables %>%
    dplyr::group_by(zona) %>%
    dplyr::summarise(stat = sum(fe * gastot_hd), 3  )   %>%
    dplyr::pull(stat)

  test_that(paste("suma del gasto con eclac_input", eclac_option), {
    expect_equal(suma, suma_real)
  })
}

# Testear suma sin varunit
for (eclac_option in c(FALSE, TRUE)) {
  suma <- expect_warning(
    create_total("gastot_hd", domains = "zona", design = dc_sin_varunit, eclac_input = eclac_option) %>%
      dplyr::pull(stat),
    "se calculated without complex design"
  )

  test_that(paste("suma del gasto sin varunit con eclac_input", eclac_option), {
    expect_equal(suma, suma_real)
  })
}

# Test sin diseño complejo y con domains
for (eclac_option in c(FALSE, TRUE)) {
  suma2 <- expect_warning(
    create_total("gastot_hd", domains = "zona", design = dc_sin_varunit, eclac_input = eclac_option) %>%
      dplyr::pull(stat)
  )

  test_that(paste("suma del gasto sin diseño complejo con eclac_input", eclac_option), {
    expect_equal(suma2, suma_real)
  })
}

# Test sin diseño complejo a nivel nacional
for (eclac_option in c(FALSE, TRUE)) {
  suma <- expect_warning(
    create_total("gastot_hd", design = dc_sin_varunit, eclac_input = eclac_option) %>%
      dplyr::pull(stat)
  )

  suma_real <- sum(dc_sin_varunit$variables$fe * dc_sin_varunit$variables$gastot_hd)

  test_that(paste("suma del gasto nivel nacional con eclac_input", eclac_option), {
    expect_equal(suma, suma_real)
  })
}

################################################
# Testear grados de libertad y otros parámetros
################################################

## eclac_input == F
all <- create_total("gastot_hd", domains = "zona+sexo", design = dc, eclac_input = F,
                    ci = TRUE, ess = TRUE, deff = TRUE, rm.na = TRUE, unweighted = TRUE, rel_error = TRUE)

# Check column names
waited_output <- c("stat", "se", "n", "cv", "deff", "lower", "upper", "relative_error", "ess", "unweighted")

test_that("suma del gasto nivel nacional con eclac_input ", {
  expect_equal(sum(names(all) %in% waited_output), length(waited_output))
})

# Check df
true_upm <- dc$variables %>%
  dplyr::group_by(sexo, zona, varunit) %>%
  dplyr::mutate(upm = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(sexo, zona) %>%
  dplyr::summarise(upm = sum(upm))

true_strata <- dc$variables %>%
  dplyr::group_by(sexo, zona, varstrat) %>%
  dplyr::mutate(strata = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(sexo, zona) %>%
  dplyr::summarise(strata = sum(strata))

true_df <- true_upm %>%
  dplyr::left_join(true_strata, by = c("sexo", "zona")) %>%
  dplyr::mutate(df = upm - strata)

test_that("conteo df diseño complejo con eclac_input", {
  expect_equal(true_df$df, all$df)
})



## eclac_input == T
all <- create_total("gastot_hd", domains = "zona+sexo", design = dc, eclac_input = T,
                    ci = TRUE, rm.na = TRUE, rel_error = TRUE)

# Check column names
waited_output <- c("stat", "se", "n", "cv", "deff", "lower", "upper", "relative_error", "ess", "unweighted")

test_that("suma del gasto nivel nacional con eclac_input ", {
  expect_equal(sum(names(all) %in% waited_output), length(waited_output))
})

# Check df
true_upm <- dc$variables %>%
  dplyr::group_by(sexo, zona, varunit) %>%
  dplyr::mutate(upm = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(sexo, zona) %>%
  dplyr::summarise(upm = sum(upm))

true_strata <- dc$variables %>%
  dplyr::group_by(sexo, zona, varstrat) %>%
  dplyr::mutate(strata = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(sexo, zona) %>%
  dplyr::summarise(strata = sum(strata))

true_df <- true_upm %>%
  dplyr::left_join(true_strata, by = c("sexo", "zona")) %>%
  dplyr::mutate(df = upm - strata)

test_that("conteo df diseño complejo con eclac_input", {
  expect_equal(true_df$df, all$df)
})



