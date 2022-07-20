context("test-create_size")


options(survey.lonely.psu = "certainty")

# Diseño complejo con varstrat y varunit
dc <- survey::svydesign(ids = ~varunit,
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




#################################
# Testear los grados de libertad
#################################

# Calcular con el enfoque INE, ya que la opción por defecto es INE

df <-  create_size("ocupado", dominios =  "zona+sexo", disenio = dc)

true_upm <- dc$variables %>%
  dplyr::group_by(sexo, zona, ocupado, varunit) %>%
  dplyr::mutate(upm = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(sexo, zona, ocupado) %>%
  dplyr::summarise(upm = sum(upm))

true_strata <- dc$variables %>%
  dplyr::group_by(sexo, zona, ocupado, varstrat) %>%
  dplyr::mutate(strata = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(sexo, zona,ocupado) %>%
  dplyr::summarise(strata = sum(strata))

true_df <- true_upm %>%
  dplyr::left_join(true_strata, by = c("sexo", "zona", "ocupado")) %>%
  dplyr::mutate(df = upm - strata) %>%
  dplyr::filter(ocupado == 1)

test_that("conteo df diseño complejo", {
  expect_equal(true_df$df, df$df)
})

# Calcular con el enfoque del INE


