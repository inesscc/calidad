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

statOwn <- create_size("ocupado", design = dc_epf_personas)

stat <- sum(epf_personas$ocupado * epf_personas$fe)

test_that("verificación estimación sin desagregación", {
  expect_equal(stat, statOwn$stat)
})

### con desagregación

statOwn <- create_size("ocupado",domains = "zona" ,design = dc_epf_personas)

stat <- epf_personas %>%
  dplyr::group_by(ocupado,zona) %>%
  dplyr::summarize(n = sum(ocupado * fe)) %>%
  dplyr::filter(ocupado == 1) %>% dplyr::pull(n)

test_that("verificación estimación con desagregación", {
  expect_equal(stat, statOwn$stat)
})

################################
# Testear los grados de libertad
################################

# Calcular con el enfoque INE, ya que la opción por defecto es INE, con desagregación

df <-  create_size("ocupado", domains =  "zona+sexo", design = dc_epf_hogar)

true_upm <- dc_epf_hogar$variables %>%
  dplyr::group_by(sexo, zona, ocupado, varunit) %>%
  dplyr::mutate(upm = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(sexo, zona, ocupado) %>%
  dplyr::summarise(upm = sum(upm))

true_strata <- dc_epf_hogar$variables %>%
  dplyr::group_by(sexo, zona, ocupado, varstrat) %>%
  dplyr::mutate(strata = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(sexo, zona,ocupado) %>%
  dplyr::summarise(strata = sum(strata))

true_df <- true_upm %>%
  dplyr::left_join(true_strata, by = c("sexo", "zona", "ocupado")) %>%
  dplyr::mutate(df = upm - strata) %>%
  dplyr::filter(ocupado == 1)

test_that("conteo df diseño complejo, versión INE, con desagregación", {
  expect_equal(true_df$df, df$df)
})

# Calcular con el enfoque de CEPAL con desagregación

df <-  create_size("VA_DC", domains =  "enc_region+rph_sexo", design = dc_enusc, df_type = "eclac")

true_upm <- dc_enusc$variables %>%
  dplyr::group_by(rph_sexo, enc_region, Conglomerado) %>%
  dplyr::mutate(upm = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(rph_sexo, enc_region) %>%
  dplyr::summarise(upm = sum(upm))

true_strata <- dc_enusc$variables %>%
  dplyr::group_by(rph_sexo, enc_region, VarStrat) %>%
  dplyr::mutate(strata = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(rph_sexo, enc_region) %>%
  dplyr::summarise(strata = sum(strata))

true_df <- true_upm %>%
  dplyr::left_join(true_strata, by = c("rph_sexo", "enc_region")) %>%
  dplyr::mutate(df = upm - strata)

test_that("conteo df diseño complejo, versión CEPAL, con desagregación", {
  expect_equal(true_df$df, df$df)
})


# Get df with INE approach

df <-  create_size("VA_DC", domains =  "enc_region+rph_sexo", design = dc_enusc,df_type = "ine")

true_upm <- dc_enusc$variables %>%
  dplyr::group_by(rph_sexo, enc_region, VA_DC, Conglomerado) %>%
  dplyr::mutate(upm = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(rph_sexo, enc_region, VA_DC) %>%
  dplyr::summarise(upm = sum(upm))

true_strata <- dc_enusc$variables %>%
  dplyr::group_by(rph_sexo, enc_region, VA_DC, VarStrat) %>%
  dplyr::mutate(strata = dplyr::if_else(dplyr::row_number() == 1, 1, 0 )) %>%
  dplyr::group_by(rph_sexo, enc_region, VA_DC) %>%
  dplyr::summarise(strata = sum(strata))

true_df <- true_upm %>%
  dplyr::left_join(true_strata, by = c("rph_sexo", "enc_region", "VA_DC")) %>%
  dplyr::mutate(df = upm - strata) %>%
  dplyr::filter(VA_DC == 1)

test_that("conteo df diseño complejo, versión CEPAL, con desagregación", {
  expect_equal(true_df$df, df$df)
})




# Calcular con el enfoque de INE sin desagregación

df <- create_size("ocupado", design = dc_epf_hogar)

true_df <- dc_epf_hogar$variables %>%
  dplyr::filter(ocupado == 1) %>% dplyr::group_by(varunit) %>% dplyr::slice(1) %>%
  nrow()-dc_epf_hogar$variables %>%
  dplyr::filter(ocupado == 1) %>% dplyr::group_by(varstrat) %>% dplyr::slice(1) %>%
  #dplyr::summarise(strata = unique(varstrat)) %>%
  nrow()

test_that("conteo df diseño complejo, version INE, sin desagregación", {
  expect_equal(true_df, df$df[1])
})

# # Calcular con el enfoque de CEPAL sin desagregación
#
# df <- create_size("ocupado", design = dc_epf_hogar)
#
# true_df <- dc_epf_hogar$variables %>%
#   dplyr::summarise(strata = unique(varunit)) %>%
#   nrow()-dc_epf_hogar$variables %>%
#   dplyr::summarise(strata = unique(varstrat)) %>%
#   nrow()
#
# test_that("conteo df diseño complejo, version CEPAL, sin desagregación", {
#   expect_equal(true_df, df$df[1])
# })

