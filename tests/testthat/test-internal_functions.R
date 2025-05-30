
context("test-internal_functions")


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


# disenio con as_survey_design
dc_as_survey_enusc <- enusc_2023 %>%
  mutate(id = Conglomerado) %>%
  srvyr::as_survey_design(id, strata = VarStrat, weights = Fact_Hog_Reg)

# disenio con update
dc_update <- update(dc_as_survey_enusc, aux=VH_DV + VH_DC )

# disenio con transformacion (simil a update)
dc_transform <- transform(dc_as_survey_enusc, aux=VH_DV + VH_DC)

# disenio sin conglomerado con as_survey_design
dc_as_survey_enusc_sin <- enusc_2023 %>%
  dplyr::mutate(id = Conglomerado) %>%
  srvyr::as_survey_design(1, strata = VarStrat, weights = Fact_Hog_Reg)

# disenio sin conglomerado
dc_sin_conglomerado <- survey::svydesign(ids = ~1, strata = ~VarStrat, weights = ~Fact_Hog_Reg,
                                         data = enusc_2023 %>% dplyr::mutate(id = Conglomerado))

# disenio sin conglomerado con as_survey_design ~0
dc_as_survey_enusc_sin0 <- enusc_2023 %>%
  dplyr::mutate(id = Conglomerado) %>%
  srvyr::as_survey_design(0, strata = VarStrat, weights = Fact_Hog_Reg)

# disenio sin conglomerado ~0
dc_sin_conglomerado0 <- survey::svydesign(ids = ~0, strata = ~VarStrat, weights = ~Fact_Hog_Reg,
                                         data = enusc_2023 %>% dplyr::mutate(id = Conglomerado))


## disenio con subset

dc_epf <- survey::svydesign(ids = ~varunit,
                            data = epf_personas %>%
                            dplyr::group_by(folio) %>%
                            dplyr::slice(1) %>%
                            dplyr::ungroup() %>%
                            dplyr::mutate(
                              metro = dplyr::if_else(zona == 1, 1, 0),
                              metro_na = dplyr::if_else(dplyr::row_number() <= 10, NA_real_, metro )),
                          strata = ~varstrat,
                          weights = ~fe)

dc_epf_subset <- subset(dc_epf, gastot_hd> 426548)   ## filtramos valores sobre el Q1 para ej

#####################
# GET_SAMPLE_SIZE
#####################

# Desagregación en el caso normal
agrupacion <- c("sexo", "zona")
n <- get_sample_size(dc$variables, agrupacion)
true_n <- dc$variables %>%
  dplyr::group_by(sexo, zona) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::ungroup()


test_that("conteo n agrupado", {
  expect_equal(n$n, true_n$n)
})

# Desagregación en el caso especial de size chile
agrupacion <- c("sexo", "zona", "ocupado")
n <- get_sample_size(dc$variables, agrupacion, df_type = "chile")

true_n <- dc$variables %>%
  dplyr::group_by(sexo, zona, ocupado) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(ocupado == 1)


test_that("conteo n agrupado caso especial", {
  expect_equal(n$n, true_n$n)
})


# Sin desagregación en el caso normal
agrupacion <- NULL
n <- get_sample_size(dc$variables, agrupacion)

true_n <- nrow(dc$variables)

test_that("conteo n sin agrupar", {
  expect_equal(n$n[1], true_n)
})

# Sin desagregación en el caso especial size-chile
domains <- NULL
agrupacion <- c( "ocupado")
agrupacion <- c(domains, agrupacion)
n <- get_sample_size(dc$variables, agrupacion, df_type = "chile" )

true_n <- dc$variables %>%
  dplyr::filter(ocupado == 1) %>%
  nrow()

test_that("conteo n sin agrupar", {
  expect_equal(n$n[1], true_n)
})



#####################
# PROBAR GET_DF
#####################

# Con diseño complejo caso normal y dominios
agrupacion <- c("sexo", "zona")
df <- get_df(dc, agrupacion)

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

test_that("conteo df diseño complejo", {
  expect_equal(true_df$df, df$df)
})

# Con diseño complejo caso chile-size y dominios
agrupacion <- c("sexo", "zona", "ocupado")
df <- get_df(dc, agrupacion, df_type = "chile")

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

test_that("conteo df diseño complejo", {
  expect_equal(true_df$df, df$df)
})


# Con diseño complejo caso normal SIN dominios
agrupacion <- NULL
df <- get_df(dc, agrupacion)

true_upm <- length(unique(dc$variables$varunit))
true_strata <- length(unique(dc$variables$varstrat))

true_df <- true_upm - true_strata

test_that("conteo df sin dominios", {
  expect_equal(true_df, df[[1]])
})

# Con diseño complejo caso especial chile-size sin dominios
agrupacion <- NULL
var <- "desocupado"
dc_filtered <-  dc_ene[dc_ene$variables[["fdt"]] == 1]
dc_filtered <- standardize_design_variables(dc_filtered)

agrupacion <- c(agrupacion, var)
df <- get_df(dc_filtered, agrupacion, df_type = "chile")

true_upm <- length(unique(dc_filtered$variables$conglomerado[dc_filtered$variables$desocupado == 1]))
true_strata <- length(unique(dc_filtered$variables$estrato_unico[dc_filtered$variables$desocupado == 1]))

true_df <- true_upm - true_strata

test_that("conteo df sin dominios caso especial", {
  expect_equal(true_df, df %>% dplyr::pull(df))
})


# Sin diseño complejo
df <- get_df(dc_sin_varunit, agrupacion)

test_that("conteo df sin diseño complejo", {
  expect_equal(df$df[1], NA)
})



##############################################
## Creacion del disenio con otras funciones ##
##############################################

### usando disenio con update/ transform
test_that("comparar resultado update con transform indicando cluster", {
  expect_equal(create_prop('VH_DV', domains = 'enc_region', design = dc_update),
               create_prop('VH_DV', domains = 'enc_region', design = dc_transform))
  })

test_that("revision mensaje por no usar funcion svydesign",{
  expect_message(create_prop('VH_DV', domains = 'enc_region', design = dc_update),
                          'Complex design with modifications')
  })


### usando dc sin conglomerado ~1
test_that('muestra sin conglomerado',{
  expect_warning(create_prop('VH_DV', domains = 'enc_region', design = dc_sin_conglomerado))
  })

test_that('muestra sin conglomerado df',{
  expect_equal(get_df(dc_sin_conglomerado, NULL)[[1]], NA)
})


### usando dc sin conglomerado as_survey_design ~1
test_that('muestra sin conglomerado con as_survey_design',{
  expect_warning(create_prop('VH_DV', domains = 'enc_region', design = dc_as_survey_enusc_sin))
})

test_that('muestra sin conglomerado as_survey_design',{
  expect_equal(get_df(dc_as_survey_enusc_sin, NULL)[[1]], NA)
})


### usando dc sin conglomerado as_survey_design ~0
test_that('muestra sin conglomerado con as_survey_design',{
  expect_warning(create_prop('VH_DV', domains = 'enc_region', design = dc_as_survey_enusc_sin0))
})

test_that('muestra sin conglomerado as_survey_design',{
  expect_equal(get_df(dc_as_survey_enusc_sin0, NULL)[[1]], NA)
})

test_that('muestra sin conglomerado con as_survey_design',{
  expect_warning(create_prop('VH_DV', domains = 'enc_region', design = dc_sin_conglomerado0))
})

### usando dc con modificacion subset
test_that("revision mensaje por no usar funcion svydesign (subset)",{
  expect_message(create_mean('gastot_hd', design = dc_epf_subset),
                 'Complex design with modifications')
})

