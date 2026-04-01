
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
  dplyr::mutate(id = Conglomerado) %>%
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


dc_enusc <- svydesign(ids = ~Conglomerado,
                      weights = ~Fact_Hog_Reg,    # fexp a nivel regional
                      strata = ~VarStrat,
                      check.strata = TRUE,
                      data = enusc_2023 %>%
                        dplyr::mutate(enc_region  = as.character(enc_region )))

dc_ele <- svydesign(ids = ~rol_ficticio, weights = ~fe_transversal, strata = ~estrato, fpc = ~pob, data = ELE7)

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


### usando dc sin conglomerado ~1  -------
test_that('muestra sin conglomerado',{
  expect_warning(create_prop('VH_DV', domains = 'enc_region', design = dc_sin_conglomerado))
  })


## calculo manual de gl sin conglomerados
gl <- enusc_2023 %>%
  dplyr::group_by(VarStrat) %>%
  dplyr::summarise(n_muestra=dplyr::n(), N_Marco=sum(Fact_Hog_Reg)) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(n=sum(n_muestra),
                   est=dplyr::n()) %>%
  dplyr::mutate(gl=n-est)


# estandarizacion de nombres
dc_sin_conglomerado <- standardize_design_variables(dc_sin_conglomerado)
names(dc_sin_conglomerado$variables) <- tolower(names(dc_sin_conglomerado$variables))


test_that('muestra sin conglomerado df',{
  expect_equal(get_df(dc_sin_conglomerado, NULL)[[1]], gl$gl)
})


### usando dc sin conglomerado as_survey_design ~1 -------
test_that('muestra sin conglomerado con as_survey_design',{
  expect_warning(create_prop('VH_DV', domains = 'enc_region', design = dc_as_survey_enusc_sin))
})



## calculo manual de gl sin conglomerados
gl_as_survey <- enusc_2023 %>%
  dplyr::group_by(VarStrat) %>%
  dplyr::summarise(n_muestra=dplyr::n(), N_Marco=sum(Fact_Hog_Reg)) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(n=sum(n_muestra),
            est=dplyr::n()) %>%
  dplyr::mutate(gl=n-est)



## estandarizacion de nombres
dc_as_survey_enusc_sin <- standardize_design_variables(dc_as_survey_enusc_sin)
names(dc_as_survey_enusc_sin$variables) <- tolower(names(dc_as_survey_enusc_sin$variables))


test_that('muestra sin conglomerado as_survey_design',{
  expect_equal(get_df(dc_as_survey_enusc_sin, NULL)[[1]], gl_as_survey$gl)
})


### usando dc sin conglomerado as_survey_design ~0 -------
test_that('muestra sin conglomerado con as_survey_design',{
  expect_warning(create_prop('VH_DV', domains = 'enc_region', design = dc_as_survey_enusc_sin0))
})


## calculo manual de gl sin conglomerados
gl_as_survey_0 <- enusc_2023 %>%
  dplyr::group_by(VarStrat) %>%
  dplyr::summarise(n_muestra=dplyr::n(), N_Marco=sum(Fact_Hog_Reg)) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(n=sum(n_muestra),
            est=dplyr::n()) %>%
  dplyr::mutate(gl=n-est)

## estandarizacion de nombres
dc_as_survey_enusc_sin0 <- standardize_design_variables(dc_as_survey_enusc_sin0)
names(dc_as_survey_enusc_sin0$variables) <- tolower(names(dc_as_survey_enusc_sin0$variables))


test_that('muestra sin conglomerado as_survey_design',{
  expect_equal(get_df(dc_as_survey_enusc_sin0, NULL)[[1]], gl_as_survey_0$gl)
})

test_that('muestra sin conglomerado con as_survey_design',{
  expect_warning(create_prop('VH_DV', domains = 'enc_region', design = dc_sin_conglomerado0))
})

### usando dc con modificacion subset
test_that("revision mensaje por no usar funcion svydesign (subset)",{
  expect_message(create_mean('gastot_hd', design = dc_epf_subset),
                 'Complex design with modifications')
})




###############################################
## create_internal_estimations se, cv y deff ##
###############################################

expect_match_survey <- function(survey_expr,
                                own_expr,
                                criterios = c("adjust","average","remove","certainty")
                                   ){

  old_opt <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = old_opt)

  for (criterio in criterios) {

    options(survey.lonely.psu = criterio)

    table_svy <- eval(survey_expr)
    table_own <- eval(own_expr)

    ## estimation
    expect_equal(
      table_own$est,
      unname(coef(table_svy)),
      info = paste("criterio:", criterio)
    )

    ## SE
    expect_equal(
      table_own$se,
      unname(SE(table_svy)),
      info = paste("criterio:", criterio)
    )

    expect_equal(
      table_own$deff,
      unname(deff(table_svy)),
      info = paste("criterio:", criterio)
    )
  }
}

## TEST ELE ##
#### ratio
test_that("get_ratio vs svyratio por dominios cod_actividad + cod_tamano + tramo", {

  expect_match_survey(
    survey_expr = quote(
      svyby(~VA_2022f, denominator = ~REMP_TOTAL, ~cod_actividad + cod_tamano + tramo,
            design = dc_ele, FUN = svyratio, deff = T)),
    own_expr = quote(
      get_FUN_domain( ~VA_2022f, denominator = ~REMP_TOTAL, domains = ~cod_actividad + cod_tamano + tramo,
                      design = dc_ele, fun_est = get_ratio, deff = T)
      )
    )
  })

test_that("get_ratio vs svyratio por dominios cod_actividad", {

  expect_match_survey(
    survey_expr = quote(
      svyby(~VA_2022f, denominator = ~REMP_TOTAL, ~cod_actividad,
            design = dc_ele, FUN = svyratio, deff = T)),
    own_expr = quote(
      get_FUN_domain( ~VA_2022f, denominator = ~REMP_TOTAL, domains = ~cod_actividad,
                      design = dc_ele, fun_est = get_ratio, deff = T)
    )
  )
})


#### total
test_that("get_total vs svytotal por dominios cod_actividad + cod_tamano + tramo", {

  expect_match_survey(
    survey_expr = quote(
      svyby(~VA_2022f, ~cod_actividad + cod_tamano + tramo,
            design = dc_ele, FUN = svytotal, deff = T)),
    own_expr = quote(
      get_FUN_domain( ~VA_2022f, domains = ~cod_actividad + cod_tamano + tramo,
                      design = dc_ele, fun_est = get_total, deff = T)
    )
  )
})

test_that("get_total vs svytotal por dominios cod_actividad", {

  expect_match_survey(
    survey_expr = quote(
      svyby(~VA_2022f, ~cod_actividad,
            design = dc_ele, FUN = svytotal, deff = T)),
    own_expr = quote(
      get_FUN_domain( ~VA_2022f, domains = ~cod_actividad,
                      design = dc_ele, fun_est = get_total, deff = T)
    )
  )
})

#### mean
test_that("get_mean vs svymean por dominios cod_actividad + cod_tamano + tramo", {

  expect_match_survey(
    survey_expr = quote(
      svyby(~VA_2022f, ~cod_actividad + cod_tamano + tramo,
            design = dc_ele, FUN = svymean, deff = T)),
    own_expr = quote(
      get_FUN_domain( ~VA_2022f, domains = ~cod_actividad + cod_tamano + tramo,
                      design = dc_ele, fun_est = get_mean, deff = T)
    )
  )
})

test_that("get_mean vs svymean por dominios cod_actividad", {

  expect_match_survey(
    survey_expr = quote(
      svyby(~VA_2022f, ~cod_actividad,
            design =  dc_ele , FUN = svymean, deff = T)),
    own_expr = quote(
      get_FUN_domain( ~VA_2022f, domains = ~cod_actividad,
                      design = dc_ele, fun_est = get_mean, deff = T)
    )
  )
})

## TEST EPF ##
#### mean
test_that("get_mean vs svymean por dominios zona+sexo+ecivil", {

  expect_match_survey(
    survey_expr = quote(
      svyby(~gastot_hd, ~zona+sexo+ecivil,
            design = dc_epf, FUN = svymean, deff = T)),
    own_expr = quote(
      get_FUN_domain( ~gastot_hd, domains = ~zona+sexo+ecivil,
                      design = dc_epf, fun_est = get_mean, deff = T)
    )
  )
})

test_that("get_mean vs svymean por dominios zona", {

  expect_match_survey(
    survey_expr = quote(
      svyby(~gastot_hd, ~zona,
            design = dc_epf, FUN = svymean, deff = T)),
    own_expr = quote(
      get_FUN_domain( ~gastot_hd, domains = ~zona,
                      design = dc_epf, fun_est = get_mean, deff = T)
    )
  )
})


#### total
test_that("get_total vs svytotal por dominios zona+sexo+ecivil", {

  expect_match_survey(
    survey_expr = quote(
      svyby(~gastot_hd, ~zona+sexo+ecivil,
            design = dc_epf, FUN = svytotal, deff = T)),
    own_expr = quote(
      get_FUN_domain( ~gastot_hd, domains = ~zona+sexo+ecivil,
                      design = dc_epf, fun_est = get_total, deff = T)
    )
  )
})

test_that("get_total vs svytotal por dominios zona", {

  expect_match_survey(
    survey_expr = quote(
      svyby(~gastot_hd, ~zona,
            design = dc_epf, FUN = svytotal, deff = T)),
    own_expr = quote(
      get_FUN_domain( ~gastot_hd, domains = ~zona,
                      design = dc_epf, fun_est = get_total, deff = T)
    )
  )
})


## TEST ENUSC ##
#### mean
test_that("get_mean vs svymean por dominios ~enc_region+rph_sexo", {

  expect_match_survey(
    survey_expr = quote(
      svyby(~VH_DV, ~enc_region+rph_sexo,
            design = dc_enusc, FUN = svymean, deff = T)),
    own_expr = quote(
      get_FUN_domain( ~VH_DV, domains = ~enc_region+rph_sexo,
                      design = dc_enusc, fun_est = get_mean, deff = T)
    )
  )
})

test_that("get_mean vs svymean por dominios ~rph_sexo", {

  expect_match_survey(
    survey_expr = quote(
      svyby(~VH_DV, ~rph_sexo,
            design = dc_enusc, FUN = svymean, deff = T)),
    own_expr = quote(
      get_FUN_domain( ~VH_DV, domains = ~rph_sexo,
                      design = dc_enusc, fun_est = get_mean, deff = T)
    )
  )
})


