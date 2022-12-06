
context("test-assess_external")


# Diseños muestrales
options(survey.lonely.psu = "certainty")

dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat,
                        data = epf_personas %>% dplyr::group_by(folio) %>% dplyr::slice(1),
                        weights = ~fe)

dc_ene <- survey::svydesign(ids = ~conglomerado, strata = ~estrato_unico, data = ene %>%
                              dplyr::mutate(mujer = dplyr::if_else(sexo == 2, 1, 0),
                                            hombre = dplyr::if_else(sexo == 1, 1, 0),
                                            desocupado = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0)
                                            ),
                            weights = ~fact_cal)



############
# assess #
############

# National level with denominator
expect_error(create_prop(var = "mujer",  denominator = "hombre", design = dc_ene, eclac_input = T),
               "eclac approach is not allowed with denominator")

# INE Chile Standard for mean
test1 <-  create_mean("gastot_hd", domains =  "zona+sexo+ecivil", design = dc, deff = T, ess = T, unweighted = T)
test <- assess(test1, publish = T)


# INE Chile Standard for proportion
test2 <-  create_prop("desocupado", domains =  "region+sexo", design = dc_ene, deff = T, ess = T, log_cv = T, unweighted = T)

x <- survey::svyby(~desocupado, by = ~region+sexo, design = dc_ene, FUN = survey::svymean)
test_cv <- survey::cv(x)

test_that("cv calculado correctamente", {
  expect_equal(sum(test2$cv == test_cv), length(test_cv) )
})



test2_sin_log <-  create_prop("desocupado", domains =  "region+sexo", design = dc_ene, deff = T, ess = T, log_cv = F, unweighted = T)
test <- assess(test2)

# INE Chile Standard for size
test3 <-  create_size("desocupado", domains =  "region", design = dc_ene, deff = T, ess = T, unweighted = T)
test <- assess(test3, publish = T)

test_that("Ningún valor de deff es infinito", {
  expect_equal(sum(test3$deff == Inf) , 0)
})



# INE Chile Standard for total
test4 <-  create_total("gastot_hd", domains =  "zona", design = dc, deff = T, ess = T, unweighted = T)
test_ine <- assess(test4, publish = T)



# CEPAL standard with default parameters
test <- assess(test1, scheme = "eclac")
test <- assess(test2, scheme = "eclac")
test <- assess(test3, scheme = "eclac")
test <- assess(test4, scheme = "eclac")

#

# Proportion without log_cv
expect_error(assess(test2_sin_log, scheme = "eclac"),
             "log_cv must be used!")


eclac <-  create_size("desocupado", domains =  "region", design = dc_ene, deff = T, ess = T,
                      unweighted = T, df_type = "eclac")

test <- assess(eclac, scheme = "eclac", unweighted = 150 )

test_that("se caigan estimaciones por conteo no ponerado en size", {
  expect_equal(sum(test$label == "supress") , 9)

})



# CEPAL standard with custom parameters
test <- assess(test1, scheme = "eclac", unweighted = 500)
test <- assess(test1, scheme = "eclac", ess  = 200 )
test <- assess(test2, scheme = "eclac", ess  = 200, df = 127 )

test_that("NA in label variable", {
  expect_equal( sum(is.na(test$label) == FALSE), dim(test)[1] )
})




# html output
out1 <- create_html(test)
out2 <- create_html(test_ine)
