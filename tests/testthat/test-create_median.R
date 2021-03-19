
context("test-create_median")

dc <- survey::svydesign(ids = ~varunit,
                        strata = ~varstrat,
                        data = epf_personas %>% dplyr::group_by(folio) %>% dplyr::slice(1),
                        weights = ~fe)
n_rep <- 10
set.seed(1234)
dc_rep <- survey::as.svrepdesign(dc , type = "subbootstrap", replicates=n_rep)
options(survey.lonely.psu = "certainty")



#########################################
# MEDIANA SIN DESAGREGACIÓN Y SIN SUBPOP#
#########################################

test <-  create_median(gastot_hd, replicas = 10, disenio = dc)

real <- survey::svyquantile(~gastot_hd,
            design = dc_rep,
            quantile = 0.5,
            method="constant",
            interval.type = "quantile",
            ties="discrete")

test_that("mediana agregada sin subpop", {
  expect_equal(real[[1]], test[[1]])
})


#########################################
# MEDIANA CON DESAGREGACIÓN Y SIN SUBPOP#
########################################
test <-  create_median(gastot_hd, dominios = sexo+zona,  replicas = 10, disenio = dc)


real <-  survey::svyby(~gastot_hd,
                   ~sexo+zona,
                   FUN =  survey::svyquantile,
                   design = dc_rep,
                   quantile = 0.5,
                   method="constant",
                   interval.type = "quantile",
                   ties="discrete",
                   ci = T)

test_that("mediana desagregada sin subpop", {
  expect_equal(real$V1[1], test$gastot_hd[1])
})


##########################################
# MEDIANA SIN DESAGREGACIÓN Y CON SUBPOP#
##########################################

test <-  create_median(gastot_hd, subpop = ocupado,  replicas = 10, disenio = dc)

real <- survey::svyquantile(~gastot_hd,
                            design = subset(dc_rep, ocupado == 1),
                            quantile = 0.5,
                            method="constant",
                            interval.type = "quantile",
                            ties="discrete")


test_that("mediana agregada con subpop", {
  expect_equal(real[1], test$quantiles[1])
})


##########################################
# MEDIANA CON DESAGREGACIÓN Y CON SUBPOP#
##########################################

test <-  create_median(gastot_hd, dominios = sexo+zona, subpop = ocupado,  replicas = 10, disenio = dc)

real <-  survey::svyby(~gastot_hd,
                       ~sexo+zona,
                       FUN =  survey::svyquantile,
                       design = subset(dc_rep, ocupado == 1),
                       quantile = 0.5,
                       method="constant",
                       interval.type = "quantile",
                       ties="discrete",
                       ci = T)

test_that("mediana desagregada con subpop", {
  expect_equal(real$V1[1], test$gastot_hd[1])
})


################################################
# ERROR ESTÁNDAR CON DESAGREGACIÓN Y CON SUBPOP#
################################################

test <-  create_median(gastot_hd, dominios = sexo+zona, subpop = ocupado,  replicas = 10, disenio = dc)

real <-  survey::svyby(~gastot_hd,
                       ~sexo+zona,
                       FUN =  survey::svyquantile,
                       design = subset(dc_rep, ocupado == 1),
                       quantile = 0.5,
                       method="constant",
                       interval.type = "quantile",
                       ties="discrete",
                       ci = T)

test_that("se desagregado con subpop", {
  expect_equal(real$se[1], test$se[1])
})






