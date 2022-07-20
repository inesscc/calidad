#
# context("test-create_median")
#
# dc <- survey::svydesign(ids = ~varunit,
#                         strata = ~varstrat,
#                         data = epf_personas %>% dplyr::group_by(folio) %>% dplyr::slice(1),
#                         weights = ~fe)
# n_rep <- 10
# set.seed(1234)
# dc_rep <- survey::as.svrepdesign(dc , type = "subbootstrap", replicates=n_rep)
# options(survey.lonely.psu = "certainty")
#
#
# # Hacer cambios en la ene para hacer los tests
#
# ene <- ene %>%
#   dplyr::mutate(fdt = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 9, 1, 0),
#                 ocupado = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0),
#                 desocupado = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0),
#                 fdt_na = dplyr::if_else(dplyr::row_number() <= 10, NA_real_, fdt ),
#                 region_fact = dplyr::case_when(
#                   region == 1  ~   "Tarapacá",
#                   region == 2  ~   "Antofagasta",
#                   region == 3  ~   "Atacama",
#                   region == 4  ~   "Coquimbo",
#                   region == 5  ~   "Valparaíso",
#                   region == 6  ~   "O'Higgins",
#                   region == 7  ~   "Maule",
#                   region == 8  ~   "Bíobío",
#                   region == 9  ~   "Araucanía",
#                   region == 10  ~   "Los Lagos",
#                   region == 11  ~   "Aysén",
#                   region == 12  ~   "Magallanes",
#                   region == 13  ~   "Metropolitana",
#                   region == 14  ~   "Los Ríos",
#                   region == 15  ~   "Arica",
#                   region == 16  ~   "Ñuble",
#                 ),
#                 region_fact = factor(region_fact, levels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
#                                                              "O'Higgins", "Maule", "Bíobío", "Araucanía", "Los Lagos", "Aysén",
#                                                              "Magallanes", "Metropolitana",  "Los Ríos", "Arica", "Ñuble")),
#                 region_int = as.integer(region_fact)
#   )
#
#
# # Diseño de la ENE
# dc_ene <- survey::svydesign(weights = ~fact_cal, ids = ~conglomerado, strata = ~estrato_unico, data = ene)
#
#
#
#
# #########################################
# # MEDIANA SIN DESAGREGACIÓN Y SIN SUBPOP#
# #########################################
#
# test <-  create_median(gastot_hd, replicas = 10, disenio = dc)
#
# real <- survey::svyquantile(~gastot_hd,
#             design = dc_rep,
#             quantile = 0.5,
#             method="constant",
#             interval.type = "quantile",
#             ties="discrete")
#
# test_that("mediana agregada sin subpop", {
#   expect_equal(real[[1]], test[[1]])
# })
#
#
# #########################################
# # MEDIANA CON DESAGREGACIÓN Y SIN SUBPOP#
# ########################################
# test <-  create_median(gastot_hd, dominios = sexo+zona,  replicas = 10, disenio = dc)
#
#
# real <-  survey::svyby(~gastot_hd,
#                    ~sexo+zona,
#                    FUN =  survey::svyquantile,
#                    design = dc_rep,
#                    quantile = 0.5,
#                    method="constant",
#                    interval.type = "quantile",
#                    ties="discrete",
#                    ci = T)
#
# test_that("mediana desagregada sin subpop", {
#   expect_equal(real$V1[1], test$median[1])
# })
#
#
# ###################################
# # MEDIANA CON DESAGREGACIÓN FACTOR#
# ###################################
#
#
# #expect_message(create_median(sexo, dominios = region_fact, replicas = 10, disenio = dc_ene), "the labels were removed")
#
#
# ##########################################
# # MEDIANA SIN DESAGREGACIÓN Y CON SUBPOP#
# ##########################################
#
# test <-  create_median(gastot_hd, subpop = ocupado,  replicas = 10, disenio = dc)
#
# real <- survey::svyquantile(~gastot_hd,
#                             design = subset(dc_rep, ocupado == 1),
#                             quantile = 0.5,
#                             method="constant",
#                             interval.type = "quantile",
#                             ties="discrete")
#
#
# test_that("mediana agregada con subpop", {
#   expect_equal(real[1], test$median[1])
# })
#
#
# ##########################################
# # MEDIANA CON DESAGREGACIÓN Y CON SUBPOP#
# ##########################################
#
# test <-  create_median(gastot_hd, dominios = sexo+zona, subpop = ocupado,  replicas = 10, disenio = dc)
#
# real <-  survey::svyby(~gastot_hd,
#                        ~sexo+zona,
#                        FUN =  survey::svyquantile,
#                        design = subset(dc_rep, ocupado == 1),
#                        quantile = 0.5,
#                        method="constant",
#                        interval.type = "quantile",
#                        ties="discrete",
#                        ci = T)
#
# test_that("mediana desagregada con subpop", {
#   expect_equal(real$se[1], test$se[1])
# })
#
# # Probar con otro método
# test <-  create_median(gastot_hd, dominios = sexo+zona, subpop = ocupado,  replicas = 10, disenio = dc, interval_type = "probability")
#
# real <-  survey::svyby(~gastot_hd,
#                        ~sexo+zona,
#                        FUN =  survey::svyquantile,
#                        design = subset(dc_rep, ocupado == 1),
#                        quantile = 0.5,
#                        method="constant",
#                        interval.type = "probability",
#                        ties="discrete",
#                        ci = T)
#
# test_that("mediana desagregada con subpop", {
#   expect_equal(real$se[1], test$se[1])
# })
#
#
#
#
# ################################################
# # ERROR ESTÁNDAR CON DESAGREGACIÓN Y CON SUBPOP#
# ################################################
#
# test <-  create_median(gastot_hd, dominios = sexo+zona, subpop = ocupado,  replicas = 10, disenio = dc, seed = 1234)
#
# real <-  survey::svyby(~gastot_hd,
#                        ~sexo+zona,
#                        FUN =  survey::svyquantile,
#                        design = subset(dc_rep, ocupado == 1),
#                        quantile = 0.5,
#                        method="constant",
#                        interval.type = "quantile",
#                        ties="discrete",
#                        ci = T)
#
# test_that("se desagregado con subpop", {
#   expect_equal(real$se[1], test$se[1])
# })
#
#
#
#
#
#
