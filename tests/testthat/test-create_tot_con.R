
context("test-create_tot_con")



options(survey.lonely.psu = "certainty")

# Hacer cambios en la ene para hacer los tests

ene <- ene %>%
  dplyr::mutate(fdt = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 9, 1, 0),
                ocupado = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0),
                desocupado = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0),
                fdt_na = dplyr::if_else(dplyr::row_number() <= 10, NA_real_, fdt ),
                region_fact <- dplyr::case_when(
                  region == 1  ~   "Tarapacá",
                  region == 2  ~   "Antofagasta",
                  region == 3  ~   "Atacama",
                  region == 4  ~   "Coquimbo",
                  region == 5  ~   "Valparaíso",
                  region == 6  ~   "O'Higgins",
                  region == 7  ~   "Maule",
                  region == 8  ~   "Bíobío",
                  region == 9  ~   "Araucanía",
                  region == 10  ~   "Los Lagos",
                  region == 11  ~   "Aysén",
                  region == 12  ~   "Magallanes",
                  region == 13  ~   "Metropolitana",
                  region == 14  ~   "Los Ríos",
                  region == 15  ~   "Arica",
                  region == 16  ~   "Ñuble",
                ),
                region_fact = as.factor(region_fact)
                )


# Diseño de la ENE
dc_ene <- survey::svydesign(weights = ~fact_cal, ids = ~conglomerado, strata = ~estrato_unico, data = ene)




#####################################
# Testear variable factor en dominios
#####################################

# Probar una variable factor en el parámetro dominios
test <- create_tot_con(desocupado, dominios =  region_fact, disenio = dc_ene)

test_that("nombres tabla agregado", {
  expect_equal(round(test[1, 2]), 33900)
})


