
#### testing tabla_html_shiny con dos dominios y enfoque cepal

tabulado <- create_tabulado(calidad::epf_personas,
                v_interes = "gastot_hd",
                v_cruce = "zona+sexo",
                denominador = NULL,
                v_subpob = NULL,
                v_fexp1 = "fe",
                v_conglom = "varunit",
                v_estratos = "varstrat",
                tipoCALCULO = "Media",
                ajuste_ene = FALSE,
                ci = FALSE,
                scheme = "cepal")


tabla_html_shiny(tabulado,scheme = "cepal")


#### testing tabla_html_shiny con dos dominios y enfoque chile

tabulado <- create_tabulado(calidad::epf_personas,
                            v_interes = "gastot_hd",
                            v_cruce = "zona+sexo",
                            denominador = NULL,
                            v_subpob = NULL,
                            v_fexp1 = "fe",
                            v_conglom = "varunit",
                            v_estratos = "varstrat",
                            tipoCALCULO = "Media",
                            ajuste_ene = FALSE,
                            ci = FALSE,
                            scheme = "chile")


tabla_html_shiny(tabulado,scheme = "chile")


#### testing tabla_html_shiny con dos dominios y enfoque cepal y etiquetas

tabulado <- create_tabulado(shiny_calidad2::enusc_2020_etiq,
                            v_interes = "VP_DC",
                            v_cruce = "enc_region",
                            denominador = NULL,
                            v_subpob = NULL,
                            v_fexp1 = "Fact_Pers",
                            v_conglom = "Conglomerado",
                            v_estratos = "VarStrat",
                            tipoCALCULO = "Proporción",
                            ajuste_ene = FALSE,
                            ci = FALSE,
                            etiquetas = TRUE,
                            scheme = "cepal")


tabla_html_shiny(tabulado,scheme = "cepal")


#### testing tabla_html_shiny con dos dominios y enfoque chile

tabulado <- create_tabulado(shiny_calidad2::enusc_2020_etiq,
                            v_interes = "VP_DC",
                            v_cruce = "enc_region",
                            denominador = NULL,
                            v_subpob = NULL,
                            v_fexp1 = "Fact_Pers",
                            v_conglom = "Conglomerado",
                            v_estratos = "VarStrat",
                            tipoCALCULO = "Proporción",
                            ajuste_ene = FALSE,
                            ci = FALSE,
                            etiquetas = TRUE,
                            scheme = "chile")


tabla_html_shiny(tabulado,scheme = "chile")



create_tabulado(calidad::enusc,
                v_interes = "VP_DC",
                v_cruce = "enc_region",
                denominador = NULL,
                v_subpob = NULL,
                v_fexp1 = "Fact_Pers",
                v_conglom = "Conglomerado",
                v_estratos = "VarStrat",
                tipoCALCULO = "Conteo casos",
                ajuste_ene = FALSE,
                ci = FALSE,
                etiquetas = TRUE,
                scheme = "chile")











