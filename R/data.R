#' VIII Encuesta de Presupuestos Familiares
#'
#' Reduced version of the VIII EPF database. Contains
#' some sociodemographic variables and the necessary information
#' to work with complex design.
#'
#' @docType data
#'
#' @format dataframe compuesto por 48.308 observaciones y 8 variables
#' \describe{
#'  \item{sexo}{1 = male; 2 = female}
#'  \item{zona}{1 = metropolitan area; 2 = rest of the regional capitals}
#'  \item{ecivil}{marital status}
#'  \item{fe}{sample weights}
#'  \item{varunit}{PSU}
#'  \item{varstrat}{strata}
#'  \item{gastot_hd}{household expenditure}
#'  \item{ocupado}{1 = employed; 0 = non-employed}
#' }
#' @source \url{https://www.ine.cl/estadisticas/sociales/ingresos-y-gastos/encuesta-de-presupuestos-familiares}
#'
#' @examples
#' data(epf_personas)
#'
"epf_personas"



#' Encuesta Nacional de Empleo - ENE. 2020-efm
#'
#' Reduced version of the ENE database. Contains
#'some sociodemographic variables and the necessary information
#'to work with complex design
#'
#' @docType data
#'
#' @format dataframe with 87.842 rows y 7 columns
#' \describe{
#'  \item{sexo}{1 = man; 2 = woman }
#'  \item{region}{region}
#'  \item{cae_especifico}{Economic activity status}
#'  \item{fe}{sample weights}
#'  \item{varunit}{PSU}
#'  \item{varstrat}{strata}
#'  \item{fdt}{It shows if the person belongs to labour force: 1 = yes; 0 = no}
#'  \item{ocupado}{1 = employed; 0 = non-employed}
#'  \item{desocupado}{1 = non-employed; 0 = employed}
#' }
#' @source \url{https://www.ine.cl/estadisticas/sociales/mercado-laboral/ocupacion-y-desocupacion}
#'
#' @examples
#' data(ene)
#'
"ene"


#' Encuesta Nacional Urbana de Seguridad Ciudadana 2019 - ENUSC 2019
#'
#' ENUSC data for the year 2019. Contains only a few variables.
#'
#' @docType data
#'
#' @format dataframe with 24.465 rows y 22 columns
#' \describe{
#'  \item{rph_sexo}{1 = man; 2 = woman}
#'  \item{region}{16 regions}
#'  \item{Fact_Pers}{person sample weights}
#'  \item{Fact_Hog}{household sample weights}
#'  \item{Conglomerado}{PSU}
#'  \item{VarStrat}{strata}
#'  \item{VP_DC}{Individual victimization. It works combined with Fact_Pers}
#'  \item{VA_DC}{Household victimization. It works combined  with Fact_Hog}
#'  \item{rph_edad}{age}
#'  \item{P3_1_1}{ Perception of increased crime in the country. It works combined with Fact_Pers}
#'  \item{P8_1_1}{Cause of increased crime in the neighborhood. It works combined with Fact_Pers}
#'  \item{muj_insg_taxi}{Female perception of insecurity inside taxis. Variable elaborated with variables P9 and rph_sexo . It works combined with Fact_Pers}
#'  \item{hom_insg_taxi}{Male perception of insecurity inside taxis. Variable elaborated with variables P9 and rph_sexo. It works combined with Fact_Pers}
#'  \item{muj_insg_micro}{Female perception of insecurity inside buses. Variable elaborated with variables P9 and rph_sexo. It works combined with Fact_Pers}
#'  \item{hom_insg_micro}{Male perception of insecurity inside buses. Variable elaborated with variables P9 and rph_sexo. It works combined with Fact_Pers}
#'  \item{muj_insg_centr.com}{Female perception of insecurity inside malls. Variable elaborated with variables P9 and rph_sexo. It works combined with Fact_Pers}
#'  \item{hom_insg_centr.com}{Male perception of insecurity inside malls. Variable elaborated with variables P9 and rph_sexo. It works combined with Fact_Pers}
#'  \item{muj_insg_loc.col}{Female perception of insecurity public transport. Variable elaborated with variables P9 and rph_sexo. It works combined with Fact_Pers}
#'  \item{hom_insg_loc.col}{Male perception of insecurity public transport. Variable elaborated with variables P9 and rph_sexo. It works combined with Fact_Pers}
#'  \item{muj_insg_barrio}{Female perception of insecurity neighborhood. Variable elaborated with variables P9 and rph_sexo. It works combined with Fact_Pers}
#'  \item{hom_insg_barrio}{Male perception of insecurity neighborhood. Variable elaborated with variables P9 and rph_sexo. It works combined with Fact_Pers}
#'  }
#' @source \url{https://www.ine.cl/docs/default-source/seguridad-ciudadana/bbdd/2019/base-de-datos---xvi-enusc-2019-(csv).csv?sfvrsn=d3465758_2&download=true}
#'
#' @examples
#' data(enusc)
#'
"enusc"


#' Encuesta de Caracterización Socioeconómica Nacional 2020 - CASEN en Pandemia 2020
#'
#' CASEN data for the year 2020. Contains only a few variables.
#'
#' @docType data
#'
#' @format dataframe with 185.437 rows y 6 columns
#' \describe{
#'  \item{folio}{household id}
#'  \item{sexo}{1 = man; 2 = woman}
#'  \item{edad}{age}
#'  \item{activ}{Economic activity status}
#'  \item{ing_aut_hog}{Household Income}
#'  \item{pobreza}{poverty status: 1 = extreme poverty, 2 = non-extreme poverty, 3 = non-poverty}
#'  \item{expr}{regional sample weights}
#'  \item{estrato}{strata}
#'  \item{cod_upm}{PSU}
#'  }
#' @source \url{http://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-en-pandemia-2020}
#'
#' @examples
#' data(casen)
#'
"casen"



#' Encuesta Nacional Urbana de Seguridad Ciudadana 2023 - ENUSC 2023
#'
#' ENUSC data for the year 2023. Contains only a few variables.
#'
#' @docType data
#'
#' @format dataframe with 49.813 rows y 15 columns
#' \describe{
#'  \item{enc_region }{16 regions}
#'  \item{enc_rpc}{Code of region, province and commune}
#'  \item{Fact_Pers_Reg}{Person sample weights at region level}
#'  \item{Fact_Pers_Com}{Person sample weights at commune level}
#'  \item{Fact_Hog_Reg}{Household sample weights at region level}
#'  \item{Fact_Hog_Com}{Household sample weights at commune level}
#'  \item{VarStrat}{Strata}
#'  \item{Conglomerado}{PSU}
#'  \item{VH_DV}{Households victimized by violent crimes. It works combined with Fact_Hog_*}
#'  \item{VH_DC}{Household victimization. It works combined  with Fact_Hog_*}
#'  \item{VP_DV}{People victimized by violent crimes. It works combined with Fact_Pers_*}
#'  \item{VP_DC}{Individual victimization. It works combined  with Fact_Pers_*}
#'  \item{PAD}{Perception of increased crime in the country. It works combined  with Fact_Pers_*}
#'  \item{rph_sexo}{1 = man; 2 = woman}
#'  \item{rph_edad}{Age}
#'
#'  }
#' @source \url{https://www.ine.gob.cl/docs/default-source/seguridad-ciudadana/bbdd/2023/base-usuario-20-enusc-2023.csv?sfvrsn=34653b72_2&download=true}
#'
#' @examples
#' data(enusc_2023)
#'
"enusc_2023"




#' Encuesta Longitudinal de Empresas
#'
#' ELE data for the year 2022. Contains only a few variables.
#'
#' @docType data
#'
#' @format dataframe with 6.592 rows y 13 columns
#' \describe{
#'  \item{rol_ficticio}{Company ID}
#'  \item{cod_actividad}{Economic activity}
#'  \item{cod_tamano}{Company size by sales}
#'  \item{tramo}{Inclusion range}
#'  \item{fe_transversal}{Cross-sectional weights}
#'  \item{fe_longitudinal}{Longitudinal weights}
#'  \item{panel}{Panel sample}
#'  \item{estrato}{Strata}
#'  \item{pob}{Finite population correction}
#'  \item{VA_2022}{Value added 2022, difference between production value and intermediate consumption}
#'  \item{VA_2022f}{VA_2022f is an adjusted version of VA_2022, where negative values are replaced with 0, while non-negative values remain unchanged.}
#'  \item{EMP}{Total personnel employed and hired by the company on a monthly basis}
#'  \item{REMP_TOTAL}{Total gross remuneration of personnel hired by the company}

#'
#'  }
#' @source \url{https://www.ine.gob.cl/docs/default-source/encuesta-longitudinal-de-empresas/bbdd/ele-2022/base-de-datos-ele7.csv?sfvrsn=1504c58d_4&download=true}
#'
#' @examples
#' data(ELE7)
#'
"ELE7"



#' Tamano muestra objetivo Encuesta Longitudinal de Empresas
#'
#' Target cross-sectional sample size ELE data for the year 2022.
#'
#' @docType data
#'
#' @format dataframe with 59 rows y 4 columns
#' \describe{
#'  \item{cod_tamano}{Company size by sales}
#'  \item{cod_actividad_letra}{Economic activity}
#'  \item{cod_actividad}{Economic activity ID}
#'  \item{n_obj}{Target sample size}

#'
#'  }
#' @source \url{https://www.ine.gob.cl/docs/default-source/encuesta-longitudinal-de-empresas/metodologias/ele-2022/informe-de-calidad-ele7.pdf?sfvrsn=6ca73eb5_4}
#'
#' @examples
#' data(ELE7_n_obj)
#'
"ELE7_n_obj"



