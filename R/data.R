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


#' Encuesta Nacional Urbana de Seguridad ciudadana 2019 - ENUSC 2019
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




