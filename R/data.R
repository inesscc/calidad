#' VIII Encuesta de Presupuestos Familiares
#'
#' Versión reducida de la base de datos de la VIII EPF. Contiene
#' algunas variables sociodemográficas y la información necesaria
#' para trabajar con el diseño complejo.
#'
#' @docType data
#'
#' @format dataframe compuesto por 48.308 observaciones y 8 variables
#' \describe{
#'  \item{sexo}{sexo: 1 = hombre; 2 = mujer}
#'  \item{zona}{área de estimación: 1 = región metropolitana; 2 = resto de capitales regionales}
#'  \item{ecivil}{estado civil}
#'  \item{fe}{factor de expansión}
#'  \item{varunit}{unidad de primera etapa}
#'  \item{varstrat}{estrato}
#'  \item{gastot_hd}{gasto promedio por hogar}
#'  \item{ocupado}{indica de la persona está ocupada o no: 1 = ocupado; 0 = no ocupado}
#' }
#' @source \url{https://www.ine.cl/estadisticas/sociales/ingresos-y-gastos/encuesta-de-presupuestos-familiares}
#'
#' @examples
#' data(epf_personas)
#'
"epf_personas"



#' Encuesta Nacional de Empleo - ENE. 2020-efm
#'
#' Versión reducida de la base de datos de la ENE. Contiene
#' algunas variables sociodemográficas y la información necesaria
#' para trabajar con el diseño complejo.
#'
#' @docType data
#'
#' @format dataframe compuesto por 87.842 observaciones y 7 variables
#' \describe{
#'  \item{sexo}{sexo: 1 = hombre; 2 = mujer }
#'  \item{region}
#'  \item{cae_especifico}{condición de actividad económica}
#'  \item{fe}{factor de expansión}
#'  \item{varunit}{unidad de primera etapa}
#'  \item{varstrat}{estrato}
#'  \item{fdt}{indica si la persona pertenece a la fuerza de trabajo: 1 = pertenece; 0 = no pertenece}
#'  \item{ocupado}{indica de la persona está ocupada o no: 1 = ocupado; 0 = no ocupado}
#'  \item{desocupado}{indica de la persona está desocupada o no: 1 = desocupada; 0 = ocupada}
#' }
#' @source \url{https://www.ine.cl/estadisticas/sociales/mercado-laboral/ocupacion-y-desocupacion}
#'
#' @examples
#' data(ene)
#'
"ene"


#' Encuesta Nacional Urbana de Seguridad ciudadana 2019 - ENUSC 2019
#'
#' Datos de la ENUSC para el año 2019. Contiene solo algunas variables.
#'
#' @docType data
#'
#' @format dataframe compuesto por 24.465 observaciones y 22 variables
#' \describe{
#'  \item{rph_sexo}{sexo: 1 = hombre; 2 = mujer}
#'  \item{region}{16 regiones del país}
#'  \item{Fact_Pers}{factor de expansión para personas}
#'  \item{Fact_Hog}{factor de expansión para hogares}
#'  \item{Conglomerado}{unidad de primera etapa}
#'  \item{VarStrat}{estrato}
#'  \item{VP_DC}{victimización Personas, utiliza Fact_Pers}
#'  \item{VA_DC}{victimización Agregada Hogares, utiliza Fact_Hog}
#'  \item{rph_edad}{edad del entrevistado}
#'  \item{P3_1_1}{Percepción aumento de la delincuencia en el país, utiliza Fact_Pers}
#'  \item{P8_1_1}{Causa aumento delincuencia en el barrio, utiliza Fact_Pers}
#'  \item{muj_insg_taxi}{Percepción inseguridad en taxis de mujeres, variable elaborada con varaibles P9 y sexo, utiliza Fact_Pers}
#'  \item{hom_insg_taxi}{Percepción inseguridad en taxis de hombres, variable elaborada con varaibles P9 y sexo, utiliza Fact_Pers}
#'  \item{muj_insg_micro}{Percepción inseguridad en microbuses de mujeres, variable elaborada con varaibles P9 y sexo, utiliza Fact_Pers}
#'  \item{hom_insg_micro}{Percepción inseguridad en microbuses de hombres, variable elaborada con varaibles P9 y sexo, utiliza Fact_Pers}
#'  \item{muj_insg_centr.com}{Percepción inseguridad en centros comerciales de mujeres, variable elaborada con varaibles P9 y sexo, utiliza Fact_Pers}
#'  \item{hom_insg_centr.com}{Percepción inseguridad en centros comerciales de hombres, variable elaborada con varaibles P9 y sexo, utiliza Fact_Pers}
#'  \item{muj_insg_loc.col}{Percepción inseguridad en locomoción colectiva de mujeres, variable elaborada con varaibles P9 y sexo, utiliza Fact_Pers}
#'  \item{hom_insg_loc.col}{Percepción inseguridad en locomoción colectiva de hombres, variable elaborada con varaibles P9 y sexo, utiliza Fact_Pers}
#'  \item{muj_insg_barrio}{Percepción inseguridad en el barrio de mujeres, variable elaborada con varaibles P9 y sexo, utiliza Fact_Pers}
#'  \item{hom_insg_barrio}{Percepción inseguridad en el barrio de hombres, variable elaborada con varaibles P9 y sexo, utiliza Fact_Pers}
#'  }
#' @source \url{https://www.ine.cl/docs/default-source/seguridad-ciudadana/bbdd/2019/base-de-datos---xvi-enusc-2019-(csv).csv?sfvrsn=d3465758_2&download=true}
#'
#' @examples
#' data(enusc)
#'
"enusc"

