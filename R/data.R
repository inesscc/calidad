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
#'   \item{sexo}{sexo: 1 = hombre; 2 = mujer }
#'   \item{zona}{área de estimación: 1 = región metropolitana; 2 = resto de capitales regionales}
#'   \item{ecivil}{estado civil}
#'   \item{fe}{factor de expansión}
#'   \item{varunit}{unidad de primera etapa}
#'   \item{varstrat}{estrato}
#'   \item{gastot_hd}{gasto promedio por hogar}
#'   \item{ocupado}{indica de la persona está ocupada o no: 1 = ocupado; 0 = no ocupado}
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
#'   \item{sexo}{sexo: 1 = hombre; 2 = mujer }
#'   \item{region}
#'   \item{cae_especifico}{condición de actividad económica}
#'   \item{fe}{factor de expansión}
#'   \item{varunit}{unidad de primera etapa}
#'   \item{varstrat}{estrato}
#'   \item{fdt}{indica si la persona pertenece a la fuerza de trabajo: 1 = pertenece; 0 = no pertenece}
#'   \item{ocupado}{indica de la persona está ocupada o no: 1 = ocupado; 0 = no ocupado}
#'   \item{desocupado}{indica de la persona está desocupada o no: 1 = desocupada; 0 = ocupada}
#' }
#' @source \url{https://www.ine.cl/estadisticas/sociales/mercado-laboral/ocupacion-y-desocupacion}
#'
#' @examples
#' data(ene)
#'
"ene"
