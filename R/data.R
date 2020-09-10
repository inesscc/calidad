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
#'   \item{ocupado}{indica de la persona está ocupada o no: 1 = ocupado; 2 = no ocupado}
#' }
#' @source \url{https://www.ine.cl/estadisticas/sociales/ingresos-y-gastos/encuesta-de-presupuestos-familiares}
#'
#' @examples
#' data(epf_personas)
#'
"epf_personas"
