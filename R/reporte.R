

#' Genera tabla en html con los resultados de la evaluación
#'
#' La función recibe como input la evaluación de las estimaciones
#'
#' @param tabla \code{dataframe} generado por la funciones \code{evaluacion_calidad_prop} o \code{evaluacion_calidad}.
#'  Contiene el resultado de aplicar el protocolo de calidad.
#'
#' @return \code{html} con los resultados de la evaluación
#'
#' @import kableExtra
#' @examples
#' tabla_html(tabla)
#' @export

tabla_html <- function(tabla) {

  tabla %>%
    dplyr::mutate(coef_var = round(coef_var, 2)) %>%
    dplyr::mutate(
      calidad = cell_spec(calidad, background  = dplyr::case_when(
        calidad == "fiable" ~ "green",
        calidad == "poco fiable" ~ "yellow",
        calidad == "no fiable" ~ "red"
      ),
      color = "white")) %>%
    kable(format.args = list(decimal.mark = ',', big.mark = "."), format = "html", escape = F) %>%
    kable_styling("striped", full_width = F) %>%
    kable_paper("hover")
}






