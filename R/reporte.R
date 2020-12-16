

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
#' tabla <- evaluacion_calidad_prop(crear_insumos_prop(ocupado, zona+sexo, dc))
#' tabla_html(tabla)
#' @export

tabla_html <- function(tabla) {

  tabla %>%
    dplyr::mutate_if(is.numeric, ~round(.x, 2)) %>%
    dplyr::mutate(
      calidad = kableExtra::cell_spec(calidad, background  = dplyr::case_when(
        calidad == "fiable" ~ "green",
        calidad == "poco fiable" ~ "yellow",
        calidad == "no fiable" ~ "red"
      ),
      color = "black")) %>%
   dplyr::mutate(
      n = kableExtra::cell_spec(n, color= dplyr::case_when(
        n < 60  ~ "red",
        n >= 60 ~ "black"
      )),
      gl = kableExtra::cell_spec(gl, color = dplyr::case_when(
        gl < 9  ~ "red",
        gl >= 9 ~ "black"
      ))) %>%
    kableExtra::kable(format.args = list(decimal.mark = ',', big.mark = "."), format = "html", escape = F) %>%
    kableExtra::kable_styling("striped", full_width = F) %>%
    kableExtra::kable_paper("hover")
}






