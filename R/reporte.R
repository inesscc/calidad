

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
#' library(survey)
#' library(dplyr)
#'
#' hogar <- epf_personas %>%
#'   group_by(folio) %>%
#'   slice(1)
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = hogar, weights = ~fe)
#' tabla <- evaluate_prop(create_prop(ocupado, dominios = zona+sexo, disenio = dc))
#' tabla_html(tabla)
#' @export


tabla_html <- function(tabla) {

  tabla %>%
    dplyr::mutate_if(is.numeric, ~round(.x, 2)) %>%
    dplyr::mutate(
      calidad = kableExtra::cell_spec(.data$calidad, background  = dplyr::case_when(
        .data$calidad == "fiable" ~ "green",
        .data$calidad == "poco fiable" ~ "yellow",
        .data$calidad == "no fiable" ~ "red"
      ),
      color = "black")) %>%
   dplyr::mutate(
      n = kableExtra::cell_spec(.data$n, color= dplyr::case_when(
        .data$n < 60  ~ "red",
        .data$n >= 60 ~ "black"
      )),
      gl = kableExtra::cell_spec(.data$gl, color = dplyr::case_when(
        .data$gl < 9  ~ "red",
        .data$gl >= 9 ~ "black"
      ))) %>%
    kableExtra::kable(format.args = list(decimal.mark = ',', big.mark = "."),
                      format = "html",
                      escape = FALSE,
                      align = "c",
                      table.attr = "style = \"color: black;\"")  %>%
    kableExtra::kable_styling("striped",
                              full_width = FALSE,
                              html_font = "arial") %>%
    kableExtra::kable_paper("hover") %>%
    kableExtra::row_spec(0, bold = TRUE, color = "black")
}



