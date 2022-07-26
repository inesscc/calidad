

#' Genera table en html con los resultados de la evaluación
#'
#' La función recibe como input la evaluación de las estimaciones
#'
#' @param table \code{dataframe} generado por la funciones \code{evaluacion_label_prop} o \code{evaluacion_label}.
#'  Contiene el resultado de aplicar el protocolo de label.
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
#' table <- evaluate(create_prop("ocupado", domains = "zona+sexo", design = dc))
#' @export


create_html <- function(table) {


  # This is the INE case
  if ( sum(class(table) %in% "cepal.eval")  == 0) {
    table %>%
      dplyr::mutate_if(is.numeric, ~round(.x, 2)) %>%
      dplyr::mutate(
        label = kableExtra::cell_spec(.data$label, background  = dplyr::case_when(
          .data$label == "fiable" ~ "green",
          .data$label == "poco fiable" ~ "yellow",
          .data$label == "no fiable" ~ "red"
        ),
        color = "black")) %>%
      dplyr::mutate(
        n = kableExtra::cell_spec(.data$n, color= dplyr::case_when(
          .data$n < 60  ~ "red",
          .data$n >= 60 ~ "black"
        )),
        df = kableExtra::cell_spec(.data$df, color = dplyr::case_when(
          .data$df < 9  ~ "red",
          .data$df >= 9 ~ "black"
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

  } else {
    table %>%
      dplyr::mutate_if(is.numeric, ~round(.x, 2)) %>%
      dplyr::mutate(
        label = kableExtra::cell_spec(.data$label, background  = dplyr::case_when(
          .data$label == "publish" ~ "green",
          .data$label == "review" ~ "yellow",
          .data$label == "supress" ~ "red"
        ),
        color = "black")) %>%
      dplyr::mutate(
        n = kableExtra::cell_spec(.data$n, color= dplyr::case_when(
          .data$n < 60  ~ "red",
          .data$n >= 60 ~ "black"
        )),
        df = kableExtra::cell_spec(.data$df, color = dplyr::case_when(
          .data$df < 9  ~ "red",
          .data$df >= 9 ~ "black"
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

}



