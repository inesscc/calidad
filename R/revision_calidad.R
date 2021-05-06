


#---------------------------------------------------------------------
#'
#' Calcula el valor de una función cuadrática
#'
#' \code{quadratic} returns the output of a particular function created by INE Chile, which
#' is evaluated at the value of the estimated proportion from a sample. If the output of the
#' function is  higher than the standard error, it is interpreted as a signal that the
#' estimation is not reliable.
#'
#'
#' @param p numeric vector with the values of the estimations for proportions
#' @return  numeric vector
#'
quadratic <- function(p) {
  purrr::map_dbl(p, function(x) {
  if (x <= 0.5) {
    (x**(2/3))/9
  } else {
    ((1 - x)**(2/3))/9
  }
  })
}



#---------------------------------------------------------------------
#' Evaluate the quality of mean estimations
#'
#' \code{evaluar_calidad_media} evauates the quality of mean estimation using the
#' methodology created by INE Chile, which considers sample size, degrees of freedom and
#' coeficient of variation.
#'
#' @param tabulado \code{dataframe} created by \code{crear_insumos_media}
#' @param publicar \code{boolean} indicating if the evaluation of the complete table
#' must be added. If it is TRUE, the function adds a new column to the \code{dataframe}
#' @param condicion \code{character} with the complete condition to filter the \code{dataframe}
#' @return \code{dataframe} with all the columns included in the input table, plus a new column
#'containing a label indicating the evaluation of each estimation: reliable, bit reliable or unreliable
#'
#' @examples
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' evaluate_mean(create_mean(gastot_hd, dominios = zona+sexo, disenio = dc))
#' @export


evaluate_mean <- function(tabulado, condicion = NULL, publicar = FALSE) {

  #Aplicar la condición requerida por el usuario
  if (!is.null(condicion) ) {
    tabulado <- tabulado %>%
      dplyr::filter(!!rlang::parse_expr(condicion))
  }
  # Chequear si existen valores NA en los insumos. Si hay NAs, se manda un warning al usuario
  suma_na <- tabulado %>%
    dplyr::mutate(contiene_na = dplyr::if_else(is.na(.data$n) | is.na(.data$gl) | is.na(.data$coef_var), 1, 0)) %>%
    dplyr::summarise(suma = sum(.data$contiene_na)) %>%
    dplyr::pull(.data$suma)

  # mandar un warning cuando se han exlcuido filas
  if (suma_na > 0) {
    warning(paste0("Se han excluido ", suma_na, " filas con NA en n, gl o cv"))
  }


  evaluacion <- tabulado %>%
    dplyr::filter(!is.na(.data$n) & !is.na(.data$gl) & !is.na(.data$coef_var)) %>%
    dplyr::mutate(eval_n = dplyr::if_else(.data$n >= 60, "n suficiente", "n insuficiente"),
           eval_gl = dplyr::if_else(gl >= 9, "gl suficiente", "gl insuficiente"),
           eval_cv = dplyr::case_when(
             .data$coef_var <= 15                  ~ "cv <= 15",
             .data$coef_var > 15 & .data$coef_var <= 30  ~ "cv entre 15 y 30",
             .data$coef_var > 30                   ~ "cv > 30"
           ),
           calidad = dplyr::case_when(
             eval_n == "n insuficiente" | eval_gl == "gl insuficiente" | eval_cv == "cv > 30"      ~ "no fiable",
             eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == "cv <= 15"         ~ "fiable",
             eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == "cv entre 15 y 30" ~ "poco fiable"
           )
    )

  # Criterio general para la publicación del tabulado
  if (publicar == TRUE) {
    evaluacion <- evaluacion %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(.data$n) & !is.na(.data$gl) & !is.na(.data$coef_var)) %>%
      dplyr::mutate(pasa = sum(dplyr::if_else(.data$calidad == "fiable", 1, 0)) / nrow(.) * 100,
                    pasa = round(.data$.data$pasa, 2),
                    publicacion = dplyr::if_else(.data$.data$pasa >= 50, "publicar tabulado", "no publicar tabulado"),
                    aprueba = paste0(.data$.data$pasa, "% de estimaciones fiables")) %>%
      dplyr::select(-.data$.data$pasa)


  }


  return(evaluacion)
}


#---------------------------------------------------------------------


#' Evaluate the quality of total estimations
#'
#' \code{evaluate_tot} evaluates the quality of total estimations using the
#' methodology created by INE Chile, which considers sample size, degrees of freedom and
#' the coefficient of variation.
#'
#' @param tabulado \code{dataframe} created by \code{crear_insumos_tot}
#' @param publicar \code{boolean} indicating if the evaluation of the complete table
#' must be added to the output. If it is TRUE, the function adds a new column to the \code{dataframe}
#' @param condicion \code{character} with the complete condition to filter the \code{dataframe}
#' @return \code{dataframe} with all the columns included in the input table, plus a new column
#'containing a label indicating the evaluation of each estimation: reliable, bit reliable or unreliable
#'
#' @examples
#' library(survey)
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' evaluate_tot(create_tot(ocupado, dominios = zona+sexo, disenio = dc))
#' @export


evaluate_tot <- function(tabulado, condicion = NULL, publicar = FALSE) {

  #Aplicar la condición requerida por el usuario
  if (!is.null(condicion) ) {
    tabulado <- tabulado %>%
      dplyr::filter(!!rlang::parse_expr(condicion))
  }

  # Chequear si existen valores NA en los insumos. Si hay NAs, se manda un warning al usuario
  suma_na <- tabulado %>%
    dplyr::mutate(contiene_na = dplyr::if_else(is.na(.data$n) | is.na(.data$gl) | is.na(.data$coef_var), 1, 0)) %>%
    dplyr::summarise(suma = sum(.data$contiene_na)) %>%
    dplyr::pull(.data$suma)

  # mandar un warning cuando se han exlcuido filas
  if (suma_na > 0) {
    warning(paste0("Se han excluido ", suma_na, " filas con NA en n, gl o cv"))
  }


  evaluacion <- tabulado %>%
    dplyr::mutate(eval_n = dplyr::if_else(.data$n >= 60, "n suficiente", "n insuficiente"),
                  eval_gl = dplyr::if_else(.data$gl >= 9, "gl suficiente", "gl insuficiente"),
                  eval_cv = dplyr::case_when(
                    .data$coef_var <= 15                  ~ "cv <= 15",
                    .data$coef_var > 15 & .data$coef_var <= 30  ~ "cv entre 15 y 30",
                    .data$coef_var > 30                   ~ "cv > 30"
                  ),
                  calidad = dplyr::case_when(
                    eval_n == "n insuficiente" | eval_gl == "gl insuficiente" | eval_cv == "cv > 30"      ~ "no fiable",
                    eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == "cv <= 15"         ~ "fiable",
                    eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == "cv entre 15 y 30" ~ "poco fiable"
                  )
    )

  # Criterio general para la publicación del tabulado
  if (publicar == TRUE) {
    evaluacion <- evaluacion %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(.data$n) & !is.na(.data$gl) & !is.na(.data$coef_var)) %>%
      dplyr::mutate(pasa = sum(dplyr::if_else(.data$calidad == "fiable", 1, 0)) / nrow(.) * 100,
                    pasa = round(.data$pasa, 2),
                    publicacion = dplyr::if_else(.data$pasa >= 50, "publicar tabulado", "no publicar tabulado"),
                    aprueba = paste0(.data$pasa, "% de estimaciones fiables")) %>%
      dplyr::select(-.data$pasa)
  }



  return(evaluacion)
}



#---------------------------------------------------------------------
#' Evaluate the quality of sum estimations for continuous variables
#'
#'
#' \code{evaluate_tot_con} evaluates the quality of total estimations for continuous variables
#' using the methodology created by INE Chile, which considers sample size, degrees of freedom and
#' the coefficient of variation.
#'
#' @param tabulado \code{dataframe} created by \code{crear_insumos_tot_con}
#' @param publicar \code{boolean} indicating if the evaluation of the complete table
#' must be added to the output. If it is TRUE, the function adds a new column to the \code{dataframe}
#' @param condicion \code{character} with the complete condition to filter the \code{dataframe}
#' @return \code{dataframe} with all the columns included in the input table, plus a new column
#'containing a label indicating the evaluation of each estimation: reliable, bit reliable or unreliable
#'
#' @examples
#'
#'
#' library(dplyr)
#' hogar <- epf_personas %>%
#'   group_by(folio) %>%
#'   slice(1)
#'
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = hogar, weights = ~fe)
#' evaluate_tot_con(create_tot_con(gastot_hd, dominios =  zona+sexo, disenio = dc))
#'
#'
#' @export


evaluate_tot_con <- function(tabulado, condicion = NULL, publicar = FALSE) {

  #Aplicar la condición requerida por el usuario
  if (!is.null(condicion) ) {
    tabulado <- tabulado %>%
      dplyr::filter(!!rlang::parse_expr(condicion))
  }

  # Chequear si existen valores NA en los insumos. Si hay NAs, se manda un warning al usuario
  suma_na <- tabulado %>%
    dplyr::mutate(contiene_na = dplyr::if_else(is.na(.data$n) | is.na(.data$gl) | is.na(.data$coef_var), 1, 0)) %>%
    dplyr::summarise(suma = sum(.data$contiene_na)) %>%
    dplyr::pull(.data$suma)

  # mandar un warning cuando se han exlcuido filas
  if (suma_na > 0) {
    warning(paste0("Se han excluido ", suma_na, " filas con NA en n, gl o cv"))
  }


  evaluacion <- tabulado %>%
    dplyr::mutate(eval_n = dplyr::if_else(.data$n >= 60, "n suficiente", "n insuficiente"),
                  eval_gl = dplyr::if_else(.data$gl >= 9, "gl suficiente", "gl insuficiente"),
                  eval_cv = dplyr::case_when(
                    .data$coef_var <= 15                  ~ "cv <= 15",
                    .data$coef_var > 15 & .data$coef_var <= 30  ~ "cv entre 15 y 30",
                    .data$coef_var > 30                   ~ "cv > 30"
                  ),
                  calidad = dplyr::case_when(
                    eval_n == "n insuficiente" | eval_gl == "gl insuficiente" | eval_cv == "cv > 30"      ~ "no fiable",
                    eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == "cv <= 15"         ~ "fiable",
                    eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == "cv entre 15 y 30" ~ "poco fiable"
                  )
    )

  # Criterio general para la publicación del tabulado
  if (publicar == TRUE) {
    evaluacion <- evaluacion %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(.data$n) & !is.na(.data$gl) & !is.na(.data$coef_var)) %>%
      dplyr::mutate(pasa = sum(dplyr::if_else(.data$calidad == "fiable", 1, 0)) / nrow(.) * 100,
                    pasa = round(.data$pasa, 2),
                    publicacion = dplyr::if_else(.data$pasa >= 50, "publicar tabulado", "no publicar tabulado"),
                    aprueba = paste0(.data$pasa, "% de estimaciones fiables")) %>%
      dplyr::select(-.data$pasa)



  }



  return(evaluacion)
}


#---------------------------------------------------------------------



#' Evaluate the quality of median estimations
#'
#'
#' \code{evaluate_median} evaluates the quality median estimations
#' using the methodology created by INE Chile, which considers sample size, degrees of freedom and
#' the coefficient of variation.
#'
#' @param tabulado \code{dataframe} created by \code{create_median}
#' @param publicar \code{boolean} indicating if the evaluation of the complete table
#' must be added to the output. If it is TRUE, the function adds a new column to the \code{dataframe}
#' @param condicion \code{character} with the complete condition to filter the \code{dataframe}
#' @return \code{dataframe} with all the columns included in the input table, plus a new column
#'containing a label indicating the evaluation of each estimation: reliable, bit reliable or unreliable
#'
#' @examples
#' library(survey)
#' library(dplyr)
#'
#' hogar <- epf_personas %>%
#'   group_by(folio) %>%
#'   dplyr::slice(1)
#'
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = hogar, weights = ~fe)
#' evaluate_median(create_median(gastot_hd, dominios = zona+sexo, disenio = dc))
#' @export

evaluate_median <- function(tabulado, condicion = NULL, publicar = FALSE) {

  #Aplicar la condición requerida por el usuario
  if (!is.null(condicion) ) {
    tabulado <- tabulado %>%
      dplyr::filter(!!rlang::parse_expr(condicion))
  }
  # Chequear si existen valores NA en los insumos. Si hay NAs, se manda un warning al usuario
  suma_na <- tabulado %>%
    dplyr::mutate(contiene_na = dplyr::if_else(is.na(.data$n) | is.na(.data$gl) | is.na(.data$coef_var), 1, 0)) %>%
    dplyr::summarise(suma = sum(.data$contiene_na)) %>%
    dplyr::pull(.data$suma)

  # mandar un warning cuando se han exlcuido filas
  if (suma_na > 0) {
    warning(paste0("Se han excluido ", suma_na, " filas con NA en n, gl o cv"))
  }


  evaluacion <- tabulado %>%
    dplyr::filter(!is.na(.data$n) & !is.na(.data$gl) & !is.na(.data$coef_var)) %>%
    dplyr::mutate(eval_n = dplyr::if_else(.data$n >= 60, "n suficiente", "n insuficiente"),
                  eval_gl = dplyr::if_else(.data$gl >= 9, "gl suficiente", "gl insuficiente"),
                  eval_cv = dplyr::case_when(
                    .data$coef_var <= 15                  ~ "cv <= 15",
                    .data$coef_var > 15 & .data$coef_var <= 30  ~ "cv entre 15 y 30",
                    .data$coef_var > 30                   ~ "cv > 30"
                  ),
                  calidad = dplyr::case_when(
                    eval_n == "n insuficiente" | eval_gl == "gl insuficiente" | eval_cv == "cv > 30"      ~ "no fiable",
                    eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == "cv <= 15"         ~ "fiable",
                    eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == "cv entre 15 y 30" ~ "poco fiable"
                  )
    )

  # Criterio general para la publicación del tabulado
  if (publicar == TRUE) {
    evaluacion <- evaluacion %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(.data$n) & !is.na(.data$gl) & !is.na(.data$coef_var)) %>%
      dplyr::mutate(pasa = sum(dplyr::if_else(.data$calidad == "fiable", 1, 0)) / nrow(.) * 100,
                    pasa = round(.data$pasa, 2),
                    publicacion = dplyr::if_else(.data$pasa >= 50, "publicar tabulado", "no publicar tabulado"),
                    aprueba = paste0(.data$pasa, "% de estimaciones fiables")) %>%
      dplyr::select(-.data$pasa)


  }


  return(evaluacion)
}

#---------------------------------------------------------------------


#' Evaluate the quality of ratio estimations
#'
#'
#' \code{evaluate_prop} evaluates the quality ratio estimations
#' using the methodology created by INE Chile, which considers sample size, degrees of freedom, standard error and
#' the coefficient of variation.
#'
#' It is evaluated according to the standard error or the coefficient of variation, based on the value of the estimation that its
#' receives as inputs. For estimations < 1 evaluates by standar error & for estimations > 1 evaluates by coefficient of variation.
#' Evaluates each estimation per row independently
#'
#' @param tabulado \code{dataframe} created by \code{create_ratio}
#' @param publicar \code{boolean} indicating if the evaluation of the complete table
#' must be added to the output. If it is TRUE, the function adds a new column to the \code{dataframe}
#' @param condicion \code{character} with the complete condition to filter the \code{dataframe}
#' @return \code{dataframe} with all the columns included in the input table, plus a new column
#'containing a label indicating the evaluation of each estimation: reliable, bit reliable or unreliable
#'
#' @examples
#' library(survey)
#' library(dplyr)
#' epf <-   mutate(epf_personas, gasto_zona1 = dplyr::if_else(zona == 1, gastot_hd, 0)) %>%
#' group_by(folio) %>%
#' slice(1)
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf, weights = ~fe)
#' evaluate_prop(create_prop(var = gasto_zona1, denominador = gastot_hd,
#'               dominios = zona+sexo, disenio = dc))
#' @export

evaluate_prop <- function(tabulado, condicion = NULL, publicar = FALSE) {

  #Aplicar la condición requerida por el usuario
  if (!is.null(condicion) ) {
    tabulado <- tabulado %>%
      dplyr::filter(!!rlang::parse_expr(condicion))
  }

  # Chequear si existen valores NA en los insumos. Si hay NAs, se manda un warning al usuario
  suma_na <- tabulado %>%
    dplyr::mutate(contiene_na = dplyr::if_else(is.na(.data$n) | is.na(.data$gl) | is.na(.data$se), 1, 0)) %>%
    dplyr::summarise(suma = sum(.data$contiene_na)) %>%
    dplyr::pull(.data$suma)

  # mandar un warning cuando se han exlcuido filas
  if (suma_na > 0) {
    warning(paste0("Se han excluido ", suma_na, " filas con NA en n, gl o se"))
  }


  evaluacion <- tabulado %>%
    dplyr::mutate(eval_n = dplyr::if_else(.data$n >= 60, "n suficiente", "n insuficiente"),
                  eval_gl = dplyr::if_else(.data$gl >= 9, "gl suficiente", "gl insuficiente"),
                  prop_est = dplyr::case_when(.data$objetivo <= 0.5 ~ "<= a 0.5",
                                              .data$objetivo < 1 & .data$objetivo > 0.5 ~ "> a 0.5",
                                              .data$objetivo > 1 ~ ">= a 1"),
                  tipo_eval = dplyr::if_else(.data$objetivo < 1, "Eval SE", "Eval CV"),
                  cuadratica = dplyr::if_else(.data$objetivo < 1, quadratic(.data$objetivo), NA_real_),
                  eval_se = dplyr::if_else(.data$objetivo < 1,
                                           dplyr::if_else(.data$se <= .data$cuadratica,
                                                          "SE adecuado", "SE alto"), NA_character_),
                  eval_cv = dplyr::if_else(.data$objetivo < 1, NA_character_,
                                           dplyr::case_when(cv <= 15            ~ "cv <= 15",
                                                            cv > 15 & cv <= 30  ~ "cv entre 15 y 30",
                                                            cv > 30             ~ "cv > 30"
                  )),
                  calidad = dplyr::case_when(
                    objetivo <1 & eval_n == "n insuficiente" | eval_gl == "gl insuficiente"                                                  ~ "no fiable",
                    objetivo <1 & eval_n == "n suficiente" & eval_gl == "gl suficiente" & prop_est == "<= a 0.5" & eval_se == "SE adecuado"  ~ "fiable",
                    objetivo <1 & eval_n == "n suficiente" & eval_gl == "gl suficiente" & prop_est == "<= a 0.5" & eval_se == "SE alto"      ~ "poco fiable",
                    objetivo <1 & eval_n == "n suficiente" & eval_gl == "gl suficiente" & prop_est == "> a 0.5" & eval_se == "SE adecuado"   ~ "fiable",
                    objetivo <1 & eval_n == "n suficiente" & eval_gl == "gl suficiente" & prop_est == "> a 0.5" & eval_se == "SE alto"       ~ "poco fiable",
                    objetivo >= 1 & eval_n == "n insuficiente" | eval_gl == "gl insuficiente" | eval_cv == "cv > 30"      ~ "no fiable",
                    objetivo >= 1 & eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == "cv <= 15"         ~ "fiable",
                    objetivo >= 1 & eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == "cv entre 15 y 30" ~ "poco fiable"))


  # Criterio general para la publicación del tabulado
  if (publicar == TRUE) {
    evaluacion <- evaluacion %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(.data$n) & !is.na(.data$gl) & !is.na(.data$se)) %>%
      dplyr::mutate(pasa = sum(dplyr::if_else(.data$calidad == "fiable", 1, 0)) / nrow(.) * 100,
                    pasa = round(.data$pasa, 2),
                    publicacion = dplyr::if_else(.data$pasa >= 50, "publicar tabulado", "no publicar tabulado"),
                    aprueba = paste0(.data$pasa, "% de estimaciones fiables")) %>%
      dplyr::select(-.data$pasa)
  }

  return(evaluacion)

}


