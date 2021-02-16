
#---------------------------------------------------------------------
#' Calcula el valor de una función cuadrática
#'
#' Calcula una función diseñada por el INE, que se utiliza para hacer una comparación
#' con el error estándar. Si este último sobrepasa el valor de la funcíón cuadrática,
#' es un indicio de que el estimador tiene una alta varianza. Siguiendo la metodología
#' del INE, existen cálculos diferenciados para los estimadores que están por sobre y por
#' debajo de 0.5.
#'
#' @param p vector numérico que contiene los valores de cada una de las estimaciones de
#' proporción
#' @return vector numérico con el cálculo realizado
#'
#' @examples
#' cuadratica(c(0.1, 0.7, 0.5))

cuadratica <- function(p) {
  purrr::map_dbl(p, function(x) {
  if (x <= 0.5) {
    (x**(2/3))/9
  } else {
    ((1 - x)**(2/3))/9
  }
  })
}

#---------------------------------------------------------------------
#' Evalúa la calidad de las estimaciones de media
#'
#' Se utiliza la metodología publicada por el INE para la evaluación de la calidad
#' de las estimaciones. Se consideran 3 variables: tamaño muestral, grados de libertad y
#' coeficiente de variación.
#'
#' @param tabulado \code{dataframe} generado por la función \code{crear_insumos}. Contiene
#' todos los insumos necesarios para la evaluación.
#' @param condicion character con la condición de filtro
#' @return \code{dataframe} con todas las columnas que tiene el input, más una nueva que
#' contiene una etiqueta que da cuenta de la calidad: fiable, poco fiable o no fiable.
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' evaluacion_calidad(crear_insumos(gastot_hd, zona+sexo, dc))
#' @export


evaluar_calidad_media <- function(tabulado, condicion = NULL, publicar = FALSE) {

  #Aplicar la condición requerida por el usuario
  if (!is.null(condicion) ) {
    tabulado <- tabulado %>%
      dplyr::filter(!!rlang::parse_expr(condicion))
  }
  # Chequear si existen valores NA en los insumos. Si hay NAs, se manda un warning al usuario
  suma_na <- tabulado %>%
    dplyr::mutate(contiene_na = dplyr::if_else(is.na(n) | is.na(gl) | is.na(coef_var), 1, 0)) %>%
    dplyr::summarise(suma = sum(contiene_na)) %>%
    dplyr::pull(suma)

  # mandar un warning cuando se han exlcuido filas
  if (suma_na > 0) {
    warning(paste0("Se han excluido ", suma_na, " filas con NA en n, gl o cv"))
  }


  evaluacion <- tabulado %>%
    dplyr::filter(!is.na(n) & !is.na(gl) & !is.na(coef_var)) %>%
    dplyr::mutate(eval_n = dplyr::if_else(n >= 60, "n suficiente", "n insuficiente"),
           eval_gl = dplyr::if_else(gl >= 9, "gl suficiente", "gl insuficiente"),
           eval_cv = dplyr::case_when(
             coef_var <= 15                  ~ "cv <= 15",
             coef_var > 15 & coef_var <= 30  ~ "cv entre 15 y 30",
             coef_var > 30                   ~ "cv > 30"
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
      dplyr::filter(!is.na(n) & !is.na(gl) & !is.na(coef_var)) %>%
      dplyr::mutate(pasa = sum(dplyr::if_else(calidad == "fiable", 1, 0)) / nrow(.) * 100,
                    pasa = round(pasa, 2),
                    publicacion = dplyr::if_else(pasa >= 50, "publicar tabulado", "no publicar tabulado"),
                    aprueba = paste0("pasa el ", pasa, "%")) %>%
      dplyr::select(-pasa)


  }


  return(evaluacion)
}

#---------------------------------------------------------------------
#' Evalúa la calidad de las estimaciones de proporción
#'
#' Se utiliza la metodología publicada por el INE para la evaluación de la calidad
#' de las estimaciones. Se consideran 3 variables: tamaño muestral, grados de libertad y
#' error estándar.
#'
#' @param tabulado \code{dataframe} generado por la función \code{crear_insumos_prop}. Contiene
#' todos los insumos necesarios para la evaluación.
#' @param condicion character con la condición de filtro
#' @return \code{dataframe} con todas las columnas que tiene el input, más una nueva que
#' contiene una etiqueta que da cuenta de la calidad: fiable, poco fiable o no fiable.
#'
#' @importFrom magrittr `%>%`
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' evaluacion_calidad_prop(crear_insumos_prop(ocupado, zona+sexo, dc))
#' @export


evaluar_calidad_prop <- function(tabulado, condicion = NULL, publicar = FALSE) {

  #Aplicar la condición requerida por el usuario
  if (!is.null(condicion) ) {
    tabulado <- tabulado %>%
      dplyr::filter(!!rlang::parse_expr(condicion))
  }

  # Chequear si existen valores NA en los insumos. Si hay NAs, se manda un warning al usuario
  suma_na <- tabulado %>%
    dplyr::mutate(contiene_na = dplyr::if_else(is.na(n) | is.na(gl) | is.na(se), 1, 0)) %>%
    dplyr::summarise(suma = sum(contiene_na)) %>%
    dplyr::pull(suma)

  # mandar un warning cuando se han exlcuido filas
  if (suma_na > 0) {
    warning(paste0("Se han excluido ", suma_na, " filas con NA en n, gl o se"))
  }

  evaluacion <- tabulado %>%
    dplyr::mutate(eval_n = dplyr::if_else(n >= 60, "n suficiente", "n insuficiente"),
           eval_gl = dplyr::if_else(gl >= 9, "gl suficiente", "gl insuficiente"),
           prop_est = dplyr::if_else(objetivo <= 0.5, "<= a 0.5", "> a 0.5"),
           cuadratica = cuadratica(objetivo),
           eval_se = dplyr::if_else(se <= cuadratica, "se adecuado", "se alto"),
           calidad = dplyr::case_when(
             eval_n == "n insuficiente" | eval_gl == "gl insuficiente"                                                 ~ "no fiable",
             eval_n == "n suficiente" & eval_gl == "gl suficiente" & prop_est == "<= a 0.5" & eval_se == "se adecuado" ~ "fiable",
             eval_n == "n suficiente" & eval_gl == "gl suficiente" & prop_est == "<= a 0.5" & eval_se == "se alto"      ~ "poco fiable",
             eval_n == "n suficiente" & eval_gl == "gl suficiente" & prop_est == "> a 0.5" & eval_se == "se adecuado"  ~ "fiable",
             eval_n == "n suficiente" & eval_gl == "gl suficiente" & prop_est == "> a 0.5" & eval_se == "se alto"       ~ "poco fiable"
           )
    )

  # Criterio general para la publicación del tabulado
  if (publicar == TRUE) {
    evaluacion <- evaluacion %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(n) & !is.na(gl) & !is.na(se)) %>%
      dplyr::mutate(pasa = sum(dplyr::if_else(calidad == "fiable", 1, 0)) / nrow(.) * 100,
                    pasa = round(pasa, 2),
                    publicacion = dplyr::if_else(pasa >= 50, "publicar tabulado", "no publicar tabulado"),
                    aprueba = paste0("pasa el ", pasa, "%"))%>%
      dplyr::select(-pasa)
  }


  return(evaluacion)

}

#---------------------------------------------------------------------
#' Evalúa la calidad de las estimaciones de totales
#'
#' Se utiliza la metodología publicada por el INE para la evaluación de la calidad
#' de las estimaciones. Se consideran 3 variables: tamaño muestral, grados de libertad y
#' coeficiente de variación.
#'
#' @param tabulado \code{dataframe} generado por la función \code{crear_insumos}. Contiene
#' todos los insumos necesarios para la evaluación.
#' @param condicion character con la condición de filtro
#' @return \code{dataframe} con todas las columnas que tiene el input, más una nueva que
#' contiene una etiqueta que da cuenta de la calidad: fiable, poco fiable o no fiable.
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' evaluacion_calidad_tot(crear_insumos_tot(ocupado_int, zona+sexo, dc))
#' @export


evaluar_calidad_tot <- function(tabulado, condicion = NULL, publicar = FALSE) {

  #Aplicar la condición requerida por el usuario
  if (!is.null(condicion) ) {
    tabulado <- tabulado %>%
      dplyr::filter(!!rlang::parse_expr(condicion))
  }

  # Chequear si existen valores NA en los insumos. Si hay NAs, se manda un warning al usuario
  suma_na <- tabulado %>%
    dplyr::mutate(contiene_na = dplyr::if_else(is.na(n) | is.na(gl) | is.na(coef_var), 1, 0)) %>%
    dplyr::summarise(suma = sum(contiene_na)) %>%
    dplyr::pull(suma)

  # mandar un warning cuando se han exlcuido filas
  if (suma_na > 0) {
    warning(paste0("Se han excluido ", suma_na, " filas con NA en n, gl o cv"))
  }


  evaluacion <- tabulado %>%
    dplyr::mutate(eval_n = dplyr::if_else(n >= 60, "n suficiente", "n insuficiente"),
                  eval_gl = dplyr::if_else(gl >= 9, "gl suficiente", "gl insuficiente"),
                  eval_cv = dplyr::case_when(
                    coef_var <= 15                  ~ "cv <= 15",
                    coef_var > 15 & coef_var <= 30  ~ "cv entre 15 y 30",
                    coef_var > 30                   ~ "cv > 30"
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
      dplyr::filter(!is.na(n) & !is.na(gl) & !is.na(coef_var)) %>%
      dplyr::mutate(pasa = sum(dplyr::if_else(calidad == "fiable", 1, 0)) / nrow(.) * 100,
                    pasa = round(pasa, 2),
                    publicacion = dplyr::if_else(pasa >= 50, "publicar tabulado", "no publicar tabulado"),
                    aprueba = paste0("pasa el ", pasa, "%")) %>%
      dplyr::select(-pasa)
  }



  return(evaluacion)
}



#---------------------------------------------------------------------
#' Evalúa la calidad de las estimaciones de sumas
#'
#' Se utiliza la metodología publicada por el INE para la evaluación de la calidad
#' de las estimaciones. Se consideran 3 variables: tamaño muestral, grados de libertad y
#' coeficiente de variación.
#'
#' @param tabulado \code{dataframe} generado por la función \code{crear_insumos}. Contiene
#' todos los insumos necesarios para la evaluación.
#' @param condicion character con la condición de filtro
#' @return \code{dataframe} con todas las columnas que tiene el input, más una nueva que
#' contiene una etiqueta que da cuenta de la calidad: fiable, poco fiable o no fiable.
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' evaluacion_calidad_suma(crear_insumos_tot(ocupado_int, zona+sexo, dc))
#' @export


evaluar_calidad_tot_con <- function(tabulado, condicion = NULL, publicar = FALSE) {

  #Aplicar la condición requerida por el usuario
  if (!is.null(condicion) ) {
    tabulado <- tabulado %>%
      dplyr::filter(!!rlang::parse_expr(condicion))
  }

  # Chequear si existen valores NA en los insumos. Si hay NAs, se manda un warning al usuario
  suma_na <- tabulado %>%
    dplyr::mutate(contiene_na = dplyr::if_else(is.na(n) | is.na(gl) | is.na(coef_var), 1, 0)) %>%
    dplyr::summarise(suma = sum(contiene_na)) %>%
    dplyr::pull(suma)

  # mandar un warning cuando se han exlcuido filas
  if (suma_na > 0) {
    warning(paste0("Se han excluido ", suma_na, " filas con NA en n, gl o cv"))
  }


  evaluacion <- tabulado %>%
    dplyr::mutate(eval_n = dplyr::if_else(n >= 60, "n suficiente", "n insuficiente"),
                  eval_gl = dplyr::if_else(gl >= 9, "gl suficiente", "gl insuficiente"),
                  eval_cv = dplyr::case_when(
                    coef_var <= 15                  ~ "cv <= 15",
                    coef_var > 15 & coef_var <= 30  ~ "cv entre 15 y 30",
                    coef_var > 30                   ~ "cv > 30"
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
      dplyr::filter(!is.na(n) & !is.na(gl) & !is.na(coef_var)) %>%
      dplyr::mutate(pasa = sum(dplyr::if_else(calidad == "fiable", 1, 0)) / nrow(.) * 100,
                    pasa = round(pasa, 2),
                    publicacion = dplyr::if_else(pasa >= 50, "publicar tabulado", "no publicar tabulado"),
                    aprueba = paste0("pasa el ", pasa, "%")) %>%
      dplyr::select(-pasa)



  }



  return(evaluacion)
}


#---------------------------------------------------------------------
#' Evalúa la calidad de las estimaciones de mediana
#'
#' Se utiliza la metodología publicada por el INE para la evaluación de la calidad
#' de las estimaciones. Se consideran 3 variables: tamaño muestral, grados de libertad y
#' coeficiente de variación.
#'
#' @param tabulado \code{dataframe} generado por la función \code{crear_insumos}. Contiene
#' todos los insumos necesarios para la evaluación.
#' @param condicion character con la condición de filtro
#' @return \code{dataframe} con todas las columnas que tiene el input, más una nueva que
#' contiene una etiqueta que da cuenta de la calidad: fiable, poco fiable o no fiable.
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' evaluacion_calidad(crear_insumos(gastot_hd, zona+sexo, dc))
#' @export


evaluar_calidad_mediana <- function(tabulado, condicion = NULL, publicar = FALSE) {

  #Aplicar la condición requerida por el usuario
  if (!is.null(condicion) ) {
    tabulado <- tabulado %>%
      dplyr::filter(!!rlang::parse_expr(condicion))
  }
  # Chequear si existen valores NA en los insumos. Si hay NAs, se manda un warning al usuario
  suma_na <- tabulado %>%
    dplyr::mutate(contiene_na = dplyr::if_else(is.na(n) | is.na(gl) | is.na(coef_var), 1, 0)) %>%
    dplyr::summarise(suma = sum(contiene_na)) %>%
    dplyr::pull(suma)

  # mandar un warning cuando se han exlcuido filas
  if (suma_na > 0) {
    warning(paste0("Se han excluido ", suma_na, " filas con NA en n, gl o cv"))
  }


  evaluacion <- tabulado %>%
    dplyr::filter(!is.na(n) & !is.na(gl) & !is.na(coef_var)) %>%
    dplyr::mutate(eval_n = dplyr::if_else(n >= 60, "n suficiente", "n insuficiente"),
                  eval_gl = dplyr::if_else(gl >= 9, "gl suficiente", "gl insuficiente"),
                  eval_cv = dplyr::case_when(
                    coef_var <= 15                  ~ "cv <= 15",
                    coef_var > 15 & coef_var <= 30  ~ "cv entre 15 y 30",
                    coef_var > 30                   ~ "cv > 30"
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
      dplyr::filter(!is.na(n) & !is.na(gl) & !is.na(coef_var)) %>%
      dplyr::mutate(pasa = sum(dplyr::if_else(calidad == "fiable", 1, 0)) / nrow(.) * 100,
                    pasa = round(pasa, 2),
                    publicacion = dplyr::if_else(pasa >= 50, "publicar tabulado", "no publicar tabulado"),
                    aprueba = paste0("pasa el ", pasa, "%")) %>%
      dplyr::select(-pasa)


  }


  return(evaluacion)
}


