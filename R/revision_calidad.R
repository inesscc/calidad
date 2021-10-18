


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
#' \code{evaluate_mean} evauates the quality of mean estimation using the
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


evaluate_mean <- function(tabulado, condicion = NULL, publicar = FALSE, scheme = "chile", ...) {

  # Defaults params for cepal and INE Chile
  default_params_ine = list(df = 9, n = 60, cv_lower_ine = 0.15, cv_upper_ine = 0.3 )
  default_params_cepal = list(df = 9, n = 100, cv_cepal = 0.2, ess = 140)


  # Default scheme is INE Chile
  if (scheme == "chile") {


    # Combine defaults params with user inputs
    user_params <- list(...)
    final_params <- default_params_ine[!names(default_params_ine) %in% names(user_params)  ]
    params <- c(final_params, user_params)



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
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "n suficiente", "n insuficiente"),
                    eval_gl = dplyr::if_else(gl >= params$df, "gl suficiente", "gl insuficiente"),
                    eval_cv = dplyr::case_when(
                      .data$coef_var <= params$cv_lower_ine  &  .data$coef_var > 0                ~ paste("cv <=", params$cv_lower_ine) ,
                      .data$coef_var > params$cv_lower_ine & .data$coef_var <= params$cv_upper_ine ~ paste("cv entre", params$cv_lower_ine, "y", params$cv_upper_ine),
                      .data$coef_var > params$cv_upper_ine                                        ~ paste("cv >", params$cv_upper_ine)
                    ),
                    calidad = dplyr::case_when(
                      eval_n == "n insuficiente" | eval_gl == "gl insuficiente" | eval_cv == paste("cv >", params$cv_upper_ine)      ~ "no fiable",
                      eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == paste("cv <=", params$cv_lower_ine)         ~ "fiable",
                      eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv ==  paste("cv entre", params$cv_lower_ine, "y", params$cv_upper_ine) ~ "poco fiable"
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

  # ESTANDAR CEPAL
  } else if (scheme == "cepal") {

    # Combine defaults params with user inputs
    user_params <- list(...)
    final_params <- default_params_cepal[!names(default_params_cepal) %in% names(user_params)  ]
    params <- c(final_params, user_params)



    # Check that all the inputs are available
    check_ess <- names(tabulado) %>%  stringr::str_detect(pattern = "ess") %>% sum()

    if (check_ess != 1) {
      stop("ess must be used!")
    }

       evaluacion <- tabulado %>%
         dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                       eval_ess = dplyr::if_else(.data$ess >= params$ess, "sufficient ess", "insufficient ess"),
                       eval_df = dplyr::if_else(gl >= params$df, "sufficient df", "insufficient df"),
                       eval_cv = dplyr::if_else(coef_var < params$cv_cepal, "adequate cv", "non adequate cv")) %>%
         dplyr::mutate(tag = dplyr::case_when(
           eval_n == "insufficient sample size" | eval_ess == "insufficient ess"  ~ "supress",
           eval_df == "insufficient df"  ~ "review",
           eval_cv ==  "adequate cv"  ~ "publish"
         ))
  } else {

    stop("scheme must be cepal or chile")

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


evaluate_tot <- function(tabulado, condicion = NULL, publicar = FALSE,  scheme = "chile", ...) {


  # Defaults params for cepal and INE Chile
  default_params_ine = list(df = 9, n = 60, cv_lower_ine = 0.15, cv_upper_ine = 0.3 )
  default_params_cepal = list(df = 9, n = 100, cv_cepal = 0.2, ess = 140)



  # Default scheme is INE Chile
  if (scheme == "chile") {

    # Combine defaults params with user inputs
    user_params <- list(...)
    final_params <- default_params_ine[!names(default_params_ine) %in% names(user_params)  ]
    params <- c(final_params, user_params)


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
    dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "n suficiente", "n insuficiente"),
                  eval_gl = dplyr::if_else(gl >= params$df, "gl suficiente", "gl insuficiente"),
                  eval_cv = dplyr::case_when(
                    .data$coef_var <= params$cv_lower_ine     &  .data$coef_var > 0              ~ paste("cv <=", params$cv_lower_ine) ,
                    .data$coef_var > params$cv_lower_ine & .data$coef_var <= params$cv_upper_ine ~ paste("cv entre", params$cv_lower_ine, "y", params$cv_upper_ine),
                    .data$coef_var > params$cv_upper_ine                                        ~ paste("cv >", params$cv_upper_ine)
                  ),
                  calidad = dplyr::case_when(
                    eval_n == "n insuficiente" | eval_gl == "gl insuficiente" | eval_cv == paste("cv >", params$cv_upper_ine)      ~ "no fiable",
                    eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == paste("cv <=", params$cv_lower_ine)         ~ "fiable",
                    eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == paste("cv entre", params$cv_lower_ine, "y", params$cv_upper_ine) ~ "poco fiable"
                  ))

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

  # Cepal scheme

  } else if (scheme == "cepal") {

    # Combine defaults params with user inputs
    user_params <- list(...)
    final_params <- default_params_cepal[!names(default_params_cepal) %in% names(user_params)  ]
    params <- c(final_params, user_params)



    # Check that all the inputs are available
    check_ess <- names(tabulado) %>%  stringr::str_detect(pattern = "ess") %>% sum()

    if (check_ess != 1) {
      stop("ess must be used!")
    }

    evaluacion <- tabulado %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                    eval_ess = dplyr::if_else(.data$ess >= params$ess, "sufficient ess", "insufficient ess"),
                    eval_df = dplyr::if_else(gl >= params$df, "sufficient df", "insufficient df"),
                    eval_cv = dplyr::if_else(coef_var < params$cv_cepal, "adequate cv", "non adequate cv")) %>%
      dplyr::mutate(tag = dplyr::case_when(
        eval_n == "insufficient sample size" | eval_ess == "insufficient ess" ~ "supress",
        eval_df == "insufficient df"  ~ "review",
        eval_cv ==  "adequate cv"  ~ "publish"
      ))

  # If the user misspells  the scheme, a message is returned
  } else {
    stop("scheme must be cepal or chile")
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


evaluate_tot_con <- function(tabulado, condicion = NULL, publicar = FALSE, scheme = "chile", ...) {

  # Defaults params for cepal and INE Chile
  default_params_ine = list(df = 9, n = 60, cv_lower_ine = 0.15, cv_upper_ine = 0.3 )
  default_params_cepal = list(df = 9, n = 100, cv_cepal = 0.2, ess = 140)



  # Default scheme is INE Chile
  if (scheme == "chile") {


   # Combine defaults params with user inputs
   user_params <- list(...)
   final_params <- default_params_ine[!names(default_params_ine) %in% names(user_params)  ]
   params <- c(final_params, user_params)


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
    dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "n suficiente", "n insuficiente"),
                  eval_gl = dplyr::if_else(gl >= params$df, "gl suficiente", "gl insuficiente"),
                  eval_cv = dplyr::case_when(
                    .data$coef_var <= params$cv_lower_ine   &  .data$coef_var > 0               ~ paste("cv <=", params$cv_lower_ine) ,
                    .data$coef_var > params$cv_lower_ine & .data$coef_var <= params$cv_upper_ine ~ paste("cv entre", params$cv_lower_ine, "y", params$cv_upper_ine),
                    .data$coef_var > params$cv_upper_ine                                        ~ paste("cv >", params$cv_upper_ine)
                  ),
                  calidad = dplyr::case_when(
                    eval_n == "n insuficiente" | eval_gl == "gl insuficiente" | eval_cv == paste("cv >", params$cv_upper_ine)      ~ "no fiable",
                    eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == paste("cv <=", params$cv_lower_ine)         ~ "fiable",
                    eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv ==  paste("cv entre", params$cv_lower_ine, "y", params$cv_upper_ine) ~ "poco fiable"
                  ))

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

  # Cepal scheme

  } else if (scheme == "cepal") {

    # Combine defaults params with user inputs
    user_params <- list(...)
    final_params <- default_params_cepal[!names(default_params_cepal) %in% names(user_params)  ]
    params <- c(final_params, user_params)


    # Check that all the inputs are available
    check_ess <- names(tabulado) %>%  stringr::str_detect(pattern = "ess") %>% sum()

    if (check_ess != 1) {
      stop("ess must be used!")
    }


    evaluacion <- tabulado %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >=  params$n, "sufficient sample size", "insufficient sample size"),
                    eval_ess = dplyr::if_else(.data$ess >= params$ess, "sufficient ess", "insufficient ess"),
                    eval_df = dplyr::if_else(gl >= params$df, "sufficient df", "insufficient df"),
                    eval_cv = dplyr::if_else(coef_var < params$cv_cepal, "adequate cv", "non adequate cv")) %>%
      dplyr::mutate(tag = dplyr::case_when(
        eval_n == "insufficient sample size" | eval_ess == "insufficient ess"  ~ "supress",
        eval_df == "insufficient df"  ~ "review",
        eval_cv ==  "adequate cv"  ~ "publish"
      ))

    # If the user misspells  the scheme, a message is returned
  } else {
    stop("scheme must be cepal or chile")
  }

  return(evaluacion)
}


#---------------------------------------------------------------------




#' Evaluate the quality of total estimations
#'
#' \code{evaluate_size} evaluates the quality of size estimations using the
#' methodology created by INE Chile, which considers sample size, degrees of freedom and
#' the coefficient of variation.
#'
#' @param tabulado \code{dataframe} created by \code{crear_insumos_tot}
#' @param publicar \code{boolean} indicating if the evaluation of the complete table
#' must be added to the output. If it is TRUE, the function adds a new column to the \code{dataframe}
#' @param condicion \code{character} with the complete condition to filter the \code{dataframe}
#' @param ... the list of cepal parameters. The complete list of parameters is
#'
#' 1. General Parameters
#'
#' \itemize{
#'   \item \code{df} degrees of freedom. default: 9
#'   \item \code{n} sample size. default ine scheme is 60. default cepal scheme: 100
#' }
#'
#' 2. INE parameters
#' \itemize{
#'   \item \code{cv_lower_ine} lower limit for cv. default: 0.15
#'   \item \code{cv_upper_ine} upper limit for cv. default: 0.3
#' }
#'
#' 3. CEPAL parameters
#' \itemize{
#'   \item \code{cv_cepal} limit for cv. default: 0.2
#'   \item \code{ess} efective sample size. default: 140
#'   \item \code{unweighted} unweighted count. default: 50
#' }
#'
#' @return \code{dataframe} with all the columns included in the input table, plus a new column
#'containing a label indicating the evaluation of each estimation: reliable, bit reliable or unreliable
#'
#' @examples
#' library(survey)
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' evaluate_size(create_size(ocupado, dominios = zona+sexo, disenio = dc))
#'
#' @export
#'
#'

evaluate_size <- function(tabulado, condicion = NULL, publicar = FALSE,  scheme = "chile", ...) {


  # Defaults params for cepal and INE Chile
  default_params_ine = list(df = 9, n = 60, cv_lower_ine = 0.15, cv_upper_ine = 0.3 )
  default_params_cepal = list(df = 9, n = 100, cv_cepal = 0.2, ess = 140, unweighted = 50)



  # Default scheme is INE Chile
  if (scheme == "chile") {

    # Combine defaults params with user inputs
    user_params <- list(...)
    final_params <- default_params_ine[!names(default_params_ine) %in% names(user_params)  ]
    params <- c(final_params, user_params)


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
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "n suficiente", "n insuficiente"),
                    eval_gl = dplyr::if_else(gl >= params$df, "gl suficiente", "gl insuficiente"),
                    eval_cv = dplyr::case_when(
                      .data$coef_var <= params$cv_lower_ine     &  .data$coef_var > 0              ~ paste("cv <=", params$cv_lower_ine) ,
                      .data$coef_var > params$cv_lower_ine & .data$coef_var <= params$cv_upper_ine ~ paste("cv entre", params$cv_lower_ine, "y", params$cv_upper_ine),
                      .data$coef_var > params$cv_upper_ine                                        ~ paste("cv >", params$cv_upper_ine)
                    ),
                    calidad = dplyr::case_when(
                      eval_n == "n insuficiente" | eval_gl == "gl insuficiente" | eval_cv == paste("cv >", params$cv_upper_ine)      ~ "no fiable",
                      eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == paste("cv <=", params$cv_lower_ine)         ~ "fiable",
                      eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == paste("cv entre", params$cv_lower_ine, "y", params$cv_upper_ine) ~ "poco fiable"
                    ))

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

    # Cepal scheme

  } else if (scheme == "cepal") {


    # Combine defaults params with user inputs
    user_params <- list(...)
    final_params <- default_params_cepal[!names(default_params_cepal) %in% names(user_params)  ]
    params <- c(final_params, user_params)


    # Check that all the inputs are available
    check_ess <- names(tabulado) %>%
      stringr::str_detect(pattern = "ess") %>%
      sum()

    check_unweighted <- names(tabulado) %>%
      stringr::str_detect(pattern = "unweighted") %>%
      sum()

    if (check_ess[[1]] != 1 | check_unweighted[[1]] != 1) {

      stop("ess and unweighted must be used!")
    }


    evaluacion <- tabulado %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                    eval_ess = dplyr::if_else(.data$ess >= params$ess, "sufficient ess", "insufficient ess"),
                    eval_unweighted = dplyr::if_else(.data$unweighted >= params$unweighted , "sufficient cases", "insufficient cases"),
                    eval_df = dplyr::if_else(gl >= params$df, "sufficient df", "insufficient df"),
                    eval_cv = dplyr::if_else(coef_var < params$cv_cepal, "adequate cv", "non adequate cv")) %>%
      dplyr::mutate(tag = dplyr::case_when(
        eval_n == "insufficient sample size"  | eval_ess == "insufficient ess" |
          eval_unweighted == "insufficient cases"   ~ "supress",
        eval_df == "insufficient df"  ~ "review",
        eval_cv ==  "adequate cv"  ~ "publish"
      ))



    # If the user misspells  the scheme, a message is returned
  } else {
    stop("scheme must be cepal or chile")
  }


  return(evaluacion)
}












#----------------------------------------------------------------------



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

evaluate_median <- function(tabulado, condicion = NULL, scheme = "chile", publicar = FALSE, ...) {


  # Defaults params for cepal and INE Chile
  default_params_ine = list(df = 9, n = 60, cv_lower_ine = 0.15, cv_upper_ine = 0.3 )
  default_params_cepal = list(df = 9, n = 100, cv_cepal = 0.2, ess = 140)


  # Default scheme is INE Chile
  if (scheme == "chile") {

    # Combine defaults params with user inputs
    user_params <- list(...)
    final_params <- default_params_ine[!names(default_params_ine) %in% names(user_params)  ]
    params <- c(final_params, user_params)


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
    dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "n suficiente", "n insuficiente"),
                  eval_gl = dplyr::if_else(gl >= params$df, "gl suficiente", "gl insuficiente"),
                  eval_cv = dplyr::case_when(
                    .data$coef_var <= params$cv_lower_ine   &  .data$coef_var > 0               ~ paste("cv <=", params$cv_lower_ine) ,
                    .data$coef_var > params$cv_lower_ine & .data$coef_var <= params$cv_upper_ine ~ paste("cv entre", params$cv_lower_ine, "y", params$cv_upper_ine),
                    .data$coef_var > params$cv_upper_ine                                        ~ paste("cv >", params$cv_upper_ine)
                  ),
                  calidad = dplyr::case_when(
                    eval_n == "n insuficiente" | eval_gl == "gl insuficiente" | eval_cv == paste("cv >", params$cv_upper_ine)      ~ "no fiable",
                    eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == paste("cv <=", params$cv_lower_ine)         ~ "fiable",
                    eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == paste("cv entre", params$cv_lower_ine, "y", params$cv_upper_ine) ~ "poco fiable"
                  ))



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


    # ESTANDAR CEPAL
  } else if (scheme == "cepal") {


    # Combine defaults params with user inputs
    user_params <- list(...)
    final_params <- default_params_cepal[!names(default_params_cepal) %in% names(user_params)  ]
    params <- c(final_params, user_params)


    # Check that all the inputs are available
    check_ess <- names(tabulado) %>%  stringr::str_detect(pattern = "ess") %>% sum()
    check_unweighted <- names(tabulado) %>%  stringr::str_detect(pattern = "unweighted") %>% sum()

    if (check_ess != 1 | check_unweighted != 1) {
      stop("unweighted and ess must be used!")
    }

    evaluacion <- tabulado %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                    eval_ess = dplyr::if_else(.data$ess >= params$ess, "sufficient ess", "insufficient ess"),
                    eval_df = dplyr::if_else(gl >= params$df, "sufficient df", "insufficient df"),
                    eval_cv = dplyr::if_else(coef_var < params$cv_cepal, "adequate cv", "non adequate cv")) %>%
      dplyr::mutate(tag = dplyr::case_when(
        eval_n == "insufficient sample size" | eval_ess == "insufficient ess"  ~ "supress",
        eval_df == "insufficient df"  ~ "review",
        eval_cv ==  "adequate cv"  ~ "publish"
      ))
  } else {

    stop("scheme must be cepal or chile")

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

evaluate_prop <- function(tabulado, condicion = NULL, publicar = FALSE, scheme = "chile", ...) {

  # Defaults params for cepal and INE Chile
  default_params_ine = list(df = 9, n = 60, cv_lower_ine = 0.15, cv_upper_ine = 0.3 )
  default_params_cepal = list(df = 9, n = 100, cv_cepal = 0.2, ess = 140, unweighted = 50, log_cv = 0.175)


  # Default scheme is INE Chile
  if (scheme == "chile") {


    # Combine defaults params with user inputs
    user_params <- list(...)
    final_params <- default_params_ine[!names(default_params_ine) %in% names(user_params)  ]
    params <- c(final_params, user_params)

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
    dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "n suficiente", "n insuficiente"),
                  eval_gl = dplyr::if_else(.data$gl >= params$df, "gl suficiente", "gl insuficiente"),
                  prop_est = dplyr::case_when(.data$objetivo <= 0.5                     ~ "<= a 0.5",
                                              .data$objetivo < 1 & .data$objetivo > 0.5 ~ "> a 0.5",
                                              .data$objetivo >= 1                        ~ ">= a 1"),
                  tipo_eval = dplyr::if_else(.data$objetivo < 1, "Eval SE", "Eval CV"),
                  cuadratica = dplyr::if_else(.data$objetivo < 1, quadratic(.data$objetivo), NA_real_),
                  eval_se = dplyr::if_else(.data$objetivo < 1,
                                           dplyr::if_else(.data$se <= .data$cuadratica,
                                                          "SE adecuado", "SE alto"), NA_character_),
                  eval_cv = dplyr::if_else(.data$objetivo < 1, NA_character_,
                                           dplyr::case_when(coef_var <= params$cv_lower_ine                           ~ paste("cv <=", params$cv_lower_ine),
                                                            coef_var > params$cv_lower_ine & coef_var <= params$cv_upper_ine ~ paste("cv entre", params$cv_lower_ine, "y", params$cv_upper_ine),
                                                            coef_var > 30                                            ~ paste("cv >", params$cv_upper_ine)
                  )),
                  calidad = dplyr::case_when(
                    objetivo <1 & eval_n == "n insuficiente" | eval_gl == "gl insuficiente"                                                  ~ "no fiable",
                    objetivo <1 & eval_n == "n suficiente" & eval_gl == "gl suficiente" & prop_est == "<= a 0.5" & eval_se == "SE adecuado"  ~ "fiable",
                    objetivo <1 & eval_n == "n suficiente" & eval_gl == "gl suficiente" & prop_est == "<= a 0.5" & eval_se == "SE alto"      ~ "poco fiable",
                    objetivo <1 & eval_n == "n suficiente" & eval_gl == "gl suficiente" & prop_est == "> a 0.5" & eval_se == "SE adecuado"   ~ "fiable",
                    objetivo <1 & eval_n == "n suficiente" & eval_gl == "gl suficiente" & prop_est == "> a 0.5" & eval_se == "SE alto"       ~ "poco fiable",
                    objetivo >= 1 & eval_n == "n insuficiente" | eval_gl == "gl insuficiente" | eval_cv == paste("cv >", params$cv_upper_ine) ~ "no fiable",
                    objetivo >= 1 & eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == paste("cv <=", params$cv_lower_ine)    ~ "fiable",
                    objetivo >= 1 & eval_n == "n suficiente" & eval_gl == "gl suficiente" & eval_cv == paste("cv entre", params$cv_lower_ine, "y", params$cv_upper_ine) ~ "poco fiable"))


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


  # Cepal scheme
  } else if (scheme == "cepal") {

    # Combine defaults params with user inputs
    user_params <- list(...)
    final_params <- default_params_cepal[!names(default_params_cepal) %in% names(user_params)  ]
    params <- c(final_params, user_params)



    # Check that all the inputs are available
    check_ess <- names(tabulado) %>%  stringr::str_detect(pattern = "ess") %>% sum()
    check_unweighted <- names(tabulado) %>%  stringr::str_detect(pattern = "unweighted") %>% sum()
    check_log_cv <- names(tabulado) %>%  stringr::str_detect(pattern = "log_cv") %>% sum()

    if (check_ess != 1 | check_unweighted != 1 | check_log_cv != 1 ) {
      stop("log_cv, unweighted and ess must be used!")
    }

    evaluacion <- tabulado %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                    eval_ess = dplyr::if_else(.data$ess >= params$ess, "sufficient ess", "insufficient ess"),
                    eval_unweighted = dplyr::if_else(.data$unweighted >= params$unweighted , "sufficient cases", "insufficient cases"),
                    eval_df = dplyr::if_else(gl >= params$df, "sufficient df", "insufficient df"),
                    eval_log_cv = dplyr::if_else(log_cv <= params$log_cv, "adequate log cv", "non adequate log cv"),
                    eval_cv = dplyr::if_else(coef_var < params$cv_cepal, "adequate cv", "non adequate cv")) %>%
      dplyr::mutate(tag = dplyr::case_when(
        eval_n == "insufficient sample size" | eval_ess == "insufficient ess" |
          eval_unweighted == "insufficient cases" | eval_log_cv == "non adequate log cv"  ~ "supress",
        eval_df == "insufficient df"  ~ "review",
        eval_cv ==  "adequate cv"  ~ "publish"
      ))

    # If the user misspells  the scheme, a message is returned
  } else {
    stop("scheme must be cepal or chile")
  }


  return(evaluacion)

}


