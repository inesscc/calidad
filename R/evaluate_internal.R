
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


evaluate_ine <- function(table, params, class = "calidad.mean") {

  # General case
  if (sum(class %in% c("calidad.mean", "calidad.size", "calidad.total")) == 1 ) {


    evaluacion <- table %>%
      dplyr::filter(!is.na(.data$n) & !is.na(.data$df) & !is.na(.data$cv)) %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                    eval_df = dplyr::if_else(df >= params$df, "sufficient df", "insufficient df"),
                    eval_cv = dplyr::case_when(
                      .data$cv <= params$cv_lower_ine  &  .data$cv > 0                ~ paste("cv <=", params$cv_lower_ine) ,
                      .data$cv > params$cv_lower_ine & .data$cv <= params$cv_upper_ine ~ paste("cv between", params$cv_lower_ine, "and", params$cv_upper_ine),
                      .data$cv > params$cv_upper_ine                                        ~ paste("cv >", params$cv_upper_ine)
                    ),
                    calidad = dplyr::case_when(
                      eval_n == "insufficient sample size" | eval_df == "insufficient df" | eval_cv == paste("cv >", params$cv_upper_ine)      ~ "no fiable",
                      eval_n == "sufficient sample size" & eval_df == "sufficient df" & eval_cv == paste("cv <=", params$cv_lower_ine)         ~ "fiable",
                      eval_n == "sufficient sample size" & eval_df == "sufficient df" & eval_cv ==  paste("cv between", params$cv_lower_ine, "and", params$cv_upper_ine) ~
                        "poco fiable"
                    )
      )

  # proportion case
  } else {

    evaluacion <- table %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                    eval_df = dplyr::if_else(.data$df >= params$df, "sufficient df", "insufficient df"),
                    prop_est = dplyr::case_when(.data$stat <= 0.5                     ~ "<= a 0.5",
                                                .data$stat < 1 & .data$stat > 0.5 ~ "> a 0.5",
                                                .data$stat >= 1                        ~ ">= a 1"),
                    tipo_eval = dplyr::if_else(.data$stat < 1, "Eval SE", "Eval CV"),
                    cuadratica = dplyr::if_else(.data$stat < 1, quadratic(.data$stat), NA_real_),
                    eval_se = dplyr::if_else(.data$stat < 1,
                                             dplyr::if_else(.data$se <= .data$cuadratica,
                                                            "admissible SE", "high SE"), NA_character_),
                    eval_cv = dplyr::if_else(.data$stat < 1, NA_character_,
                                             dplyr::case_when(cv <= params$cv_lower_ine                           ~ paste("cv <=", params$cv_lower_ine),
                                                              cv > params$cv_lower_ine & cv <= params$cv_upper_ine ~ paste("cv between", params$cv_lower_ine, "abd", params$cv_upper_ine),
                                                              cv > 0.3                                            ~ paste("cv >", params$cv_upper_ine)
                                             )),
                    calidad = dplyr::case_when(
                      stat <1 & eval_n == "insufficient sample size" | eval_df == "insufficient df"                                                  ~ "no fiable",
                      stat <1 & eval_n == "sufficient sample size" & eval_df == "sufficient df" & prop_est == "<= a 0.5" & eval_se == "admissible SE"  ~ "fiable",
                      stat <1 & eval_n == "sufficient sample size" & eval_df == "sufficient df" & prop_est == "<= a 0.5" & eval_se == "high SE"      ~ "poco fiable",
                      stat <1 & eval_n == "sufficient sample size" & eval_df == "sufficient df" & prop_est == "> a 0.5" & eval_se == "admissible SE"   ~ "fiable",
                      stat <1 & eval_n == "sufficient sample size" & eval_df == "sufficient df" & prop_est == "> a 0.5" & eval_se == "high SE"       ~ "poco fiable",
                      stat >= 1 & eval_n == "insufficient sample size" | eval_df == "insufficient df" | eval_cv == paste("cv >", params$cv_upper_ine) ~ "no fiable",
                      stat >= 1 & eval_n == "sufficient sample size" & eval_df == "sufficient df" & eval_cv == paste("cv <=", params$cv_lower_ine)    ~ "fiable",
                      stat >= 1 & eval_n == "sufficient sample size" & eval_df == "sufficient df" & eval_cv == paste("cv between", params$cv_lower_ine, "and", params$cv_upper_ine) ~ "poco fiable"))



  }



  return(evaluacion)
}


#-------------------------------------------------
evaluate_cepal <- function(table, params, class = "calidad.mean") {

  # General case
  if (sum(class %in% c("calidad.mean", "calidad.size", "calidad.total")) == 1 ) {

    evaluation <- table %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                    eval_ess = dplyr::if_else(.data$ess >= params$ess, "sufficient ess", "insufficient ess"),
                    eval_df = dplyr::if_else(df >= params$df, "sufficient df", "insufficient df"),
                    eval_cv = dplyr::if_else(cv < params$cv_cepal, "adequate cv", "non adequate cv")) %>%
      dplyr::mutate(tag = dplyr::case_when(
        eval_n == "insufficient sample size" | eval_ess == "insufficient ess"  ~ "supress",
        eval_df == "insufficient df"  ~ "review",
        eval_cv ==  "adequate cv"  ~ "publish"


      ))
  # Proportion case
  } else {

    evaluation <- table %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                    eval_ess = dplyr::if_else(.data$ess >= params$ess, "sufficient ess", "insufficient ess"),
                    eval_unweighted = dplyr::if_else(.data$unweighted >= params$unweighted , "sufficient cases", "insufficient cases"),
                    eval_df = dplyr::if_else(df >= params$df, "sufficient df", "insufficient df"),
                    eval_log_cv = dplyr::if_else(log_cv <= params$log_cv, "adequate log cv", "non adequate log cv"),
                    eval_cv = dplyr::if_else(cv < params$cv_cepal, "adequate cv", "non adequate cv")) %>%
      dplyr::mutate(tag = dplyr::case_when(
        eval_n == "insufficient sample size" | eval_ess == "insufficient ess" |
          eval_unweighted == "insufficient cases" | eval_log_cv == "non adequate log cv"  ~ "supress",
        eval_df == "insufficient df"  ~ "review",
        eval_cv ==  "adequate cv"  ~ "publish"
      ))


  }
  return(evaluation)


}

#-------------------------------------------------
  publish_table <- function(evaluation) {
    evaluation <- evaluation %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(.data$n) & !is.na(.data$df) & !is.na(.data$cv)) %>%
      dplyr::mutate(pasa = sum(dplyr::if_else(.data$calidad == "fiable", 1, 0)) / nrow(.) * 100,
                    pasa = round(.data$pasa, 2),
                    publicacion = dplyr::if_else(.data$pasa >= 50, "publicar tabulado", "no publicar tabulado"),
                    aprueba = paste0(.data$pasa, "% de estimaciones fiables")) %>%
      dplyr::select(-.data$pasa)

    return(evaluation)
  }

#-------------------------------------------------

combine_params <- function(deafult_params, user_params) {

  # Combine defaults params with user inputs
  deafult_params <- deafult_params[!names(deafult_params) %in% names(user_params)  ]
  final_params <- c(deafult_params, user_params)
  return(final_params)

}
