
# Check that all the inputs are available
check_cepal_inputs <- function(table, var) {
  check_ess <- names(table) %>%  stringr::str_detect(pattern =  var) %>% sum()
  if (check_ess != 1) {stop(paste(var, "must be used!"))}
}
#---------------------------------------------------------------------
#'
#' Calcula el valor de una función cuadrática
#'
#' \code{quadratic} returns the output of a particular function created by INE Chile, which
#' is assessed at the value of the estimated proportion from a sample. If the output of the
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
#--------------------------------------------------------------------
assess_ine <- function(table, params, class = "calidad.mean") {

  # General case
  if (sum(class %in% c("calidad.mean", "calidad.size", "calidad.total")) == 1 ) {

    evaluacion <- table %>%
      dplyr::filter(!is.na(.data$n) & !is.na(.data$df) & !is.na(.data$cv)) %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                    eval_df = dplyr::if_else(.data$df >= params$df, "sufficient df", "insufficient df"),
                    eval_cv = dplyr::case_when(
                      .data$cv <= params$cv_lower_ine  &  .data$cv > 0                ~ paste("cv <=", params$cv_lower_ine) ,
                      .data$cv > params$cv_lower_ine & .data$cv <= params$cv_upper_ine ~ paste("cv between", params$cv_lower_ine, "and", params$cv_upper_ine),
                      .data$cv > params$cv_upper_ine                                        ~ paste("cv >", params$cv_upper_ine)
                    ),
                    label = dplyr::case_when(
                      eval_n == "insufficient sample size" | eval_df == "insufficient df" | eval_cv == paste("cv >", params$cv_upper_ine)      ~ "non-reliable",
                      eval_n == "sufficient sample size" & eval_df == "sufficient df" & eval_cv == paste("cv <=", params$cv_lower_ine)         ~ "reliable",
                      eval_n == "sufficient sample size" & eval_df == "sufficient df" & eval_cv ==  paste("cv between", params$cv_lower_ine, "and", params$cv_upper_ine) ~
                        "weakly reliable"
                    )
      )
    # proportion case
  } else {

    evaluacion <- table %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                    eval_df = dplyr::if_else(.data$df >= params$df, "sufficient df", "insufficient df"),
                    prop_est = dplyr::case_when(.data$stat <= 0.5                     ~ "<= 0.5",
                                                .data$stat < 1 & .data$stat > 0.5 ~ "> 0.5",
                                                .data$stat >= 1                        ~ ">= 1"),
                    eval_type = dplyr::if_else(.data$stat < 1, "Eval SE", "Eval CV"),
                    quadratic = dplyr::if_else(.data$stat < 1, quadratic(.data$stat), NA_real_),
                    eval_se = dplyr::if_else(.data$stat < 1,
                                             dplyr::if_else(.data$se <= .data$quadratic,
                                                            "admissible SE", "high SE"), NA_character_),
                    eval_cv = dplyr::if_else(.data$stat < 1, NA_character_,
                                             dplyr::case_when(cv <= params$cv_lower_ine                           ~ paste("cv <=", params$cv_lower_ine),
                                                              cv > params$cv_lower_ine & cv <= params$cv_upper_ine ~ paste("cv between", params$cv_lower_ine, "and", params$cv_upper_ine),
                                                              cv > 0.3                                            ~ paste("cv >", params$cv_upper_ine)
                                             )),
                    label = dplyr::case_when(
                      stat <1 & eval_n == "insufficient sample size" | eval_df == "insufficient df"                                                  ~ "non-reliable",
                      stat <1 & eval_n == "sufficient sample size" & eval_df == "sufficient df" & prop_est == "<= 0.5" & eval_se == "admissible SE"  ~ "reliable",
                      stat <1 & eval_n == "sufficient sample size" & eval_df == "sufficient df" & prop_est == "<= 0.5" & eval_se == "high SE"      ~ "weakly reliable",
                      stat <1 & eval_n == "sufficient sample size" & eval_df == "sufficient df" & prop_est == "> 0.5" & eval_se == "admissible SE"   ~ "reliable",
                      stat <1 & eval_n == "sufficient sample size" & eval_df == "sufficient df" & prop_est == "> 0.5" & eval_se == "high SE"       ~ "weakly reliable",
                      stat >= 1 & eval_n == "insufficient sample size" | eval_df == "insufficient df" | eval_cv == paste("cv >", params$cv_upper_ine) ~ "non-reliable",
                      stat >= 1 & eval_n == "sufficient sample size" & eval_df == "sufficient df" & eval_cv == paste("cv <=", params$cv_lower_ine)    ~ "reliable",
                      stat >= 1 & eval_n == "sufficient sample size" & eval_df == "sufficient df" & eval_cv == paste("cv between", params$cv_lower_ine, "and", params$cv_upper_ine) ~ "weakly reliable"))

  }
  return(evaluacion)
}
#-------------------------------------------------
assess_cepal2020 <- function(table, params, class = "calidad.mean") {
  # General case
  if (sum(class %in% c("calidad.mean", "calidad.size", "calidad.total")) == 1 ) {

    evaluation <- table %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                    eval_ess = dplyr::if_else(.data$ess >= params$ess, "sufficient ess", "insufficient ess"),
                    eval_unweighted = dplyr::if_else(.data$unweighted >= params$unweighted , "sufficient cases", "insufficient cases"),
                    eval_df = dplyr::if_else(.data$df >= params$df, "sufficient df", "insufficient df"),
                    eval_cv = dplyr::if_else(.data$cv < params$cv_cepal, "adequate cv", "non adequate cv")) %>%
      dplyr::mutate(label = dplyr::case_when(
        eval_n == "insufficient sample size" | eval_ess == "insufficient ess" | eval_unweighted == "insufficient cases" ~ "supress",
        eval_df == "insufficient df"  ~ "review",
        eval_cv ==  "adequate cv"  ~ "publish"


      ))
    # Proportion case
  } else {

    evaluation <- table %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                    eval_ess = dplyr::if_else(.data$ess >= params$ess, "sufficient ess", "insufficient ess"),
                    eval_unweighted = dplyr::if_else(.data$unweighted >= params$unweighted , "sufficient cases", "insufficient cases"),
                    eval_df = dplyr::if_else(.data$df >= params$df, "sufficient df", "insufficient df"),
                    eval_log_cv = dplyr::if_else(.data$log_cv <= params$log_cv, "adequate log cv", "non adequate log cv"),
                    eval_cv = dplyr::if_else(.data$cv < params$cv_cepal, "adequate cv", "non adequate cv")) %>%
      dplyr::mutate(label = dplyr::case_when(
        eval_n == "insufficient sample size" | eval_ess == "insufficient ess" |
          eval_unweighted == "insufficient cases" | eval_log_cv == "non adequate log cv"  ~ "supress",
        eval_df == "insufficient df" | eval_cv == "non adequate cv" ~ "review",
        eval_cv ==  "adequate cv"  ~ "publish"
      ))

  }

  # Add cepal 2020 class to the final object
  evaluation <- add_class(evaluation, "cepal2020.eval")

  return(evaluation)


}
#-------------------------------------------------
###################
utils::globalVariables(c("eval_deff", "eval_ess"))

assess_cepal2023 <- function(table, params, class = "calidad.mean", domain_info = FALSE) {
  evaluation <- table %>%
    dplyr::mutate(eval_deff = dplyr::case_when(.data$deff >= 1 ~ "Sufficient deff",
                                        .data$deff < 1 & domain_info & .data$n >= params$n ~ "Sufficient deff",
                                        TRUE ~ "non-reliable")) %>%
    dplyr::mutate(
      eval_ess = dplyr::if_else(eval_deff == "Sufficient deff" & .data$ess >= params$ess, "Sufficient ess", "non-reliable"),
      eval_df = dplyr::if_else(eval_ess == "Sufficient ess" & .data$df >= params$df, "Sufficient df",
                               dplyr::if_else(eval_ess == "Sufficient ess" & .data$df < params$df & !domain_info, "non-reliable",
                                              dplyr::if_else(eval_ess == "Sufficient ess" & .data$df < params$df & domain_info, "Sufficient df", "non-reliable")))
    )
  if (sum(class %in% c("calidad.mean", "calidad.size", "calidad.total")) == 1) {
    evaluation <- evaluation %>%
      dplyr::mutate(label = dplyr::case_when(
        eval_df == "Sufficient df" & .data$cv > params$cv_upper_cepal ~ "non-reliable",
        eval_df == "Sufficient df" & .data$cv > params$cv_lower_cepal & .data$cv <= params$cv_upper_cepal & .data$unweighted < params$CCNP_a ~ "non-reliable",
        eval_df == "Sufficient df" & .data$cv > params$cv_lower_cepal & .data$cv <= params$cv_upper_cepal & .data$unweighted >= params$CCNP_a ~ "weakly-reliable",
        eval_df == "Sufficient df" & .data$cv <= params$cv_lower_cepal & .data$unweighted >= params$CCNP_b ~ "reliable",
        eval_df == "Sufficient df" & .data$cv <= params$cv_lower_cepal & .data$unweighted < params$CCNP_b & .data$unweighted >= params$CCNP_a ~ "weakly-reliable",
        eval_df == "Sufficient df" & .data$cv <= params$cv_lower_cepal & .data$unweighted < params$CCNP_a ~ "non-reliable",
        TRUE ~ "non-reliable"
      ))
    #proportion
  } else {
    if (!"log_cv" %in% colnames(table)) {
      stop("log_cv must be used!")
    }
    evaluation <- evaluation %>%
      dplyr::mutate(label = dplyr::case_when(
        eval_df == "Sufficient df" & .data$stat < 0.5 & .data$log_cv <= params$cvlog_max & .data$unweighted >= params$CCNP_b ~ "reliable",
        eval_df == "Sufficient df" & .data$stat < 0.5 & .data$log_cv <= params$cvlog_max & .data$unweighted < params$CCNP_b & .data$unweighted >= params$CCNP_a ~ "weakly-reliable",
        eval_df == "Sufficient df" & .data$stat < 0.5 & .data$log_cv <= params$cvlog_max & .data$unweighted < params$CCNP_a ~ "non-reliable",
        eval_df == "Sufficient df" & .data$stat >= 0.5 & .data$log_cv <= params$cvlog_max & .data$unweighted >= params$CCNP_b ~ "reliable",
        eval_df == "Sufficient df" & .data$stat >= 0.5 & .data$log_cv <= params$cvlog_max & .data$unweighted < params$CCNP_b & .data$unweighted >= params$CCNP_a ~ "weakly-reliable",
        eval_df == "Sufficient df" & .data$stat >= 0.5 & .data$log_cv <= params$cvlog_max & .data$unweighted < params$CCNP_a ~ "non-reliable",
        eval_df == "Sufficient df" & .data$stat >= 0.5 & .data$log_cv > params$cvlog_max & .data$unweighted >= params$CCNP_a ~ "weakly-reliable",
        eval_df == "Sufficient df" & .data$stat >= 0.5 & .data$log_cv > params$cvlog_max & .data$unweighted < params$CCNP_a ~ "non-reliable",
        TRUE ~ "non-reliable"
      ))
  }
  evaluation <- add_class(evaluation, "cepal2023.eval")
  return(evaluation)
}




#-------------------------------------------------
publish_table <- function(evaluation) {
  evaluation <- evaluation %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$n) & !is.na(.data$df) & !is.na(.data$cv)) %>%
    dplyr::mutate(pasa = sum(dplyr::if_else(.data$label == "reliable", 1, 0)) / nrow(.) * 100,
                  pasa = round(.data$pasa, 2),
                  publication = dplyr::if_else(.data$pasa >= 50, "publish", "do not publish"),
                  pass = paste0(.data$pasa, "% reliable estimates")) %>%
    dplyr::select(-"pasa")
  return(evaluation)
}
#-------------------------------------------------
combine_params <- function(default_params, user_params) {
  # Combine defaults params with user inputs
  default_params <- default_params[!names(default_params) %in% names(user_params)]
  final_params <- c(default_params, user_params)
  return(final_params)
}

