
# Check that all the inputs are available
check_cepal_inputs <- function(table, var) {
  check_ess <- names(table) %>%  stringr::str_detect(pattern =  var) %>% sum()
  if (check_ess != 1) {stop(paste(var, "must be used!"))}
}


## check n_obj
check_n_obj_var <- function(table_n_obj, table , var = 'n_obj'){

  if(! var %in% names(table_n_obj)){
    if(!is.null(table_n_obj)){
      stop("Oops! n_obj missing in table_n_obj object. Please review your data.")
    }
    message('n_obj missing in table_n_obj object')

    if((! var %in% names(table))){
      warning('n_obj missing in the table. \n
              The process will consider estimations with n < 30 as non-reliable.')

      return(FALSE)    # No merge

    }else{
      message('n_obj in table!')
      return(FALSE)    # No merge
    }

  }else{

    return(TRUE)       # merge
  }
}

## check type col
check_type_cols <- function(tabla1, tabla2, columnas_comunes) {

  tipos_t1 <- sapply(tabla1 %>% dplyr::select(dplyr::all_of(columnas_comunes)), class)
  tipos_t2 <- sapply(tabla2 %>% dplyr::select(dplyr::all_of(columnas_comunes)), class)

  comparacion <- data.frame(
    col = columnas_comunes,
    table1 = sapply(tipos_t1[columnas_comunes], paste, collapse = ", "),
    table2 = sapply(tipos_t2[columnas_comunes], paste, collapse = ", "),
    coincide = tipos_t1[columnas_comunes] == tipos_t2[columnas_comunes],
    stringsAsFactors = FALSE)

    if(sum(comparacion$coincide)!=length(columnas_comunes)){
      print(comparacion)
      stop("Columns types doesn't match. Please review your data.")

    }
}


merge_columns <- function(table, table_n_obj){

  columns <- intersect(names(table), names(table_n_obj))

  check_type_cols(table, table_n_obj, columns)

  if(! (table_n_obj %>% dplyr::pull(.data$n_obj) %>% is.numeric())){
    warning('Oops! n_obj is not numeric. Please review your data. \n
            The process will consider estimations with n < 30 as unreliable')
    return(table)
  }

  if((length(columns)>0) & (nrow(table_n_obj) == nrow(table))){

    table_merge <- table %>%
      dplyr::left_join(table_n_obj, by = columns)


    if(nrow(table_merge) != nrow(table)){
      stop("Oops! Joining tables resulted in a different number of rows. Please review your data.")

      return(table)

    }else if(sum(is.na(table_merge %>% dplyr::pull(.data$n_obj)))>0){

      if (sum(table_merge$n<30, na.rm = T)>0){

        stop("Oops! NA values found in n_obj column and some rows where n < 30. Please review your data.")

      }else{

        warning("Oops! NA values found in n_obj column. The process will skip the sample recovery verification for those rows.")

        return(table_merge)
      }

      return(table)

    }else{
      return(table_merge)
    }


  }else{

    stop("Columns are missing in the table or the number of rows in table_n_obj and the table don't match. Please review your data or manually add n_obj to the table")

    return(table)
  }

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
assess_ine <- function(table, params, class = "calidad.mean", ratio_between_0_1 = TRUE) {

  # General case
  if (sum(class %in% c("calidad.mean", "calidad.size", "calidad.total")) == 1 | (sum(class %in% 'calidad.prop') == 1 & (sum(table$stat>1)>0 | !ratio_between_0_1))) {

    if ((ratio_between_0_1) & sum(class %in% 'calidad.prop') == 1){
      warning('Oops! A ratio estimation greater than 1 was detected. The assessment will use cv.')
    }


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
                    prop_est = dplyr::case_when(.data$stat <= 0.5                 ~ "<= 0.5",
                                                .data$stat < 1 & .data$stat > 0.5 ~ "> 0.5"),

                    quadratic = dplyr::if_else(.data$stat < 1, quadratic(.data$stat), NA_real_),

                    eval_se = dplyr::if_else(.data$se <= .data$quadratic, "admissible SE", "high SE"),

                    label = dplyr::case_when(
                      eval_n == "insufficient sample size" | eval_df == "insufficient df"                                                  ~ "non-reliable",
                      eval_n == "sufficient sample size" & eval_df == "sufficient df" & prop_est == "<= 0.5" & eval_se == "admissible SE"  ~ "reliable",
                      eval_n == "sufficient sample size" & eval_df == "sufficient df" & prop_est == "<= 0.5" & eval_se == "high SE"      ~ "weakly reliable",
                      eval_n == "sufficient sample size" & eval_df == "sufficient df" & prop_est == "> 0.5" & eval_se == "admissible SE"   ~ "reliable",
                      eval_n == "sufficient sample size" & eval_df == "sufficient df" & prop_est == "> 0.5" & eval_se == "high SE"       ~ "weakly reliable"
                      )
                    )

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
# CEPAL 2023
utils::globalVariables(c("eval_deff", "eval_ess"))

assess_cepal2023 <- function(table, params, class = "calidad.mean", domain_info = FALSE, low_df_justified =FALSE, ratio_between_0_1 = TRUE) {

  evaluation <- table %>%
    dplyr::mutate(eval_deff = dplyr::case_when(.data$deff >= 1 ~ "Sufficient deff",
                                               .data$deff < 1 & domain_info & .data$n >= params$n ~ "Sufficient deff",
                                               TRUE ~ "non-reliable")) %>%
    dplyr::mutate(
      eval_ess = dplyr::if_else(eval_deff == "Sufficient deff" & .data$ess >= params$ess, "Sufficient ess", "non-reliable"),
      eval_df = dplyr::if_else(eval_ess == "Sufficient ess" & .data$df >= params$df, "Sufficient df",
                               dplyr::if_else(eval_ess == "Sufficient ess" & .data$df < params$df & domain_info & low_df_justified , "Sufficient df",
                                              "non-reliable"))
    )

  if ((sum(class %in% c("calidad.mean", "calidad.size", "calidad.total")) == 1) | (sum(class %in% 'calidad.prop') == 1 & (sum(table$stat>1)>0 | !ratio_between_0_1))) {

    if ((ratio_between_0_1) & sum(class %in% 'calidad.prop') == 1){
      warning('Oops! A ratio estimation greater than 1 was detected. The assessment will use cv.')
    }

    evaluation <- evaluation %>%
      dplyr::mutate(
        eval_cv = dplyr::case_when(
          .data$cv > params$cv_upper_cepal ~ paste("cv >", params$cv_upper_cepal),
          .data$cv > params$cv_lower_cepal & .data$cv <= params$cv_upper_cepal ~ paste("cv between", params$cv_lower_cepal, "and", params$cv_upper_cepal),
          .data$cv <= params$cv_lower_cepal ~ paste("cv <=", params$cv_lower_cepal))
        ) %>%

      dplyr::mutate(label = dplyr::case_when(
        eval_df == "Sufficient df" & eval_cv == paste("cv >", params$cv_upper_cepal) ~ "non-reliable",
        eval_df == "Sufficient df" & eval_cv == paste("cv between", params$cv_lower_cepal, "and", params$cv_upper_cepal) & .data$unweighted < params$CCNP_a ~ "non-reliable",
        eval_df == "Sufficient df" & eval_cv == paste("cv between", params$cv_lower_cepal, "and", params$cv_upper_cepal) & .data$unweighted >= params$CCNP_a ~ "weakly-reliable",
        eval_df == "Sufficient df" & eval_cv == paste("cv <=", params$cv_lower_cepal) & .data$unweighted >= params$CCNP_b ~ "reliable",
        eval_df == "Sufficient df" & eval_cv == paste("cv <=", params$cv_lower_cepal) & .data$unweighted < params$CCNP_b & .data$unweighted >= params$CCNP_a ~ "weakly-reliable",
        eval_df == "Sufficient df" & eval_cv == paste("cv <=", params$cv_lower_cepal) & .data$unweighted < params$CCNP_a ~ "non-reliable",
        TRUE ~ "non-reliable"
      ))
    #proportion
  } else {
    if (!"log_cv" %in% colnames(table)) {
      stop("log_cv must be used!")
    }
    evaluation <- evaluation %>%
      dplyr::mutate(eval_log_cv = dplyr::case_when(
        .data$log_cv <= params$cvlog_max ~ paste("log_cv <=", params$cvlog_max),
        .data$log_cv > params$cvlog_max ~ paste("log_cv >", params$cvlog_max)

        )) %>%

      dplyr::mutate(label = dplyr::case_when(
        eval_df == "Sufficient df" & .data$stat < 0.5 & eval_log_cv == paste("log_cv <=", params$cvlog_max) & .data$unweighted >= params$CCNP_b ~ "reliable",
        eval_df == "Sufficient df" & .data$stat < 0.5 &  eval_log_cv == paste("log_cv <=", params$cvlog_max) & .data$unweighted < params$CCNP_b & .data$unweighted >= params$CCNP_a ~ "weakly-reliable",
        eval_df == "Sufficient df" & .data$stat < 0.5 &  eval_log_cv == paste("log_cv <=", params$cvlog_max) & .data$unweighted < params$CCNP_a ~ "non-reliable",
        eval_df == "Sufficient df" & .data$stat >= 0.5 & eval_log_cv == paste("log_cv <=", params$cvlog_max) & .data$unweighted >= params$CCNP_b ~ "reliable",
        eval_df == "Sufficient df" & .data$stat >= 0.5 & eval_log_cv == paste("log_cv <=", params$cvlog_max) & .data$unweighted < params$CCNP_b & .data$unweighted >= params$CCNP_a ~ "weakly-reliable",
        eval_df == "Sufficient df" & .data$stat >= 0.5 & eval_log_cv == paste("log_cv <=", params$cvlog_max) & .data$unweighted < params$CCNP_a ~ "non-reliable",
        eval_df == "Sufficient df" & .data$stat >= 0.5 & eval_log_cv == paste("log_cv >", params$cvlog_max) & .data$unweighted >= params$CCNP_a ~ "weakly-reliable",
        eval_df == "Sufficient df" & .data$stat >= 0.5 & eval_log_cv == paste("log_cv >", params$cvlog_max) & .data$unweighted < params$CCNP_a ~ "non-reliable",
        TRUE ~ "non-reliable"
      ))
  }
  evaluation <- add_class(evaluation, "cepal2023.eval")
  return(evaluation)
}

#-------------------------------------------------

## economicas standard
assess_economicas <- function(table, params, class = "calidad.mean", domain_info = FALSE, ratio_between_0_1 = TRUE) {

  if('n_obj' %in% names(table)){
    if(table %>% dplyr::pull(.data$n_obj) %>% is.numeric()){
      table <- table %>%
        dplyr::mutate(compliance_rate = .data$n/.data$n_obj)

    }else{
      table <- table %>%
        dplyr::mutate(compliance_rate = NA)
    }

  }else{
    table <- table %>%
      dplyr::mutate(compliance_rate = NA)
  }

  # General case
  if ((sum(class %in% c("calidad.mean", "calidad.size", "calidad.total")) == 1 )| (sum(class %in% 'calidad.prop') == 1 & (sum(table$stat>1)>0 | !ratio_between_0_1))) {

    if ((ratio_between_0_1) & sum(class %in% 'calidad.prop') == 1){
      warning('Oops! A ratio estimation greater than 1 was detected. The assessment will use cv.')
    }


    evaluation <- table %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                    eval_compliance_rate = dplyr::if_else(is.na(.data$compliance_rate),'insufficient rate',
                                                            dplyr::if_else(.data$compliance_rate>=1, 'sufficient rate', 'insufficient rate')),

                    eval_df = dplyr::if_else(.data$df >= params$df, "sufficient df", "insufficient df"),
                    eval_cv = dplyr::case_when(
                      .data$cv <= params$cv_lower_econ  & .data$cv > 0                   ~ paste("cv <=", params$cv_lower_econ) ,
                      .data$cv > params$cv_lower_econ & .data$cv <= params$cv_upper_econ ~ paste("cv between", params$cv_lower_econ, "and", params$cv_upper_econ),
                      .data$cv > params$cv_upper_econ                                    ~ paste("cv >", params$cv_upper_econ)
                      )
                    ) %>%

      dplyr::mutate(label = dplyr::case_when(
        eval_n == "insufficient sample size" & domain_info == FALSE ~ "non-reliable",
        eval_n == "insufficient sample size" & domain_info == TRUE & eval_compliance_rate == 'insufficient rate' ~ "non-reliable",
        eval_n == "sufficient sample size" & eval_df == "sufficient df" & eval_cv == paste("cv <=", params$cv_lower_econ)~ "reliable",
        eval_n == "insufficient sample size" & domain_info == TRUE & eval_compliance_rate == 'sufficient rate' &
          eval_df == "sufficient df" & eval_cv == paste("cv <=", params$cv_lower_econ)~ "reliable",
        eval_n == "sufficient sample size" & eval_df == "sufficient df" & eval_cv ==  paste("cv between", params$cv_lower_econ, "and", params$cv_upper_econ) ~ "weakly reliable",
        eval_n == "insufficient sample size" & domain_info == TRUE & eval_compliance_rate == 'sufficient rate' &
          eval_df == "sufficient df" & eval_cv ==  paste("cv between", params$cv_lower_econ, "and", params$cv_upper_econ) ~ "weakly reliable",

        TRUE ~ "non-reliable"
      ))



    # Proportion case
  } else {

    evaluation <- table %>%
      dplyr::mutate(eval_n = dplyr::if_else(.data$n >= params$n, "sufficient sample size", "insufficient sample size"),
                    eval_compliance_rate = dplyr::if_else(is.na(.data$compliance_rate),'insufficient rate',
                                                            dplyr::if_else(.data$compliance_rate>=1, 'sufficient rate', 'insufficient rate')),
                    eval_df = dplyr::if_else(.data$df >= params$df, "sufficient df", "insufficient df"),
                    eval_se = dplyr::case_when(
                      .data$stat< 0.5 & .data$se<=quadratic(.data$stat) ~ "admissible SE",
                      .data$stat>=0.5 & .data$se<=quadratic(.data$stat) ~ "admissible SE",
                      TRUE ~ "high SE"
                    )
      ) %>%

      dplyr::mutate(label = dplyr::case_when(
        eval_n == "insufficient sample size" & domain_info == FALSE ~ "non-reliable",
        eval_n == "insufficient sample size"  & domain_info == TRUE & eval_compliance_rate == 'insufficient rate' ~ "non-reliable",
        eval_n == "sufficient sample size" & eval_df == "sufficient df" & eval_se == "admissible SE" ~ "reliable",
        eval_n == "insufficient sample size"  & domain_info == TRUE & eval_compliance_rate == 'sufficient rate'&
          eval_df == "sufficient df" & eval_se == "admissible SE" ~ "reliable",
        eval_n == "sufficient sample size" & eval_df == "sufficient df" & eval_se == "high SE" ~ "weakly reliable",
        eval_n == "insufficient sample size"  & domain_info == TRUE & eval_compliance_rate == 'sufficient rate'&
          eval_df == "sufficient df" & eval_se == "high SE" ~ "weakly reliable",

        TRUE ~ "weakly reliable"
      ))

  }

  evaluation <- add_class(evaluation, "economicas.eval")


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

