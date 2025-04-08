#---------------------------------------------------------------------
#' Assess the quality of mean estimations
#'
#' \code{assess} evaluates the quality of mean estimations using the
#' methodology created by INE Chile, which considers sample size, degrees of freedom, and
#' coefficient of variation.
#'
#' @param table \code{dataframe} created by \code{crear_insumos_media}.
#' @param publish \code{boolean} indicating if the evaluation of the complete table
#' must be added. If \code{TRUE}, the function adds a new column to the \code{dataframe}.
#' @param scheme \code{character} variable indicating the evaluation protocol to use. Options are "chile", "eclac_2020", "eclac_2023", "chile_economicas".
#' @param domain_info Logical. If \code{TRUE}, indicates that the study domain information is available and will be used for assessment.
#' This affects how the evaluation is conducted, leveraging specific domain-level data to refine the assessment results.
#' When \code{FALSE}, domain-specific adjustments are omitted, and a generalized assessment is performed.
#' @param low_df_justified Logical. If \code{TRUE} the low degrees of freedom are justified and will be used for assessment. By default \code{FALSE}.
#' @param table_n_obj Default \code{NULL}. Dataframe with the target sample size column \code{n_obj} and columns with the domains to evaluate. Its important check the domain columns type with table.
#' @param ratio_between_0_1 \code{boolean}. If \code{TRUE}, indicates that the estimator is a ratio between 0 and 1.
#' @param ... additional parameters for the evaluation. The complete list of parameters is:

#' 1. General Parameters
#' \itemize{
#'   \item \code{df} degrees of freedom. Default: 9.
#'   \item \code{n} sample size. Default for chile scheme: 60. Default for CEPAL schemes: 100. Default for chile economic standard scheme: 30.
#' }
#'
#' 2. chile Parameters
#' \itemize{
#'   \item \code{cv_lower_ine} lower limit for CV. Default: 0.15.
#'   \item \code{cv_upper_ine} upper limit for CV. Default: 0.3.
#' }
#'
#' 3. CEPAL 2020 Parameters
#' \itemize{
#'   \item \code{cv_cepal} limit for CV. Default: 0.2.
#'   \item \code{ess} effective sample size. Default: 140.
#'   \item \code{unweighted} unweighted count. Default: 50.
#'   \item \code{log_cv} logarithmic coefficient of variation. Default: 0.175.
#' }
#'
#' 4. CEPAL 2023 Parameters
#' \itemize{
#'   \item \code{cv_lower_cepal} lower limit for CV. Default: 0.2.
#'   \item \code{cv_upper_cepal} upper limit for CV. Default: 0.3.
#'   \item \code{ess} effective sample size. Default: 60.
#'   \item \code{cvlog_max} maximum logarithmic coefficient of variation. Default: 0.175.
#'   \item \code{CCNP_b} unweighted count before adjustment. Default: 50.
#'   \item \code{CCNP_a} unweighted count after adjustment. Default: 30.
#' }
#'
#' 5. Chile Economic Survey Standard Parameters
#' \itemize{
#'   \item \code{cv_lower_econ} lower limit for CV. Default: 0.2.
#'   \item \code{cv_upper_econ} upper limit for CV. Default: 0.3.
#' }
#'
#' @return \code{dataframe} with all the columns included in the input table, plus a new column
#' containing a label indicating the evaluation of each estimation: reliable, bit reliable, or unreliable.
#'
#' @examples
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' assess(create_mean("gastot_hd", domains = "zona+sexo", design = dc))
#' @export

assess <- function(table, publish = FALSE, scheme = c("chile", "eclac_2020", "eclac_2023", "chile_economicas"), domain_info = FALSE, low_df_justified = FALSE, table_n_obj = NULL, ratio_between_0_1 = TRUE, ...) {

  # check if the scheme has the correct input
  scheme <- match.arg(scheme)

  # Defaults params for cepal and INE Chile
  user_params <- list(...)

  default_params_ine = list(df = 9, n = 60, cv_lower_ine = 0.15, cv_upper_ine = 0.3 )
  default_params_cepal2020 = list(df = 9, n = 100, cv_cepal = 0.2, ess = 140, unweighted = 50, log_cv = 0.175)
  default_params_cepal2023 <- list(df = 9, n = 100, cv_lower_cepal = 0.2, cv_upper_cepal = 0.3, ess = 60, cvlog_max = 0.175, CCNP_b = 50, CCNP_a = 30)
  default_params_economicas <- list(df = 9, n = 30, cv_lower_econ = 0.2, cv_upper_econ = 0.3)



  # Default scheme is INE Chile
  if (scheme == "chile") {

    params <- combine_params(default_params_ine, user_params)
    evaluation <- assess_ine(table, params, class(table), ratio_between_0_1 = ratio_between_0_1)


    # General criteria for the publication of the chile table
    if (publish == TRUE) {
      evaluation <- publish_table(evaluation)
    }

    #  CEPAL 2020
  } else if (scheme == "eclac_2020") {

    # Check that all the inputs are available
    check_cepal_inputs(table, "ess")
    check_cepal_inputs(table, "unweighted")
    if (sum(class(table) %in% c( "calidad.prop")) == 1 ) {
      check_cepal_inputs(table, "log_cv")
    }

    # Combine defaults params with user inputs
    params <- combine_params(default_params_cepal2020, user_params)

    # Apply standard
    evaluation <- assess_cepal2020(table, params, class(table))


    # CEPAL 2023 Standard
  } else if (scheme == "eclac_2023") {
    # Check that all the inputs are available for CEPAL 2023
    check_cepal_inputs(table, "ess")
    check_cepal_inputs(table, "unweighted")
    check_cepal_inputs(table, "deff")
    if (sum(class(table) %in% c( "calidad.prop")) == 1 & ratio_between_0_1 & sum(table$stat>1)==0) {
      check_cepal_inputs(table, "log_cv")
    }

    # Combine defaults params with user inputs for CEPAL 2023
    params <- combine_params(default_params_cepal2023, user_params)

     # Apply CEPAL 2023 standard, passing the domain_info argument
    evaluation <- assess_cepal2023(table, params, class(table),
                                   domain_info = domain_info,            # Assuming default domain_info is FALSE
                                   low_df_justified = low_df_justified,
                                   ratio_between_0_1 = ratio_between_0_1)


    # Economics Standard
  } else if (scheme == "chile_economicas"){

    ## check n_obj
    merge <- check_n_obj_var(table_n_obj, table)

    ## try to join tables
    if(merge){
      table <- merge_columns(table, table_n_obj)
    }

    # Combine defaults params with user inputs for economicas
    params <- combine_params(default_params_economicas, user_params)

    # Apply economicas standard, passing the domain_info argument
    evaluation <- assess_economicas(table, params, class(table),
                                    domain_info = domain_info,
                                    ratio_between_0_1 = ratio_between_0_1)

    if (publish == TRUE) {
      evaluation <- publish_table(evaluation)
    }

  }

  return(evaluation)
}



