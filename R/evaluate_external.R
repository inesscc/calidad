


#---------------------------------------------------------------------
#' assess the quality of mean estimations
#'
#' \code{assess} assess the quality of mean estimation using the
#' methodology created by INE Chile, which considers sample size, degrees of freedom and
#' coeficient of variation.
#'
#' @param table \code{dataframe} created by \code{crear_insumos_media}
#' @param publish \code{boolean} indicating if the evaluation of the complete table
#' must be added. If it is TRUE, the function adds a new column to the \code{dataframe}
#' @param scheme string variable, default scheme is "chile" which refers to the evaluation protocol proposed by INE Chile. the alternative is "eclac" to use the eclac protocol
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
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' assess(create_mean("gastot_hd", domains = "zona+sexo", design = dc))
#' @export


assess <- function(table, publish = FALSE, scheme = c("chile", "eclac") , ...) {

  # check if the scheme has the correct input
  scheme <- match.arg(scheme)

  # Defaults params for cepal and INE Chile
  user_params <- list(...)
  default_params_ine = list(df = 9, n = 60, cv_lower_ine = 0.15, cv_upper_ine = 0.3 )
  default_params_cepal = list(df = 9, n = 100, cv_cepal = 0.2, ess = 140, unweighted = 50, log_cv = 0.175)



  # Default scheme is INE Chile
  if (scheme == "chile") {

    params <- combine_params(default_params_ine, user_params)
    evaluation <- assess_ine(table, params, class(table))


    # General criteria for the publication of the INE table
    if (publish == TRUE) {
      evaluation <- publish_table(evaluation)
    }

    #  CEPAL Standard
  } else if (scheme == "eclac") {

    # Check that all the inputs are available
    check_cepal_inputs(table, "ess")
    check_cepal_inputs(table, "unweighted")
    if (sum(class(table) %in% c( "calidad.prop")) == 1 ) {
      check_cepal_inputs(table, "log_cv")
    }



    # Combine defaults params with user inputs
    params <- combine_params(default_params_cepal, user_params)

    # Apply standard
    evaluation <- assess_cepal(table, params, class(table))

  }

  return(evaluation)
}

