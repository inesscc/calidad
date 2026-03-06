
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


#' Create the inputs to evaluate the quality of mean estimates
#'
#' \code{create_mean()} generates a \code{data.frame} containing the estimated mean,
#' degrees of freedom, sample size, and quality measures such as the standard
#' error and coefficient of variation. Estimates can be produced for multiple
#' domains defined by one or more grouping variables.
#'
#' @param var Name of numeric variable to estimate within the \code{data.frame}.
#' @param domains Domains variables to be estimated separated by the `+` character.
#' @param subpop Integer dummy variable used to filter a subpopulation before estimation. If `NULL`, no filtering is applied.
#' @param design A complex survey design object created with \code{survey::svydesign()}.
#' @param ci \code{boolean}. If `TRUE`, compute confidence intervals (see Details).
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used. TODO: PREGUNTAR SOBRE ESTO
#' @param standard_eval \code{boolean} indicating if the function is wrapped inside another function, if \code{TRUE} avoid lazy eval errors. TODO: PREGUNTAR SOBRE ESTO
#' @param ess \code{boolean}. If `TRUE`, compute an effective sample size.
#' @param rm.na \code{boolean}. If `TRUE`, remove observations with missing values in `var` before estimation.
#' @param deff \code{boolean}. If `TRUE`, compute a design effect
#' @param rel_error \code{boolean}. If `TRUE`, compute relative error measures.
#' @param unweighted \code{boolean}. If `TRUE`, add unweighted counts.
#' @param eclac_input \code{boolean}. If `TRUE`, return inputs formatted for ECLAC workflows.
#' @import survey
#'
#' @details
#' For each domain, the mean is estimated using the Horvitz-Thompson estimator:
#' \deqn{\hat{\mu} = \frac{\sum_{i \in s} w_i y_i}{\sum_{i \in s} w_i},}
#' where \eqn{y_i} is the study variable, \eqn{w_i = 1/\pi_i} is the survey weight and \eqn{\pi_i} is the first-order inclusion probability.
#'
#' The standard error is computed using a Taylor-linearized variance estimator.
#' For means, the linearized variable is \eqn{u_i = \frac{w_i(y_i - \hat{\mu})}{\sum_{i \in s} w_i}}. The variance is then calculated from the sampling units defined in the survey design object.
#' When explicit clustering is present, the linearized values are first aggregated within PSU and stratum as
#' \deqn{E_{hi} = \sum_{k \in PSU_{hi}} u_{hk},}
#' and stratum-specific contributions are combined to obtain the total variance.
#' If no clustering variable is defined, the observational units are treated as sampling units for variance estimation. When finite population sizes are available, a finite population correction is applied.
#'
#' Strata with a single sampled unit are handled according to
#' `getOption("survey.lonely.psu")` (e.g., `"adjust"`,`"remove"`, `"certainty"` or `"average"`).
#'
#' Degrees of freedom are computed as the number of sampled PSUs minus the number of strata within each domain.
#' When `ci = TRUE`, confidence intervals are based on a critical value from the t distribution with these degrees of freedom. If `ajuste_ene = TRUE`, a
#' fixed critical value equal to 2 is used instead.

#' @return A \code{data.frame} containing the estimated mean and quality metrics such as the standard error (`se`) and coefficient of variation (`cv`).
#'
#' @examples
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_mean("gastot_hd", "zona+sexo", design = dc)
#' @export

create_mean = function(var, domains = NULL, subpop = NULL, design, ci = FALSE, ess = FALSE, ajuste_ene = FALSE, standard_eval = FALSE,
                       rm.na = FALSE, deff = FALSE, rel_error = FALSE, unweighted = FALSE, eclac_input = FALSE) {


  # get design variables
  design_vars <- get_design_vars(design)

  # Create list of variables included in domains
  agrupacion <- create_groupby_vars(domains)

  # Select relevant columns
  design <- design[, c(agrupacion, var, subpop, design_vars)]

  # Turn on eclac indicators if the user wants it
  eclac_inputs <- eclac_standard(eclac_input)
  ess <- eclac_inputs$ess
  unweighted <- eclac_inputs$unweighted
  deff <- eclac_inputs$deff

  # Standardize design variable names
  design <- standardize_design_variables(design)

  # Convert everything to lowercase to avoid problems in next steps
  names(design$variables) <- tolower(names(design$variables))
  lower_params <- purrr::map(list("var" = var, "subpop" = subpop, "domains" = domains), tolower_strings)
  var <- lower_params$var
  subpop <- lower_params$subpop
  domains <- lower_params$domains

  # Remove NA values
  if (rm.na == TRUE) {
    design <- design[!is.na(design$variables[[var]])]
  }

  # Checking for the objective variable and subpop
  check_input_var(var, design)
  check_subpop_var(subpop, design)

  # warning if the standard error is not obtained from the complex design
  se_message(design)

  # Filter if the user adds subpop parameter
  design <- filter_design(design, subpop)

  # Convert inputs to formula in order to get an easier manipulation with survey
  var_form <- convert_to_formula(var)
  domains_form <- convert_to_formula(domains)

  # Get main results using survey
  tabla <- get_table(var_form, domains_form, design, fun = get_mean)

  # Create list of variables used during the calculation
  agrupacion <- create_groupby_vars(domains)

  # get sample size for each group
  n <- get_sample_size(design$variables, agrupacion)

  # Get degrees of freedom
  gl <- get_df(design, agrupacion)

  # Get coefficient of variation
  #cv <- get_cv(tabla, design, agrupacion)

  # Combine all the information in one single table
  final <- create_output(tabla, agrupacion, gl, n)#, cv)

  # Order columns and standardize variable names
  final <- standardize_columns(final, var, denom = NULL)

  # Get confidence intervals if the user includes this parameter
  if (ci == TRUE) {
    final <- get_ci(final, ajuste_ene = ajuste_ene, ci_logit = FALSE)
  }

  # Add relative error, if the user uses this parameter
  if (rel_error == TRUE) {
    final <- final %>%
      dplyr::mutate(relative_error = stats::qt(c(.975), df = .data$df) * cv)
  }

  # Add the ess if the user uses this parameter
  final <- get_ess(ess)

  # Add non weighted count if it is required
  if (unweighted) {
    final <- final %>%
      dplyr::mutate(unweighted = n)
  }

  # Add a class to the object
  final <- add_class(final, "calidad.mean")
  return(final)
}




#-------------------------------------------------------------------------

#' Create the inputs to evaluate the quality of the sum of continuous variables
#'
#' \code{create_total} generates a \code{data.frame} containing the estimated total, degrees of freedom, sample size, and quality measures such as the
#' standard error and coefficient of variation. Estimates can be produced for multiple domains defined by one or more grouping variables.
#'
#' @param var Name of numeric variable to estimate within the \code{data.frame}.
#' @param domains Domains variables to be estimated separated by the `+` character.
#' @param subpop Integer dummy variable used to filter a subpopulation before estimation. If `NULL`, no filtering is applied.
#' @param design A complex survey design object created with \code{survey::svydesign()}.
#' @param ci \code{boolean}. If `TRUE`, compute confidence intervals (see Details).
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used. TODO: PREGUNTAR SOBRE ESTO
#' @param standard_eval \code{boolean} indicating if the function is wrapped inside another function, if \code{TRUE} avoid lazy eval errors. TODO: PREGUNTAR SOBRE ESTO
#' @param ess \code{boolean}. If `TRUE`, compute an effective sample size.
#' @param rm.na \code{boolean}. If `TRUE`, remove observations with missing values in `var` before estimation.
#' @param deff \code{boolean}. If `TRUE`, compute a design effect
#' @param rel_error \code{boolean}. If `TRUE`, compute relative error measures.
#' @param unweighted \code{boolean}. If `TRUE`, add unweighted counts.
#' @param eclac_input \code{boolean}. If `TRUE`, return inputs formatted for ECLAC workflows.
#' @import survey
#'
#' @details
#' For each domain, the population total is estimated using the Horvitz-Thompson estimator
#' \deqn{\hat{T} = \sum_{i \in s} \frac{y_i}{\pi_i} = \sum_{i \in s} w_i y_i,}
#' where \eqn{y_i} is the study variable, \eqn{\pi_i} is the first-order inclusion probability, and \eqn{w_i = 1/\pi_i} is the survey weight.
#'
#' The standard error is computed using a Taylor-linearized variance estimator.
#' For totals, the linearized variable is \eqn{u_i = y_i wi_i}. The variance is then calculated from the sampling units defined in the survey design object.
#' When explicit clustering is present, the linearized values are first aggregated within PSU and stratum as
#' \deqn{E_{hi} = \sum_{k \in PSU_{hi}} u_{hk},}
#' and stratum-specific contributions are combined to obtain the total variance.
#' If no clustering variable is defined, the observational units are treated as sampling units for variance estimation. When finite population sizes are available, a finite population correction is applied.
#'
#' Strata with a single sampled unit are handled according to
#' `getOption("survey.lonely.psu")` (e.g., `"adjust"`,`"remove"`, `"certainty"` or `"average"`).
#'
#' Degrees of freedom are computed as the number of sampled PSUs minus the number of strata within each domain.
#' When `ci = TRUE`, confidence intervals are based on a critical value from the t distribution with these degrees of freedom. If `ajuste_ene = TRUE`, a
#' fixed critical value equal to 2 is used instead.
#'
#'
#' @return A \code{data.frame} containing the estimated total and quality metrics such as the standard error (`se`) and coefficient of variation (`cv`).
#'
#'
#' @examples
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_total("gastot_hd", "zona+sexo", subpop = "ocupado", design = dc)
#' @export

create_total <- function(var, domains = NULL, subpop = NULL, design, ci = FALSE, ess = FALSE, ajuste_ene = FALSE, standard_eval = FALSE, rm.na = FALSE,
                         deff = FALSE, rel_error = FALSE, unweighted = FALSE, eclac_input = FALSE) {


  # get design variables
  design_vars <- get_design_vars(design)

  # Create list of variables included in domains
  agrupacion <- create_groupby_vars(domains)

  # Select relevant columns
  design <- design[, c(agrupacion, var, subpop, design_vars)]

  # Turn on eclac indicators if the user wants it
  eclac_inputs <- eclac_standard(eclac_input)
  ess <- eclac_inputs$ess
  unweighted <- eclac_inputs$unweighted
  deff <- eclac_inputs$deff

  # Standardize design variable names
  design <- standardize_design_variables(design)

  # Convert everything to lowercase to avoid problems in next steps
  names(design$variables) <- tolower(names(design$variables))
  lower_params <- purrr::map(list("var" = var, "subpop" = subpop, "domains" = domains), tolower_strings)
  var <- lower_params$var
  subpop <- lower_params$subpop
  domains <- lower_params$domains

  # Remove NA values
  if (rm.na == TRUE) {
    design <- design[!is.na(design$variables[[var]])]
  }

  # Checking for the objective variable and subpop
  check_input_var(var, design, estimation = "total")
  check_subpop_var(subpop, design)

  # warning if the standard error is not obtained from the complex design
  se_message(design)

  # Filter if the user adds subpop parameter
  design <- filter_design(design, subpop)

  # Convert inputs to formula in order to get an easier manipulation with survey
  var_form <- convert_to_formula(var)
  domains_form <- convert_to_formula(domains)

  # Get main results using survey
  tabla <- get_table(var_form, domains_form, design, fun = get_total)

  # Create list of variables used during the calculation
  agrupacion <- create_groupby_vars(domains)

  # get sample size for each group
  n <- get_sample_size(design$variables, agrupacion)

  # Get degrees of freedom
  gl <- get_df(design, agrupacion)

  # Get coefficient of variation
  #cv <- get_cv(tabla, design, agrupacion)

  # Combine all the information in one single table
  final <- create_output(tabla, agrupacion, gl, n)#, cv)

  # Order columns and standardize variable names
  final <- standardize_columns(final, var, denom = NULL)

  # Get confidence intervals if the user includes this parameter
  if (ci == TRUE) {
    final <- get_ci(final, ajuste_ene = ajuste_ene, ci_logit = FALSE)
  }

  # add relative error, if the user uses this parameter
  if (rel_error == TRUE) {
    final <- final %>%
      dplyr::mutate(relative_error = stats::qt(c(.975), df = .data$df) * cv)
  }

  # add the ess if the user uses this parameter
  final <- get_ess(ess)

  # add non weighted count if it is required
  if (unweighted) {
    final <- final %>%
      dplyr::mutate(unweighted = n)
  }

  # Add a class to the object
  final <- add_class(final, "calidad.total")

  return(final)
}


#--------------------------------------------------------------------
#' Create the inputs to evaluate the quality of total estimations
#'
#' \code{create_size} generates a \code{dataframe} with the following elements: sum,
#' degrees of freedom, sample size, and coefficient of variation. The function allows
#' grouping in several domains.
#'
#' @param var numeric variable within the \code{dataframe}. When the domain parameter is not used,
#' it is possible to include more than one variable using the + separator. When a value is introduced
#' in the domain parameter, the estimation variable must be a dummy variable.
#' @param domains Domains variables to be estimated separated by the `+` character.
#' @param subpop Integer dummy variable used to filter a subpopulation before estimation. If `NULL`, no filtering is applied.
#' @param design A complex survey design object created with \code{survey::svydesign()}.
#' @param ci \code{boolean}. If `TRUE`, compute confidence intervals (see Details).
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used. TODO: PREGUNTAR SOBRE ESTO
#' @param standard_eval \code{boolean} indicating if the function is wrapped inside another function, if \code{TRUE} avoid lazy eval errors. TODO: PREGUNTAR SOBRE ESTO
#' @param ess \code{boolean}. If `TRUE`, compute an effective sample size.
#' @param rm.na \code{boolean}. If `TRUE`, remove observations with missing values in `var` before estimation.
#' @param deff \code{boolean}. If `TRUE`, compute a design effect
#' @param rel_error \code{boolean}. If `TRUE`, compute relative error measures.
#' @param unweighted \code{boolean}. If `TRUE`, add unweighted counts.
#' @param df_type \code{character} use degrees of freedom calculation approach from INE Chile or CEPAL. Options are "chile" or "eclac".
#' @param eclac_input \code{boolean}. If `TRUE`, return inputs formatted for ECLAC workflows.
#' @import survey
#'
#' @details
#'#' For each domain, the population total is estimated using the Horvitz-Thompson estimator TODO: ARREGLAR ESTO
#' \deqn{\hat{T} = \sum_{i \in s} \frac{y_i}{\pi_i} = \sum_{i \in s} w_i y_i,}
#' where \eqn{y_i} is the study variable, \eqn{\pi_i} is the first-order inclusion probability, and \eqn{w_i = 1/\pi_i} is the survey weight.
#'
#' The standard error is computed using a Taylor-linearized variance estimator.
#' For totals, the linearized variable is \eqn{u_i = y_i wi_i}. The variance is then calculated from the sampling units defined in the survey design object.
#' When explicit clustering is present, the linearized values are first aggregated within PSU and stratum as
#' \deqn{E_{hi} = \sum_{k \in PSU_{hi}} u_{hk},}
#' and stratum-specific contributions are combined to obtain the total variance.
#' If no clustering variable is defined, the observational units are treated as sampling units for variance estimation. When finite population sizes are available, a finite population correction is applied.
#'
#' Strata with a single sampled unit are handled according to
#' `getOption("survey.lonely.psu")` (e.g., `"adjust"`,`"remove"`, `"certainty"` or `"average"`).
#'
#' Degrees of freedom are computed as the number of sampled PSUs minus the number of strata within each domain.
#' When `ci = TRUE`, confidence intervals are based on a critical value from the t distribution with these degrees of freedom. If `ajuste_ene = TRUE`, a
#' fixed critical value equal to 2 is used instead.
#'
#' @return A \code{data.frame} containing the estimated total and quality metrics such as the standard error (`se`) and coefficient of variation (`cv`).
#'
#' @examples
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_size("ocupado", "zona+sexo", design = dc)
#' @export

create_size <- function(var, domains = NULL, subpop = NULL, design, ci = FALSE, ess = FALSE, ajuste_ene = FALSE, standard_eval = FALSE, rm.na = FALSE,
                        deff = FALSE, rel_error = FALSE, unweighted = FALSE, df_type = c("chile", "eclac"), eclac_input = FALSE) {

  df_type <- match.arg(df_type)

  # get design variables
  design_vars <- get_design_vars(design)

  # Create list of variables included in domains
  agrupacion <- create_groupby_vars(domains)

  # Select relevant columns
  design <- design[, c(agrupacion, var, subpop, design_vars)]

  # Turn on eclac indicators if the user wants it
  eclac_inputs <- eclac_standard(eclac_input)
  ess <- eclac_inputs$ess
  unweighted <- eclac_inputs$unweighted
  deff <- eclac_inputs$deff

  # Standardize design variable names
  design <- standardize_design_variables(design)

  # Convert everything to lowercase to avoid problems
  names(design$variables) <- tolower(names(design$variables))
  lower_params <- purrr::map(list("var" = var, "subpop" = subpop, "domains" = domains), tolower_strings)
  var <- lower_params$var
  subpop <- lower_params$subpop
  domains <- lower_params$domains

  # Remove NA values
  if (rm.na == TRUE) {
    design <- design[!is.na(design$variables[[var]])]
  }

  # Checking for the objective variable and subpop
  check_input_var(var, design, estimation = "size")
  check_subpop_var(subpop, design)

  # warning if the standard error is not obtained from the complex design
  se_message(design)

  # Filter if the user adds subpop parameter
  design <- filter_design(design, subpop)

  # Convert inputs to formulas to get an easier manipulation with survey
  var_form <- convert_to_formula(var)
  domains_form <- convert_to_formula(domains)
  agrupacion <- create_groupby_vars(domains)

  # Add estimation variable for the case chile-size
  if (df_type == "chile") {
    agrupacion <- c(agrupacion, var)
    #domains_form <- convert_to_formula(paste0(domains, "+", var))
  }

  # Get main results using survey
  tabla <- get_table(var_form, domains_form, design, fun = get_total, type_est = "size")

  # get sample size for each group
  n <- get_sample_size(design$variables, agrupacion, df_type)

  # Get degrees of freedom
  gl <- get_df(design, agrupacion, df_type)

  # Get coefficient of variation
  #cv <- get_cv(tabla, design, agrupacion, type_est = "size")

  # Combine all the information in one single table
  final <- create_output(tabla, agrupacion, gl = gl, n)#, cv)

  # Order columns and standardize variable names
  final <- standardize_columns(final, var, denom = NULL)

  # Get confidence intervals if the user includes this parameter
  if (ci == TRUE) {
    final <- get_ci(final, ajuste_ene = ajuste_ene, ci_logit = FALSE)
  }

  # add relative error, if the user uses this parameter
  if (rel_error == TRUE) {
    final <- final %>%
      dplyr::mutate(relative_error = stats::qt(c(.975), df = .data$df) * cv)
  }

  # add the ess if the user uses this parameter
  final <- get_ess(ess)

  # add non weighted count if it is required
  if (unweighted) {
    final <- get_unweighted(table = final, domains = domains, var = var, disenio = design)
  }

  final <- add_class(final, "calidad.size")

  return(final)
}



#-----------------------------------------------------------------------
#' Create the inputs to evaluate the quality of proportion estimations
#'
#' \code{create_prop} generates a \code{dataframe} with the following elements: sum,
#' degrees of freedom, sample size, standard error, and coefficient of variation. The function allows
#' grouping in several domains.
#'
#' @param var numeric variable within the \code{dataframe}, is the numerator of the ratio to be calculated.
#' @param denominator numeric variable within the \code{dataframe}, is the denominator of the ratio to be calculated. If the \code{var} parameter is dummy, it can be NULL.
#' @param domains Domains variables to be estimated separated by the `+` character.
#' @param subpop Integer dummy variable used to filter a subpopulation before estimation. If `NULL`, no filtering is applied.
#' @param design A complex survey design object created with \code{survey::svydesign()}.
#' @param ci \code{boolean}. If `TRUE`, compute confidence intervals (see Details).
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used. TODO: PREGUNTAR SOBRE ESTO
#' @param standard_eval \code{boolean} indicating if the function is wrapped inside another function, if \code{TRUE} avoid lazy eval errors. TODO: PREGUNTAR SOBRE ESTO
#' @param ess \code{boolean}. If `TRUE`, compute an effective sample size.
#' @param rm.na \code{boolean}. If `TRUE`, remove observations with missing values in `var` before estimation.
#' @param deff \code{boolean}. If `TRUE`, compute a design effect
#' @param rel_error \code{boolean}. If `TRUE`, compute relative error measures.
#' @param unweighted \code{boolean}. If `TRUE`, add unweighted counts.
#' @param eclac_input \code{boolean}. If `TRUE`, return inputs formatted for ECLAC workflows.
#' @param ci_logit \code{boolean} indicating if interval confidence is logit, only available for proportions.
#' @param scheme \code{character} variable indicating the evaluation protocol to use for CEPAL standard. Options are "eclac_2020" and "eclac_2023". The "eclac_2020" option does not support ratio estimation.
#' @param log_cv \code{boolean} logarithmic coefficient of variation.
#' @import survey
#'
#' @details
#' Strata with a single sampled unit are handled according to
#' `getOption("survey.lonely.psu")` (e.g., `"adjust"`,`"remove"`, `"certainty"` or `"average"`).
#'
#' Degrees of freedom are computed as the number of sampled PSUs minus the number of strata within each domain.
#' When `ci = TRUE`, confidence intervals are based on a critical value from the t distribution with these degrees of freedom. If `ajuste_ene = TRUE`, a
#' fixed critical value equal to 2 is used instead.
#'
#'
#' @return A \code{data.frame} containing the estimated total and quality metrics such as the standard error (`se`) and coefficient of variation (`cv`).
#'
#' @examples
#' library(survey)
#' library(dplyr)
#'
#' epf <- mutate(epf_personas, gasto_zona1 = if_else(zona == 1, gastot_hd, 0))
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf, weights = ~fe)
#' old_options <- options()
#' options(survey.lonely.psu = "certainty")
#'
#' create_prop(var = "gasto_zona1", denominator = "gastot_hd", design = dc)
#'
#' enusc <- filter(enusc, Kish == 1)
#'
#' dc <- svydesign(ids = ~Conglomerado, strata = ~VarStrat, data = enusc, weights = ~Fact_Pers)
#' options(survey.lonely.psu = "certainty")
#' create_prop(var = "VP_DC", denominator = "hom_insg_taxi", design = dc)
#' options(old_options)
#' @export
create_prop <- function(var, denominator = NULL, domains = NULL, subpop = NULL, design, ci = FALSE, deff = FALSE, ess = FALSE,
                        ajuste_ene = FALSE, rel_error = FALSE, log_cv = FALSE, unweighted = FALSE, standard_eval = FALSE,
                        eclac_input = FALSE, ci_logit = FALSE, rm.na = FALSE, scheme = c('eclac_2020', 'eclac_2023')) {


  scheme <- match.arg(scheme)

  # eclac 2020 approach is not allowed with denominator
  if (!is.null(denominator) & eclac_input== TRUE & scheme == 'eclac_2020') {
    stop("eclac approach is not allowed with denominator for scheme eclac_2020")
  }

  # Turn on eclac indicators if the user selects eclac_2020 or eclac_2023
  eclac_inputs <- eclac_standard(eclac_input, proportion = TRUE)
  ess <- eclac_inputs$ess
  unweighted <- eclac_inputs$unweighted
  deff <- eclac_inputs$deff
  log_cv <- eclac_inputs$log_cv

  if (!is.null(denominator)) {
    final <- create_ratio_internal(var, denominator, domains, subpop, design, ci, deff, ess, ajuste_ene, unweighted, rel_error, log_cv)
  }

  if (is.null(denominator)) {
    final <- create_prop_internal(var, domains, subpop, design, ci, deff, ess, ajuste_ene, rel_error, log_cv, unweighted, ci_logit = ci_logit)
  }

  # Add a class to the object
  final <- add_class(final, "calidad.prop")

  return(final)

}

