
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))



#' Create the inputs to evaluate the quality of mean estimations
#'
#' \code{create_mean} generates a \code{dataframe} with the following elements: mean,
#' degrees of freedom, sample size and coefficient of variation. The function allows
#' grouping in several domains.
#'
#' @param var numeric variable within the  \code{dataframe}.
#' @param domains domains to be estimated separated by the + character.
#' @param subpop integer dummy variable to filter the dataframe
#' @param design complex design created by \code{survey} package
#' @param ci \code{boolean} indicating if the confidence intervals must be calculated
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @param standard_eval \code{boolean} Indicating if the function is wrapped inside a function, if \code{TRUE} avoid lazy eval errors
#' @param ess \code{boolean} Effective sample size
#' @param rm.na \code{boolean} Remove NA if it is required
#' @param deff \code{boolean} Design effect
#' @param rel_error \code{boolean} Relative error
#' @param unweighted \code{boolean} Add non weighted count if it is required
#' @param eclac_input \code{boolean} return eclac inputs
#' @import survey
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'
#' @examples
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_mean("gastot_hd", "zona+sexo",  design = dc)
#' @export

create_mean = function(var, domains = NULL, subpop = NULL, design, ci = F, ess = F, ajuste_ene = F, standard_eval = F,
                       rm.na = F, deff = F, rel_error = F, unweighted = F, eclac_input = F) {

  # Turn on eclac indicators if the user wants it
  eclac_inputs <-  eclac_standard(eclac_input)
  ess = eclac_inputs$ess
  unweighted = eclac_inputs$unweighted
  deff = eclac_inputs$deff


  # Homologar nombres de variables  del diseño
  design <- standardize_design_variables(design)

  # Sacar los NA si el usuario lo requiere
  if (rm.na == T) {
    design <- design[!is.na(design$variables[[var]])]
  }

  # Chequear que la variable objetivo y la variable subpop cumplan con ciertas condiciones
  check_input_var(var, design)
  check_subpop_var(subpop, design)

  # Lanzar warning del error estándar cuando no se usa el diseño
  se_message(design)

  # Filtrar diseño, si el usuario agrega el parámetro subpop
  design <- filter_design(design, subpop)

  #Convertir los inputs en formulas para adecuarlos a survey
  var_form <- convert_to_formula(var)

  # Convertir en formula para survey
  domains_form <- convert_to_formula(domains)

  #Generar la tabla con los calculos
  tabla <- calcular_tabla(var_form, domains_form, design, fun = survey::svymean)

  # Crear listado de variables que se usan para el cálculo
  agrupacion <- create_groupby_vars(domains)

  #Calcular el tamanio muestral de cada grupo
  n <- get_sample_size(design$variables, agrupacion)

  #Calcular los grados de libertad de todos los cruces
  gl <- get_df(design, agrupacion)

  #Extrear el coeficiente de variacion
  cv <- get_cv(tabla, design, agrupacion)

  #Unir toda la informacion en una tabla final
  final <- create_output(tabla, agrupacion,  gl, n, cv)

  # Ordenar las columnas y estandarizar los nombres de las variables
  final <- standardize_columns(final, var, denom = NULL )

  # Se calculan los intervalos de confianza solo si el usuario lo requiere
  if (ci == T) {
    final <- get_ci(final,  ajuste_ene = ajuste_ene)
  }

  # add relative error, if the user uses this parameter
  if (rel_error == T) {
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
  final <- add_class(final, "calidad.mean")
  return(final)
}



#-------------------------------------------------------------------------


#' Create the inputs to evaluate the quality of the sum of continuous variables
#'
#' \code{create_total} generates a \code{dataframe} with the following elements: sum,
#' degrees of freedom, sample size and coefficient of variation. The function allows
#' grouping in several domains.
#'
#' @param var numeric variable within the  \code{dataframe}.
#' @param domains domains to be estimated separated by the + character.
#' @param subpop integer dummy variable to filter the dataframe
#' @param design complex design created by \code{survey} package
#' @param ci \code{boolean} indicating if the confidence intervals must be calculated
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @param standard_eval \code{boolean} Indicating if the function is wrapped inside a function, if \code{TRUE} avoid lazy eval errors
#'
#' @param deff \code{boolean} Design effect
#' @param ess \code{boolean} Effective sample size
#' @param rm.na \code{boolean} Remove NA if it is required
#' @param rel_error \code{boolean} Relative error
#' @param eclac_input \code{boolean} return eclac inputs
#' @param unweighted \code{boolean} Add non weighted count if it is required
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#' @import haven
#' @examples
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_total("gastot_hd", "zona+sexo", subpop = "ocupado", design = dc)
#' @export

create_total <- function(var, domains = NULL, subpop = NULL, design, ci = F, ess = F, ajuste_ene = F, standard_eval = F, rm.na = F,
                         deff = F, rel_error = F, unweighted = F, eclac_input = F) {

  # Turn on eclac indicators if the user wants it
  eclac_inputs <-  eclac_standard(eclac_input)
  ess = eclac_inputs$ess
  unweighted = eclac_inputs$unweighted
  deff = eclac_inputs$deff


  # Homologar nombres de variables  del diseño
  design <- standardize_design_variables(design)

  # Sacar los NA si el usuario lo requiere
  if (rm.na == T) {
    design <- design[!is.na(design$variables[[var]])]
  }

  # Chequear que la variable objetivo y la variable subpop cumplan con ciertas condiciones
  check_input_var(var, design, estimation = "total")
  check_subpop_var(subpop, design)

  # Lanzar warning del error estándar cuando no se usa el diseño
  se_message(design)

  # Filtrar diseño, si el usuario agrega el parámetro subpop
  design <- filter_design(design, subpop)


  #Convertir los inputs en formulas para adecuarlos a survey
  var_form <- convert_to_formula(var)

  # Convertir en formula para survey
  domains_form <- convert_to_formula(domains)

  #Generar la tabla con los calculos
  tabla <- calcular_tabla(var_form, domains_form, design, fun = survey::svytotal)

  # Crear listado de variables que se usan para el cálculo
  agrupacion <- create_groupby_vars(domains)

  #Calcular el tamanio muestral de cada grupo
  n <- get_sample_size(design$variables, agrupacion)


  #Calcular los grados de libertad de todos los cruces
  gl <- get_df(design, agrupacion)

  #Extrear el coeficiente de variacion
  cv <- get_cv(tabla, design, agrupacion)

  #Unir toda la informacion en una tabla final
  final <- create_output(tabla, agrupacion,  gl, n, cv)

  # Ordenar las columnas y estandarizar los nombres de las variables
  final <- standardize_columns(final, var, denom = NULL )

  # Se calculan los intervalos de confianza solo si el usuario lo requiere
  if (ci == T) {
    final <- get_ci(final,  ajuste_ene = ajuste_ene)
  }

  # add relative error, if the user uses this parameter
  if (rel_error == T) {
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
#' degrees of freedom, sample size and coefficient of variation. The function allows
#' grouping in several domains.
#' @param var numeric variable within the  \code{dataframe}. When the domain parameter is not used,
#' it is possible to include more than one variable using the + separator. When a value is introduced
#' in the domain parameter, the estimation variable must be a dummy variable.
#' @param domains domains to be estimated separated by the + character.
#' @param subpop integer dummy variable to filter the dataframe
#' @param design complex design created by \code{survey} package
#' @param ci \code{boolean} indicating if the confidence intervals must be calculated
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @param standard_eval \code{boolean} Indicating if the function is wrapped inside a function, if \code{TRUE} avoid lazy eval errors
#'
#' @param deff \code{boolean} Design effect
#' @param ess \code{boolean} Effective sample size
#' @param rm.na \code{boolean} Remove NA if it is required
#' @param rel_error \code{boolean} Relative error
#' @param eclac_input \code{boolean} return eclac inputs
#' @param unweighted \code{boolean} Add non weighted count if it is required
#' @param df_type \code{string} Use degrees of freedom calculation approach from INE Chile or CEPAL, by default "ine".
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#' @import tidyr
#' @examples
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_size("ocupado", "zona+sexo", design = dc)
#' @export

create_size <- function(var, domains = NULL, subpop = NULL, design, ci = F, ess = F, ajuste_ene = F, standard_eval = F, rm.na = F,
                         deff = F, rel_error = F,  unweighted = F, df_type = "ine", eclac_input = F) {


  # Turn on eclac indicators if the user wants it
  eclac_inputs <-  eclac_standard(eclac_input)
  ess = eclac_inputs$ess
  unweighted = eclac_inputs$unweighted
  deff = eclac_inputs$deff


  # Homologar nombres de variables  del diseño
  design <- standardize_design_variables(design)

  # Sacar los NA si el usuario lo requiere
  if (rm.na == T) {
    design <- design[!is.na(design$variables[[var]])]
  }

  # Chequear que la variable objetivo y la variable subpop cumplan con ciertas condiciones
  check_input_var(var, design, estimation = "size")
  check_subpop_var(subpop, design)

  # Lanzar warning del error estándar cuando no se usa el diseño
  se_message(design)

  # Filtrar diseño, si el usuario agrega el parámetro subpop
  design <- filter_design(design, subpop)

  #Convertir los inputs en formulas para adecuarlos a survey
  var_form <- convert_to_formula(var)

  # Convertir en formula para survey
  domains_form <- convert_to_formula(domains)

  # Crear listado de variables que se usan para el cálculo
  agrupacion <- create_groupby_vars(domains)

  # Add estimation variable for the case ine-size
  if (df_type == "ine") {
    agrupacion <- c(agrupacion, var)
    domains_form <- convert_to_formula(paste0(domains, "+", var))
  }

  #Generar la tabla con los calculos
  tabla <- calcular_tabla(var_form, domains_form, design, fun = survey::svytotal)

    #Calcular el tamanio muestral de cada grupo
  n <- get_sample_size(design$variables, agrupacion, df_type)

  #Calcular los grados de libertad de todos los cruces
  gl <- get_df(design,agrupacion,df_type)

  #Extrear el coeficiente de variacion
  cv <- get_cv(tabla, design, agrupacion)

  if(df_type == "ine" & is.null(domains)){
    cv <- cv[2]
  }

  #Unir toda la informacion en una tabla final
  final <- create_output(tabla, agrupacion,  gl, n, cv)

  # Ordenar las columnas y estandarizar los nombres de las variables
  final <- standardize_columns(final, var, denom = NULL)

  # Se calculan los intervalos de confianza solo si el usuario lo requiere
  if (ci == T) {
    final <- get_ci(final,  ajuste_ene = ajuste_ene)
  }

  # add relative error, if the user uses this parameter
  if (rel_error == T) {
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

  final <- add_class(final, "calidad.size")


  return(final)

}




#-----------------------------------------------------------------------
#' Create the inputs to evaluate the quality of proportion estimations
#'
#' \code{create_prop} generates a \code{dataframe} with the following elements: sum,
#' degrees of freedom, sample size, standard error and coefficient of variation. The function allows
#' grouping in several domains.
#'
#' @param var numeric variable within the \code{dataframe}, is the numerator of the ratio to be calculated.
#' @param denominador numeric variable within the \code{dataframe}, is the denominator of the ratio to be calculated. If the \code{var} parameter is dummy, it can be NULL
#' @param domains domains to be estimated separated by the + character.
#' @param design complex design created by \code{survey} package
#' @param subpop integer dummy variable to filter the dataframe
#' @param ci \code{boolean} indicating if the confidence intervals must be calculated
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @param standard_eval \code{boolean} Indicating if the function is wrapped inside a function, if \code{TRUE} avoid lazy eval errors
#' @param deff \code{boolean} Design effect
#' @param ess \code{boolean} Effective sample size
#' @param rel_error \code{boolean} Relative error
#' @param eclac_input \code{boolean} return eclac inputs
#' @param log_cv \code{boolean} logarithmic coefficient of variation
#' @param unweighted \code{boolean} Add non weighted count if it is required
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'
#' @examples
#' library(survey)
#' library(dplyr)
#' epf <- mutate(epf_personas, gasto_zona1 = if_else(zona == 1, gastot_hd, 0))
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf, weights = ~fe)
#' options(survey.lonely.psu = "certainty")
#'
#' create_prop(var = "gasto_zona1", denominador = "gastot_hd", design =  dc)
#'
#' enusc <- filter(enusc, Kish == 1)
#'
#' dc <- svydesign(ids = ~Conglomerado, strata = ~VarStrat, data = enusc, weights = ~Fact_Pers)
#' options(survey.lonely.psu = "certainty")
#' create_prop(var = "VP_DC", denominador = "hom_insg_taxi", design = dc)
#'
#' @export
#'

create_prop = function(var, denominador = NULL, domains = NULL, subpop = NULL, design, ci = F, deff = F, ess = F, ajuste_ene = F,
                       rel_error = F, log_cv = F, unweighted = F, standard_eval = F, eclac_input = F){

  # Turn on eclac indicators if the user wants it
  eclac_inputs <-  eclac_standard(eclac_input, proportion = T)
  ess = eclac_inputs$ess
  unweighted = eclac_inputs$unweighted
  deff = eclac_inputs$deff
  log_cv = eclac_inputs$log_cv


  if(!is.null(denominador)){
    final = create_ratio_internal(var,denominador, domains, subpop, design, ci, deff, ess,  ajuste_ene, rel_error )
  }

  if(is.null(denominador)) {
    final = create_prop_internal(var,  domains, subpop, design, ci, deff, ess,  ajuste_ene, rel_error, log_cv, unweighted)
  }

  # Add a class to the object
  final <- add_class(final, "calidad.prop")

  return(final)
}


