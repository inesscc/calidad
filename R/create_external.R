
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))



#' Create the inputs to evaluate the quality of mean estimations
#'
#' \code{create_mean} generates a \code{dataframe} with the following elements: mean,
#' degrees of freedom, sample size and coefficient of variation. The function allows
#' grouping in several domains.
#'
#' @param var numeric variable within the  \code{dataframe}.
#' @param dominios domains to be estimated separated by the + character.
#' @param subpop integer dummy variable to filter the dataframe
#' @param disenio complex design created by \code{survey} package
#' @param ci \code{boolean} indicating if the confidence intervals must be calculated
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @param standard_eval \code{boolean} Indicating if the function is wrapped inside a function, if \code{TRUE} avoid lazy eval errors
#' @param ess \code{boolean} Effective sample size
#' @param rm.na \code{boolean} Remove NA if it is required
#' @param deff \code{boolean} Design effect
#' @param rel_error \code{boolean} Relative error
#' @param unweighted \code{boolean} Add non weighted count if it is required
#' @import survey
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'
#' @examples
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_mean("gastot_hd", "zona+sexo",  disenio = dc)
#' @export

create_mean = function(var, dominios = NULL, subpop = NULL, disenio, ci = F, ess = F, ajuste_ene = F, standard_eval = F,
                       rm.na = F, deff = F, rel_error = F, unweighted = F) {


  # Homologar nombres de variables  del diseño
  disenio <- standardize_design_variables(disenio)

  # Sacar los NA si el usuario lo requiere
  if (rm.na == T) {
    disenio <- disenio[!is.na(disenio$variables[[var]])]
  }

  # Chequear que la variable objetivo y la variable subpop cumplan con ciertas condiciones
  check_input_var(var, disenio)
  check_subpop_var(subpop, disenio)

  # Lanzar warning del error estándar cuando no se usa el diseño
  se_message(disenio)

  # Filtrar diseño, si el usuario agrega el parámetro subpop
  disenio <- filter_design(disenio, subpop)

  #Convertir los inputs en formulas para adecuarlos a survey
  var_form <- convert_to_formula(var)

  # Convertir en formula para survey
  dominios_form <- convert_to_formula(dominios)

  #Generar la tabla con los calculos
  tabla <- calcular_tabla(var_form, dominios_form, disenio, fun = survey::svymean)

  # Crear listado de variables que se usan para el cálculo
  agrupacion <- create_groupby_vars(dominios)

  #Calcular el tamanio muestral de cada grupo
  n <- get_sample_size(disenio$variables, agrupacion)

  #Calcular los grados de libertad de todos los cruces
  gl <- get_df(disenio, agrupacion)

  #Extrear el coeficiente de variacion
  cv <- get_cv(tabla, disenio, agrupacion)

  #Unir toda la informacion en una tabla final
  final <- create_output(tabla, agrupacion,  gl, n, cv)

  # Ordenar las columnas y estandarizar los nombres de las variables
  final <- standardize_columns(final, var, denom = NULL )

  # Se calculan los intervalos de confianza solo si el usuario lo requiere
  if (ci == T) {
    final <- calcular_ic(final,  ajuste_ene = ajuste_ene)
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
#' @param dominios domains to be estimated separated by the + character.
#' @param subpop integer dummy variable to filter the dataframe
#' @param disenio complex design created by \code{survey} package
#' @param ci \code{boolean} indicating if the confidence intervals must be calculated
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @param standard_eval \code{boolean} Indicating if the function is wrapped inside a function, if \code{TRUE} avoid lazy eval errors
#'
#' @param deff \code{boolean} Design effect
#' @param ess \code{boolean} Effective sample size
#' @param rm.na \code{boolean} Remove NA if it is required
#' @param rel_error \code{boolean} Relative error
#'
#' @param unweighted \code{boolean} Add non weighted count if it is required
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'
#' @examples
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_total("gastot_hd", "zona+sexo", subpop = "ocupado", disenio = dc)
#' @export

create_total <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F, ess = F, ajuste_ene = F, standard_eval = F, rm.na = F,
                         deff = F, rel_error = F, unweighted = F) {

  # Homologar nombres de variables  del diseño
  disenio <- standardize_design_variables(disenio)

  # Sacar los NA si el usuario lo requiere
  if (rm.na == T) {
    disenio <- disenio[!is.na(disenio$variables[[var]])]
  }

  # Chequear que la variable objetivo y la variable subpop cumplan con ciertas condiciones
  check_input_var(var, disenio, estimation = "total")
  check_subpop_var(subpop, disenio)

  # Lanzar warning del error estándar cuando no se usa el diseño
  se_message(disenio)

  # Filtrar diseño, si el usuario agrega el parámetro subpop
  disenio <- filter_design(disenio, subpop)


  #Convertir los inputs en formulas para adecuarlos a survey
  var_form <- convert_to_formula(var)

  # Convertir en formula para survey
  dominios_form <- convert_to_formula(dominios)

  #Generar la tabla con los calculos
  tabla <- calcular_tabla(var_form, dominios_form, disenio, fun = survey::svytotal)

  # Crear listado de variables que se usan para el cálculo
  agrupacion <- create_groupby_vars(dominios)

  #Calcular el tamanio muestral de cada grupo
  n <- get_sample_size(disenio$variables, agrupacion)


  #Calcular los grados de libertad de todos los cruces
  gl <- get_df(disenio, agrupacion)

  #Extrear el coeficiente de variacion
  cv <- get_cv(tabla, disenio, agrupacion)

  #Unir toda la informacion en una tabla final
  final <- create_output(tabla, agrupacion,  gl, n, cv)

  # Ordenar las columnas y estandarizar los nombres de las variables
  final <- standardize_columns(final, var, denom = NULL )

  # Se calculan los intervalos de confianza solo si el usuario lo requiere
  if (ci == T) {
    final <- calcular_ic(final,  ajuste_ene = ajuste_ene)
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
#' @param dominios domains to be estimated separated by the + character.
#' @param subpop integer dummy variable to filter the dataframe
#' @param disenio complex design created by \code{survey} package
#' @param ci \code{boolean} indicating if the confidence intervals must be calculated
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @param standard_eval \code{boolean} Indicating if the function is wrapped inside a function, if \code{TRUE} avoid lazy eval errors
#'
#' @param deff \code{boolean} Design effect
#' @param ess \code{boolean} Effective sample size
#' @param rm.na \code{boolean} Remove NA if it is required
#' @param rel_error \code{boolean} Relative error
#'
#' @param unweighted \code{boolean} Add non weighted count if it is required
#' @param df_type \code{string} Use degrees of freedom calculation approach from INE Chile or CEPAL, by default "ine".
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#' @import tidyr
#' @examples
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_size("ocupado", "zona+sexo", disenio = dc)
#' @export

create_size <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F, ess = F, ajuste_ene = F, standard_eval = F, rm.na = F,
                         deff = F, rel_error = F,  unweighted = F, df_type = "ine") {

  # Homologar nombres de variables  del diseño
  disenio <- standardize_design_variables(disenio)

  # Sacar los NA si el usuario lo requiere
  if (rm.na == T) {
    disenio <- disenio[!is.na(disenio$variables[[var]])]
  }

  # Chequear que la variable objetivo y la variable subpop cumplan con ciertas condiciones
  check_input_var(var, disenio, estimation = "size")
  check_subpop_var(subpop, disenio)

  # Lanzar warning del error estándar cuando no se usa el diseño
  se_message(disenio)

  # Filtrar diseño, si el usuario agrega el parámetro subpop
  disenio <- filter_design(disenio, subpop)

  #Convertir los inputs en formulas para adecuarlos a survey
  var_form <- convert_to_formula(var)

  # Convertir en formula para survey
  dominios_form <- convert_to_formula(dominios)

  # Crear listado de variables que se usan para el cálculo
  agrupacion <- create_groupby_vars(dominios)

  # Add estimation variable for the case ine-size
  if (df_type == "ine") {
    agrupacion <- c(agrupacion, var)
    dominios_form <- convert_to_formula(paste0(dominios, "+", var))
  }

  #Generar la tabla con los calculos
  tabla <- calcular_tabla(var_form, dominios_form, disenio, fun = survey::svytotal)

    #Calcular el tamanio muestral de cada grupo
  n <- get_sample_size(disenio$variables, agrupacion, df_type)

  #Calcular los grados de libertad de todos los cruces
  gl <- get_df(disenio,agrupacion,df_type)

  #Extrear el coeficiente de variacion
  cv <- get_cv(tabla, disenio, agrupacion)

  if(df_type == "ine" & is.null(dominios)){
    cv <- cv[2]
  }

  #Unir toda la informacion en una tabla final
  final <- create_output(tabla, agrupacion,  gl, n, cv)

  # Ordenar las columnas y estandarizar los nombres de las variables
  final <- standardize_columns(final, var, denom = NULL)

  # Se calculan los intervalos de confianza solo si el usuario lo requiere
  if (ci == T) {
    final <- calcular_ic(final,  ajuste_ene = ajuste_ene)
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
#' @param dominios domains to be estimated separated by the + character.
#' @param disenio complex design created by \code{survey} package
#' @param subpop integer dummy variable to filter the dataframe
#' @param ci \code{boolean} indicating if the confidence intervals must be calculated
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @param standard_eval \code{boolean} Indicating if the function is wrapped inside a function, if \code{TRUE} avoid lazy eval errors
#' @param deff \code{boolean} Design effect
#' @param ess \code{boolean} Effective sample size
#' @param rel_error \code{boolean} Relative error
#'
#' @param log_cv \code{boolean} logarithmic coefficient of variation
#'
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
#' create_prop(var = "gasto_zona1", denominador = "gastot_hd", disenio =  dc)
#'
#' enusc <- filter(enusc, Kish == 1)
#'
#' dc <- svydesign(ids = ~Conglomerado, strata = ~VarStrat, data = enusc, weights = ~Fact_Pers)
#' options(survey.lonely.psu = "certainty")
#' create_prop(var = "VP_DC", denominador = "hom_insg_taxi", disenio = dc)
#'
#' @export
#'

create_prop = function(var, denominador = NULL, dominios = NULL, subpop = NULL, disenio, ci = F, deff = F, ess = F, ajuste_ene = F,
                       rel_error = F, log_cv = F, unweighted = F, standard_eval = F){


  if(!is.null(denominador)){
    final = create_ratio_internal(var,denominador, dominios, subpop, disenio, ci, deff, ess,  ajuste_ene, rel_error )
  }

  if(is.null(denominador)) {
    final = create_prop_internal(var,  dominios, subpop, disenio, ci, deff, ess,  ajuste_ene, rel_error, log_cv, unweighted)
  }

  # Add a class to the object
  final <- add_class(final, "calidad.prop")

  return(final)
}


