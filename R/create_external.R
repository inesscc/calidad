
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
#' create_mean(gastot_hd, zona+sexo,  disenio = dc)
#' @export

create_mean = function(var, dominios = NULL, subpop = NULL, disenio, ci = F, ess = F, ajuste_ene = F, standard_eval = F,
                       rm.na = F, deff = F, rel_error = F, unweighted = F) {

  # Homologar nombres de variables  del diseño
  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]
  disenio$variables$fe = disenio$variables[[unificar_variables_factExp(disenio)]]

  # Sacar los NA si el usuario lo requiere
  if (rm.na == T) {
    disenio <- disenio[!is.na(disenio$variables[[var]])]
  }

  # Chequear que la variable objetivo y la variable subpop cumplan con ciertas condiciones
  check_input_var(var, disenio)
  check_subpop_var(subpop, disenio)

  # Filtrar diseño, si el usuario agrega el parámetro subpop
  disenio <- filter_design(disenio, subpop)

  #Convertir los inputs en formulas para adecuarlos a survey
  var_form <- convert_to_formula(var)


  # Convertir en formula para survey
  dominios_form <- convert_to_formula(dominios)

  #Generar la tabla con los calculos
  tabla <- calcular_tabla(var_form, dominios_form, disenio)

  # Crear listado de variables que se usan para el cálculo
  agrupacion <- create_groupby_vars(dominios)

  #Calcular el tamanio muestral de cada grupo
  n <- calcular_n(disenio$variables, agrupacion)

  #Calcular los grados de libertad de todos los cruces
  gl <- get_df(disenio$variables, agrupacion)

  #Extrear el coeficiente de variacion
  cv <- get_cv(tabla, disenio, agrupacion)

  #Unir toda la informacion en una tabla final
  final <- create_output(tabla, agrupacion,  gl, n, cv)

  # Ordenar las columnas y estandarizar los nombres de las variables
  final <- standardize_columns(final, var )

  # Se calculan los intervalos de confianza solo si el usuario lo requiere
  if (ci == T) {
    final <- calcular_ic(final,  ajuste_ene = ajuste_ene)
  }


  # add relative error, if the user uses this parameter
  if (rel_error == T) {
    final <- final %>%
      dplyr::mutate(relative_error = stats::qt(c(.975), df = df) * cv)
  }


  # add the ess if the user uses this parameter
  final <- get_ess(ess)

  # add non weighted count if it is required
  if (unweighted) {
    final <- final %>%
      dplyr::mutate(unweighted = n)
  }

  return(final)
}


#--------------------------------------------------------------------

#' Create the inputs to evaluate the quality of the sum of continuous variables
#'
#' \code{create_tot_con} generates a \code{dataframe} with the following elements: sum,
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
#' create_tot_con(gastot_hd, zona+sexo, subpop = ocupado, disenio = dc)
#' @export

create_tot_con <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F, ess = F, ajuste_ene = F, standard_eval = F, rm.na = F,
                           deff = F, rel_error = F, unweighted = F) {

  # chequear_var_disenio(disenio$variables)

  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]
  disenio$variables$fe = disenio$variables[[unificar_variables_factExp(disenio)]]


  if(standard_eval == F){

    var <- rlang::enexpr(var)
    var <- rlang::expr_name(var)

    dominios <- rlang::enexpr(dominios)
    if(!is.null(dominios)){
      dominios <- rlang::expr_name(dominios)
    }

    subpop <- rlang::enexpr(subpop)
    if(!is.null(subpop)){
      subpop <- rlang::expr_name(subpop)
    }

  }

  # Sacar los NA si el usuario lo requiere
  if (rm.na == T) {
    disenio <- disenio[!is.na(disenio$variables[[var]])]
  }


  # Verificar que la variable de estimacion sea numerica. Se interrumpe si no es numerica
  if (!is.numeric(disenio$variables[[var]]) ) stop("Debes usar una variable numerica")

  # Pasar la variable objetivo al formato de survey
  var_form <- paste0("~", var) %>%
    stats::as.formula()

  # ESTO CORRESPONDE AL CASO EN EL QUE HAY DESAGREGACIoN
  if (!is.null(dominios)) {

    # Esto corre para el caso en el que NO hay subpop
    if (is.null(subpop)) {

      #Identificar las variables ingresadas para la desagregacion
      agrupacion <- dominios %>%
        stringr::str_split(pattern = "\\+")

      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrup1 <- c(agrupacion, var)

      # Agregar ~ para adecuar a formato de survey
      dominios_form <- paste0("~", dominios) %>%
        stats::as.formula()

      # Esto corre para subpop
    } else if (!is.null(subpop)) { # caso que tiene subpop

      # Chequear que la variable de subpop es una dummy. Si no se cumple, se interrumpe la ejecucion
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))
      if (sum(is.na(disenio$variables[[subpop]] > 0 ))) stop("subpop contains NAs!")

      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("subpop must be a dummy variable!")

      #Identificar las variables ingresadas para la desagregacion
      agrupacion <- dominios %>%
        stringr::str_split(pattern = "\\+")
      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrupacion <- c(subpop, agrupacion)
      agrup1 <- c(agrupacion, var)


      dominios_form <- paste(subpop, dominios, sep =  "+")
      dominios_form <- paste0("~", dominios_form) %>%
        stats::as.formula()
    }


    tabla <- survey::svyby(formula = var_form, by = dominios_form, design = disenio, FUN = survey::svytotal, deff = deff)

    gl <- calcular_upm(disenio$variables, agrupacion) %>%
      dplyr::left_join(calcular_estrato(disenio$variables, agrupacion), by = agrupacion) %>%
      dplyr::mutate(gl = .data$upm - .data$varstrat)  %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)


    n <- calcular_n(disenio$variables, dominios = agrupacion) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    # Coeficiente de variación
    tabla$cv <- survey::cv(tabla)

    # #Unir toda la informacion. Se hace con join para asegurar que no existan problemas en la union
    final <- tabla %>%
      dplyr::mutate_at(dplyr::vars(agrupacion), as.character) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl" )),
                       by = agrupacion) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n" )),
                       by = agrupacion) %>%
      dplyr::rename(coef_var = cv) %>%
      dplyr::relocate(coef_var, .after = last_col()) %>%
      dplyr::relocate(gl, .after = n)


    # Ajustar nombres de la tabla, para que tengan un formato estándar. Se deja el deff al final
    names(final) <- names(final) %>%
      stringr::str_replace(pattern = paste0("DEff.", var), "deff") %>%
      stringr::str_replace(pattern =  var, "total")


    #Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final, tipo = "total_agregado",ajuste_ene = ajuste_ene)
    }


    # ESTE ES EL CASO NO AGREGADO
  } else {

    tabla <- survey::svytotal(x = var_form, design = disenio, deff = T )

    # Tabla con los totales
    totales <- as.data.frame(tabla) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::rename(se = var)


    # Tamanio muestral
    n <- nrow(disenio$variables) %>%
      as.data.frame() %>%
      dplyr::mutate(variable = var) %>%
      dplyr::rename(n = ".")

    # Grados de libertad
    upm <- length(unique(disenio$variables$varunit))
    varstrat <- length(unique(disenio$variables$varstrat))
    gl <- cbind(upm, varstrat)
    gl <- gl %>%
      as.data.frame() %>%
      dplyr::mutate(variable = var,
                    gl =  upm - varstrat)

    # Coeficiente de variacion
    cv <- cv(tabla, design = disenio)
    cv <- as.data.frame(cv) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::rename(coef_var = var)

    # COnstruir tabla final
    final <- totales %>%
      dplyr::left_join(n, by = "variable") %>%
      dplyr::left_join(gl %>% dplyr::select(-upm, -varstrat), by = "variable") %>%
      dplyr::left_join(cv, by = "variable")


    # Se calcula el intervalo de confianza solo si el usuario lo pide
    if (ci == T) {
      final <- calcular_ic(data = final, tipo = "total_agregado",  ajuste_ene = ajuste_ene)
    }

  }

  # Reorder columns if it is neccesary
  if (deff == T) {
    final <- final %>%
      dplyr::relocate(deff, .after = dplyr::last_col())
  }


  # add the ess if the user uses this parameter
  final <- get_ess(ess)


  # add relative error, if the user uses this parameter
  if (rel_error == T) {
    final <- final %>%
      dplyr::mutate(relative_error = stats::qt(c(.975), df = gl) * coef_var)
  }

  # add non weighted count if it is required
  if (unweighted) {
    final <- final %>%
      dplyr::mutate(unweighted = n)
  }


  if(!is.null(dominios) && !is.null(subpop)){
    final = final %>% dplyr::filter(!!rlang::parse_expr(subpop) == 1) %>% dplyr::select(-!!rlang::parse_expr(subpop))
  }



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
#' create_tot_con(gastot_hd, zona+sexo, subpop = ocupado, disenio = dc)
#' @export

create_total <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F, ess = F, ajuste_ene = F, standard_eval = F, rm.na = F,
                         deff = F, rel_error = F, unweighted = F) {

  # chequear_var_disenio(disenio$variables)

  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]
  disenio$variables$fe = disenio$variables[[unificar_variables_factExp(disenio)]]


  if(standard_eval == F){

    var <- rlang::enexpr(var)
    var <- rlang::expr_name(var)

    dominios <- rlang::enexpr(dominios)
    if(!is.null(dominios)){
      dominios <- rlang::expr_name(dominios)
    }

    subpop <- rlang::enexpr(subpop)
    if(!is.null(subpop)){
      subpop <- rlang::expr_name(subpop)
    }

  }

  # Sacar los NA si el usuario lo requiere
  if (rm.na == T) {
    disenio <- disenio[!is.na(disenio$variables[[var]])]
  }


  # Verificar que la variable de estimacion sea numerica. Se interrumpe si no es numerica
  if (!is.numeric(disenio$variables[[var]]) ) stop("Debes usar una variable numerica")

  # Pasar la variable objetivo al formato de survey
  var_form <- paste0("~", var) %>%
    stats::as.formula()

  # ESTO CORRESPONDE AL CASO EN EL QUE HAY DESAGREGACIoN
  if (!is.null(dominios)) {

    # Esto corre para el caso en el que NO hay subpop
    if (is.null(subpop)) {

      #Identificar las variables ingresadas para la desagregacion
      agrupacion <- dominios %>%
        stringr::str_split(pattern = "\\+")

      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrup1 <- c(agrupacion, var)

      # Agregar ~ para adecuar a formato de survey
      dominios_form <- paste0("~", dominios) %>%
        stats::as.formula()

      # Esto corre para subpop
    } else if (!is.null(subpop)) { # caso que tiene subpop

      # Chequear que la variable de subpop es una dummy. Si no se cumple, se interrumpe la ejecucion
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))
      if (sum(is.na(disenio$variables[[subpop]] > 0 ))) stop("subpop contains NAs!")

      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("subpop must be a dummy variable!")

      #Identificar las variables ingresadas para la desagregacion
      agrupacion <- dominios %>%
        stringr::str_split(pattern = "\\+")
      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrupacion <- c(subpop, agrupacion)
      agrup1 <- c(agrupacion, var)


      dominios_form <- paste(subpop, dominios, sep =  "+")
      dominios_form <- paste0("~", dominios_form) %>%
        stats::as.formula()
    }


    tabla <- survey::svyby(formula = var_form, by = dominios_form, design = disenio, FUN = survey::svytotal, deff = deff)

    gl <- calcular_upm(disenio$variables, agrupacion) %>%
      dplyr::left_join(calcular_estrato(disenio$variables, agrupacion), by = agrupacion) %>%
      dplyr::mutate(gl = .data$upm - .data$varstrat)  %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)


    n <- calcular_n(disenio$variables, dominios = agrupacion) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    # Coeficiente de variación
    tabla$cv <- survey::cv(tabla)

    # #Unir toda la informacion. Se hace con join para asegurar que no existan problemas en la union
    final <- tabla %>%
      dplyr::mutate_at(dplyr::vars(agrupacion), as.character) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl" )),
                       by = agrupacion) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n" )),
                       by = agrupacion) %>%
      dplyr::rename(coef_var = cv) %>%
      dplyr::relocate(coef_var, .after = last_col()) %>%
      dplyr::relocate(gl, .after = n)


    # Ajustar nombres de la tabla, para que tengan un formato estándar. Se deja el deff al final
    names(final) <- names(final) %>%
      stringr::str_replace(pattern = paste0("DEff.", var), "deff") %>%
      stringr::str_replace(pattern =  var, "total")


    #Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final, tipo = "total_agregado",ajuste_ene = ajuste_ene)
    }


    # ESTE ES EL CASO NO AGREGADO
  } else {

    tabla <- survey::svytotal(x = var_form, design = disenio, deff = T )

    # Tabla con los totales
    totales <- as.data.frame(tabla) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::rename(se = var)


    # Tamanio muestral
    n <- nrow(disenio$variables) %>%
      as.data.frame() %>%
      dplyr::mutate(variable = var) %>%
      dplyr::rename(n = ".")

    # Grados de libertad
    upm <- length(unique(disenio$variables$varunit))
    varstrat <- length(unique(disenio$variables$varstrat))
    gl <- cbind(upm, varstrat)
    gl <- gl %>%
      as.data.frame() %>%
      dplyr::mutate(variable = var,
                    gl =  upm - varstrat)

    # Coeficiente de variacion
    cv <- cv(tabla, design = disenio)
    cv <- as.data.frame(cv) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::rename(coef_var = var)

    # COnstruir tabla final
    final <- totales %>%
      dplyr::left_join(n, by = "variable") %>%
      dplyr::left_join(gl %>% dplyr::select(-upm, -varstrat), by = "variable") %>%
      dplyr::left_join(cv, by = "variable")


    # Se calcula el intervalo de confianza solo si el usuario lo pide
    if (ci == T) {
      final <- calcular_ic(data = final, tipo = "total_agregado",  ajuste_ene = ajuste_ene)
    }

  }

  # Reorder columns if it is neccesary
  if (deff == T) {
    final <- final %>%
      dplyr::relocate(deff, .after = dplyr::last_col())
  }


  # add the ess if the user uses this parameter
  final <- get_ess(ess)


  # add relative error, if the user uses this parameter
  if (rel_error == T) {
    final <- final %>%
      dplyr::mutate(relative_error = stats::qt(c(.975), df = gl) * coef_var)
  }

  # add non weighted count if it is required
  if (unweighted) {
    final <- final %>%
      dplyr::mutate(unweighted = n)
  }


  if(!is.null(dominios) && !is.null(subpop)){
    final = final %>% dplyr::filter(!!rlang::parse_expr(subpop) == 1) %>% dplyr::select(-!!rlang::parse_expr(subpop))
  }



  return(final)
}



#--------------------------------------------------------------------

#' Create the inputs to evaluate the quality of total estimations
#'
#' \code{create_tot} generates a \code{dataframe} with the following elements: sum,
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
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#' @import tidyr
#' @examples
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_tot(ocupado, zona+sexo, disenio = dc)
#' @export

create_tot <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F, ess = F, ajuste_ene = F, standard_eval = F, rm.na = F,
                       deff = F, rel_error = F,  unweighted = F) {

  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]
  disenio$variables$fe = disenio$variables[[unificar_variables_factExp(disenio)]]

  if(standard_eval == F){

    var <- rlang::enexpr(var)
    var <- rlang::expr_name(var)

    dominios <- rlang::enexpr(dominios)
    if(!is.null(dominios)){
      dominios <- rlang::expr_name(dominios)
    }

    subpop <- rlang::enexpr(subpop)
    if(!is.null(subpop)){
      subpop <- rlang::expr_name(subpop)
    }

  }

  # Sacar los NA si el usuario lo requiere
  if (rm.na == T) {
    disenio <- disenio[!is.na(disenio$variables[[var]])]
  }

  # ESTO CORRESPONDE AL CASO CON DESAGREGACIoN
  if (!is.null(dominios)) {

    # Verificar que la variabe de entrada es correcta
    if (!is.numeric(disenio$variables[[var]])) stop("La variable debe ser numerica!")

    # Verificar que la variable es dummy
    test <- disenio$variable %>%
      dplyr::mutate(test = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0, 1, 0)) %>%
      dplyr::summarise(pasa = sum(test))

    n_filas <- nrow(disenio$variable)
    if (n_filas != test$pasa) stop("Debes usar una variable dummy cuando desagregas!")

    # Esto corre para el caso en el que NO hay subpop
    if (is.null(subpop)) {

      #Identificar las variables ingresadas para la desagregacion
      agrupacion <- dominios %>%
        stringr::str_split(pattern = "\\+")
      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrup1 <- c(agrupacion, var)

      # Agregar ~ para adecuar a formato de survey
      dominios_form <- paste0("~", dominios) %>%
        stats::as.formula()

      # Esto corre para subpop
    } else if (!is.null(rlang::enexpr(subpop))) { # caso que tiene subpop

      # Chequear que la variable de subpop es una dummy. Si no se cumple, se interrumpe la ejecucion
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))
      if (sum(is.na(disenio$variables[[subpop]] > 0 ))) stop("subpop contains NAs!")

      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("subpop must be a dummy variable!")

      #Identificar las variables ingresadas para la desagregacion
      agrupacion <- dominios %>%
        stringr::str_split(pattern = "\\+")
      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrupacion <- c(subpop, agrupacion  )
      agrup1 <- c(agrupacion, var)

      #dominios_form <- paste(agrupacion, "+")
      dominios_form <- paste(subpop, dominios, sep =  "+")
      dominios_form <- paste0("~", dominios_form) %>%
        stats::as.formula()

    }
    # Pasar a la variable objetivo al formato de survey
    var_form <- paste0("~",var) %>%
      stats::as.formula()

    # Generar la tabla de estimaciones
    tabla <- survey::svyby(formula = var_form, by = dominios_form, design = disenio, FUN = survey::svytotal, deff = deff)

    gl <- calcular_upm(disenio$variables, agrup1) %>%
      dplyr::left_join(calcular_estrato(disenio$variables, agrup1), by = agrup1) %>%
      dplyr::mutate(gl = .data$upm - .data$varstrat)  %>%
      dplyr::filter(!!rlang::parse_expr(var) == 1)  %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    n <- calcular_n(disenio$variables, dominios = agrup1) %>%
      dplyr::filter(!!rlang::parse_expr(var) == 1) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)


    # # Coeficiente de variación
    tabla$cv <- survey::cv(tabla)


    # #Unir toda la informacion. Se hace con join para asegurar que no existan problemas en la union
    final <- tabla %>%
      dplyr::mutate_at(dplyr::vars(agrupacion), as.character) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl" )),
                       by = agrupacion) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n" )),
                       by = agrupacion) %>%
      dplyr::rename(coef_var = cv) %>%
      dplyr::relocate(coef_var, .after = last_col()) %>%
      dplyr::relocate(gl, .after = n)


    # Ajustar nombres de la tabla, para que tengan un formato estándar. Se deja el deff al final
    names(final) <- names(final) %>%
      stringr::str_replace(pattern = paste0("DEff.", var), "deff") %>%
      stringr::str_replace(pattern =  var, "total")

    #Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final, tipo = "total_agregado",ajuste_ene = ajuste_ene)
    }



    # ESTO CORRESPONDE AL CASO SIN DESAGRAGACIoN
  } else {

    n_cat = length(unique(disenio$variable[[var]]))

    if (n_cat > 50 ) stop("La variable puede ser continua, posee mas de 50 categorias!")


    # Identificar las variables ingresadas por el usuario
    agrupacion <- var %>%
      stringr::str_split(pattern = "\\+")
    agrup1 <- stringr::str_remove_all(string =  agrupacion, pattern = " ")


    # Convertir variables a string. Esto se hace debido a que survey tiene distintos tratamientos para variables numericas o de string
    disenio <- survey::svydesign(ids = ~varunit, strata = ~varstrat,
                                 data = disenio$variables %>% dplyr::mutate_at(.vars = dplyr::vars(agrup1), list(as.character)),
                                 weights = ~fe)

    # Acomodar a formato de survey
    var_form <- paste0("~",var) %>%
      stats::as.formula()

    # Si el usuario ingresa subpoblacion, se filtra la base de datos para la subpoblacion de referencia
    if (!is.null(subpop)) {

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecucion
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))

      if (sum(is.na(disenio$variables[[subpop]] > 0 ))) stop("subpop contains NAs!")

      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("subpop must be a dummy variable!")

      # Aqui se filtra el disenio
      disenio <- disenio[disenio$variables[[subpop]] == 1]

    }

    # Tabla que se usa luego para calcular cv

    tabla <- survey::svytotal(x = var_form, design = disenio, deff = deff )

    # Tabla con los totales
    totales <- as.data.frame(tabla) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::rename(se = SE)

    # Tamanio muestral
    n <- purrr::map(agrup1, calcular_n_total, datos = disenio$variables) %>%
      purrr::reduce(dplyr::bind_rows)

    # Grados de libertad
    gl <- calcular_gl_total(agrup1, disenio$variables)

    #Extrear el coeficiente de variacion
    cv <- cv(tabla, design = disenio)
    cv <- as.data.frame(cv) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::rename(coef_var = cv)

    # COnstruir tabla final
    final <- totales %>%
      dplyr::left_join(n, by = "variable") %>%
      dplyr::left_join(gl %>% dplyr::select(-.data$upm, -.data$varstrat), by = "variable") %>%
      dplyr::left_join(cv, by = "variable")


    # Se calcula el intervalo de confianza solo si el usuario lo pide
    if (ci == T) {
      final <- calcular_ic(data = final, tipo = "total_agregado",  ajuste_ene = ajuste_ene)

    }




  }

  ##################
  # SHARED OUTPUTS #
  ##################


  # Las filas en las que no exsiten casos generan valores NA. Esos casos se eliminan
  names(final) <- tolower(names(final))
  final <- final %>%
    dplyr::filter(!is.nan(.data$coef_var))

  # Reorder columns if it is neccesary
  if (deff == T) {
    final <- final %>%
      dplyr::relocate(deff, .after = dplyr::last_col())
  }


  # add the ess if the user uses this parameter
  final <- get_ess(ess)

  # add relative error, if the user uses this parameter
  if (rel_error == T) {
    final <- final %>%
      dplyr::mutate(relative_error = stats::qt(c(.975), df = gl) * coef_var)
  }

  # add non weighted count if it is required
  if (unweighted) {
    final <- final %>%
      dplyr::mutate(unweighted = n)
  }


  if(!is.null(dominios) && !is.null(subpop)){
    final = final %>% dplyr::filter(!!rlang::parse_expr(subpop)  == 1) %>% dplyr::select(-!!rlang::parse_expr(subpop))
  }



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
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#' @import tidyr
#' @examples
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_tot(ocupado, zona+sexo, disenio = dc)
#' @export

create_size <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F, ess = F, ajuste_ene = F, standard_eval = F, rm.na = F,
                        deff = F, rel_error = F,  unweighted = F) {

  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]
  disenio$variables$fe = disenio$variables[[unificar_variables_factExp(disenio)]]

  if(standard_eval == F){

    var <- rlang::enexpr(var)
    var <- rlang::expr_name(var)

    dominios <- rlang::enexpr(dominios)
    if(!is.null(dominios)){
      dominios <- rlang::expr_name(dominios)
    }

    subpop <- rlang::enexpr(subpop)
    if(!is.null(subpop)){
      subpop <- rlang::expr_name(subpop)
    }

  }

  # Sacar los NA si el usuario lo requiere
  if (rm.na == T) {
    disenio <- disenio[!is.na(disenio$variables[[var]])]
  }

  # ESTO CORRESPONDE AL CASO CON DESAGREGACIoN
  if (!is.null(dominios)) {

    # Verificar que la variabe de entrada es correcta
    if (!is.numeric(disenio$variables[[var]])) stop("La variable debe ser numerica!")

    # Verificar que la variable es dummy
    test <- disenio$variable %>%
      dplyr::mutate(test = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0, 1, 0)) %>%
      dplyr::summarise(pasa = sum(test))

    n_filas <- nrow(disenio$variable)
    if (n_filas != test$pasa) stop("Debes usar una variable dummy cuando desagregas!")

    # Esto corre para el caso en el que NO hay subpop
    if (is.null(subpop)) {

      #Identificar las variables ingresadas para la desagregacion
      agrupacion <- dominios %>%
        stringr::str_split(pattern = "\\+")
      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrup1 <- c(agrupacion, var)

      # Agregar ~ para adecuar a formato de survey
      dominios_form <- paste0("~", dominios) %>%
        stats::as.formula()

      # Esto corre para subpop
    } else if (!is.null(rlang::enexpr(subpop))) { # caso que tiene subpop

      # Chequear que la variable de subpop es una dummy. Si no se cumple, se interrumpe la ejecucion
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))
      if (sum(is.na(disenio$variables[[subpop]] > 0 ))) stop("subpop contains NAs!")

      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("subpop must be a dummy variable!")

      #Identificar las variables ingresadas para la desagregacion
      agrupacion <- dominios %>%
        stringr::str_split(pattern = "\\+")
      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrupacion <- c(subpop, agrupacion  )
      agrup1 <- c(agrupacion, var)

      #dominios_form <- paste(agrupacion, "+")
      dominios_form <- paste(subpop, dominios, sep =  "+")
      dominios_form <- paste0("~", dominios_form) %>%
        stats::as.formula()

    }
    # Pasar a la variable objetivo al formato de survey
    var_form <- paste0("~",var) %>%
      stats::as.formula()

    # Generar la tabla de estimaciones
    tabla <- survey::svyby(formula = var_form, by = dominios_form, design = disenio, FUN = survey::svytotal, deff = deff)

    # gl <- calcular_upm(disenio$variables, agrup1) %>%
    #   dplyr::left_join(calcular_estrato(disenio$variables, agrup1), by = agrup1) %>%
    #   dplyr::mutate(gl = .data$upm - .data$varstrat)  %>%
    #   dplyr::filter(!!rlang::parse_expr(var) == 1)  %>%
    #   dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    gl <- calcular_upm(disenio$variables, agrupacion) %>%
      dplyr::left_join(calcular_estrato(disenio$variables, agrupacion), by = agrupacion) %>%
      dplyr::mutate(gl = .data$upm - .data$varstrat)  %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)


    n <- calcular_n(disenio$variables, dominios = agrupacion) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)


    # # Coeficiente de variación
    tabla$cv <- survey::cv(tabla)


    # #Unir toda la informacion. Se hace con join para asegurar que no existan problemas en la union
    final <- tabla %>%
      dplyr::mutate_at(dplyr::vars(agrupacion), as.character) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl" )),
                       by = agrupacion) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n" )),
                       by = agrupacion) %>%
      dplyr::rename(coef_var = cv) %>%
      dplyr::relocate(coef_var, .after = last_col()) %>%
      dplyr::relocate(gl, .after = n)

    # Ajustar nombres de la tabla, para que tengan un formato estándar. Se deja el deff al final
    names(final) <- names(final) %>%
      stringr::str_replace(pattern = paste0("DEff.", var), "deff") %>%
      stringr::str_replace(pattern =  var, "total")

    #Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final, tipo = "total_agregado",ajuste_ene = ajuste_ene)
    }


    # add unweighted count if it is required
    if (unweighted) {

      unweighted_n <- calcular_n(disenio$variables, dominios = agrup1) %>%
        dplyr::filter(!!rlang::parse_expr(var) == 1) %>%
        dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character) %>%
        dplyr::rename(unweighted = n)


      final <- final %>%
        dplyr::left_join(unweighted_n, by = agrupacion)
    }


    # ESTO CORRESPONDE AL CASO SIN DESAGRAGACIoN
  } else {

    n_cat = length(unique(disenio$variable[[var]]))

    if (n_cat > 50 ) stop("La variable puede ser continua, posee mas de 50 categorias!")


    # Identificar las variables ingresadas por el usuario
    agrupacion <- var %>%
      stringr::str_split(pattern = "\\+")
    agrup1 <- stringr::str_remove_all(string =  agrupacion, pattern = " ")


    # Convertir variables a string. Esto se hace debido a que survey tiene distintos tratamientos para variables numericas o de string
    disenio <- survey::svydesign(ids = ~varunit, strata = ~varstrat,
                                 data = disenio$variables %>% dplyr::mutate_at(.vars = dplyr::vars(agrup1), list(as.character)),
                                 weights = ~fe)

    # Acomodar a formato de survey
    var_form <- paste0("~",var) %>%
      stats::as.formula()

    # Si el usuario ingresa subpoblacion, se filtra la base de datos para la subpoblacion de referencia
    if (!is.null(subpop)) {

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecucion
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))

      if (sum(is.na(disenio$variables[[subpop]] > 0 ))) stop("subpop contains NAs!")

      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("subpop must be a dummy variable!")

      # Aqui se filtra el disenio
      disenio <- disenio[disenio$variables[[subpop]] == 1]

    }

    # Tabla que se usa luego para calcular cv

    tabla <- survey::svytotal(x = var_form, design = disenio, deff = deff )

    # Tabla con los totales
    totales <- as.data.frame(tabla) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::rename(se = SE)

    # Tamanio muestral

    n <- purrr::map(agrup1, calcular_n_total, datos = disenio$variables) %>%
      purrr::reduce(dplyr::bind_rows)


    # Grados de libertad
    gl <- calcular_gl_total(agrup1, disenio$variables)

    #Extrear el coeficiente de variacion
    cv <- cv(tabla, design = disenio)
    cv <- as.data.frame(cv) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::rename(coef_var = cv)

    # COnstruir tabla final
    final <- totales %>%
      dplyr::left_join(n, by = "variable") %>%
      dplyr::left_join(gl %>% dplyr::select(-.data$upm, -.data$varstrat), by = "variable") %>%
      dplyr::left_join(cv, by = "variable")


    # Se calcula el intervalo de confianza solo si el usuario lo pide
    if (ci == T) {
      final <- calcular_ic(data = final, tipo = "total_agregado",  ajuste_ene = ajuste_ene)

    }

    # Últimos ajustes para crear tamaño de muestra y conteo no ponderado
    final <- final %>%
      dplyr::rename(unweighted = n)  %>%
      dplyr::mutate(n = sum(unweighted))

    # not remove  unweighted count if it is required
    if (unweighted == F) {
      final <- final %>%
        dplyr::select(-unweighted)
    }

  }

  ##################
  # SHARED OUTPUTS #
  ##################


  # Las filas en las que no exsiten casos generan valores NA. Esos casos se eliminan
  names(final) <- tolower(names(final))
  final <- final %>%
    dplyr::filter(!is.nan(.data$coef_var))

  # Reorder columns if it is neccesary
  if (deff == T) {
    final <- final %>%
      dplyr::relocate(deff, .after = dplyr::last_col())
  }


  # add the ess if the user uses this parameter
  final <- get_ess(ess)

  # add relative error, if the user uses this parameter
  if (rel_error == T) {
    final <- final %>%
      dplyr::mutate(relative_error = stats::qt(c(.975), df = gl) * coef_var)
  }



  if(!is.null(dominios) && !is.null(subpop)){
    final = final %>% dplyr::filter(!!rlang::parse_expr(subpop)  == 1) %>% dplyr::select(-!!rlang::parse_expr(subpop))
  }



  return(final)
}





#-----------------------------------------------------------------------


#' Create the inputs to evaluate the quality of median estimations
#'
#' \code{create_median} uses a non parametric method to generate a \code{dataframe}
#' with the following elements: sum, degrees of freedom, sample size and coefficient
#' of variation. The function allows grouping in several domains.
#'
#' @param var numeric variable within the  \code{dataframe}
#' @param dominios domains to be estimated separated by the + character.
#' @param subpop integer dummy variable to filter the dataframe
#' @param disenio complex design created by \code{survey} package
#' @param replicas \code{integer} indicating the number of replicates to be used
#' @param ci \code{boolean} indicating if the confidence intervals must be calculated
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @param standard_eval \code{boolean} Indicating if the function is wrapped inside a function, if \code{TRUE} avoid lazy eval errors
#' @param rm.na \code{boolean} Remove NA if it is required
#' @param seed numeric variable to get similar results, by default is set at 1234
#' @param rel_error \code{boolean} Relative error
#' @param interval_type string variable "quantile"
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#' @import itertools
#' @examples
#' dc <- survey::svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' dc_rep <-  survey::as.svrepdesign(dc , type = "subbootstrap", replicates=10)
#' create_median(gastot_hd, zona+sexo, disenio = dc)
#' @export

create_median <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F, replicas = 10,  ajuste_ene = F,standard_eval = F,
                          rm.na = F, seed = 1234, rel_error = F, interval_type = "quantile") {

  # Ajustar nombre de variables del disenio muestral
  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]
  disenio$variables$fe = disenio$variables[[unificar_variables_factExp(disenio)]]




  if(standard_eval == F){

    var <- rlang::enexpr(var)
    var <- rlang::expr_name(var)

    dominios <- rlang::enexpr(dominios)
    if(!is.null(dominios)){
      dominios <- rlang::expr_name(dominios)
    }

    subpop <- rlang::enexpr(subpop)
    if(!is.null(subpop)){
      subpop <- rlang::expr_name(subpop)
    }

  }


  # Sacar los NA si el usuario lo requiere
  if (rm.na == T) {
    disenio <- disenio[!is.na(disenio$variables[[var]])]
  }

  # Si las variables que están en dominios son factores, se hace la conversion a integer
  if (!is.null(dominios)) {
    disenio <- convert_to_integer(dominios, disenio)
  }

  # Arreglar las variables de disenioo para que tengan menos numeros.
  # Esto solo se hace si la variable de conglomerados es muy larga

  change_psu <- sum(nchar(as.character(disenio$variables$varunit))[1] >= 5) > 1

  if (change_psu) {
    keys <- disenio$variables %>%
      dplyr::group_by(varunit) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(varunit2 = dplyr::row_number()) %>%
      dplyr::select(.data$varunit2, varunit)

    disenio$variables <- disenio$variables %>%
      dplyr::left_join(keys, by = "varunit") %>%
      dplyr::select(-varunit) %>%
      dplyr::rename(varunit = .data$varunit2)

    # Volver a declarar el disenioo normal
    disenio <- survey::svydesign(ids = ~varunit, strata = ~varstrat, weights = ~fe, data = disenio$variables )

  }

  # Generar el disenio replicado
  set.seed(seed)
  disenio <-  survey::as.svrepdesign(disenio, type = "subbootstrap", replicates = replicas)


  # Chequear que la variable no sea character
  if (is.character(disenio$variables[[var]]) == T) stop("You are using a character vector!")

  #Chequear que la variable sea continua. Si no lo es, aparece un warning
  es_prop <- disenio$variables %>%
    dplyr::mutate(es_prop = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0, 1, 0))

  if (sum(es_prop$es_prop) == nrow(disenio$variables)) warning("It seems you are using a proportion variable!")


  #Convertir los inputs en formulas para adecuarlos a survey
  var_form <- paste0("~", var) %>%
    stats::as.formula()

  # ESTO CORRESPONDE AL CASO CON DESAGREGACIoN
  if (!is.null(dominios)) {

    # Esto corre para el caso en el que NO hay subpop
    if (is.null(subpop)) {

      dominios_form <- paste0("~",dominios) %>%
        stats::as.formula()

      #Generar la tabla con los calculos
      tabla <- calcular_medianas_internal(var_form, dominios_form, disenio)

      # Esto corre para subpop
    } else if (!is.null(subpop)) { # caso que tiene subpop

      # Chequear que la variable de subpop es una dummy. Si no se cumple, se interrumpe la ejecucion
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop) == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))

      if (sum(is.na(disenio$variables[[subpop]] > 0 ))) stop("subpop contains NAs!")

      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("subpop must be a dummy variable!")

      # Agregar a los dominios, la variable subpop
      dominios_form <-   paste(dominios, subpop, sep = "+")
      dominios_form <- paste0("~", dominios_form) %>%
        stats::as.formula()

      #Generar la tabla con los calculos

      tabla <- calcular_medianas_internal(var_form, dominios_form, disenio, sub = T)

    }

    #Extraer nombres
    nombres <- names(tabla)
    agrupacion <-  nombres[c(-(length(nombres) - 1), -length(nombres)) ]

    #Calcular el tamanio muestral de cada grupo
    n <- calcular_n(disenio$variables, agrupacion) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    #Calcular los grados de libertad de todos los cruces
    gl <- calcular_upm(disenio$variables, agrupacion) %>%
      dplyr::left_join(calcular_estrato(disenio$variables, agrupacion), by = agrupacion) %>%
      dplyr::mutate(gl = .data$upm - .data$varstrat) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)


    #Extrear el coeficiente de variacion
    #cv <- cv(tabla, design = disenio)
    cv <- tabla$se / tabla$V1

    cv <- tabla %>%
      dplyr::select(agrupacion) %>%
      dplyr::bind_cols(coef_var = cv) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    #Unir toda la informacion. Se hace con join para asegurar que no existan problemas en la union
    final <- tabla %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl")),
                       by = agrupacion) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n")),
                       by = agrupacion) %>%
      dplyr::left_join(cv %>% dplyr::select(c(agrupacion, "coef_var")),
                       by = agrupacion) %>%
      dplyr::rename(!!rlang::parse_expr(var) := .data$V1)



    names(final)[grep(var,names(final))] = "median"

    # Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final, tipo = "mediana_agregado",ajuste_ene = ajuste_ene)
    }

    # ESTO CORRESPONDE AL CASO SIN DESAGREGACIoN
  } else {


    # Si el usuario ingresa subpoblacion, se filtra la base de datos para la subpoblacion de referencia
    if (!is.null(subpop)) {

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecucion
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop) == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))

      if (sum(is.na(disenio$variables[[subpop]] > 0 ))) stop("subpop contains NAs!")
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("subpop must be a dummy variable!")

      disenio <- disenio[disenio$variables[[subpop]] == 1]
    }

    dominios_form = dominios
    #Generar la tabla con los calculos
    tabla <- calcular_tabla(var_form, dominios_form, disenio, media = F)

    # Tamanio muestral
    n <- nrow(disenio$variables)

    # Calcular grados de libertad
    varstrat <- length(unique(disenio$variables$varstrat))
    varunit <- length(unique(disenio$variables$varunit))
    gl <- varunit - varstrat

    # Calcular coeficiente de variacion
    cv <- cv(tabla, design = disenio)

    # Armar tabla final
    final <- data.frame(tabla )

    # Armar tabla completa con todos los insumos
    final <- dplyr::bind_cols(final, "gl" = gl , "n" = n, "coef_var" = cv[1])
    names(final)[2] <- "se"

    names(final)[grep("quantiles",names(final))] = "median"

    # Se calcular el intervalo de confianza solo si el usuario lo pide
    if (ci == T) {
      final <- calcular_ic(data = final, tipo = "mediana_agregado",  ajuste_ene = ajuste_ene)
    }

  }

  # add relative error, if the user uses this parameter
  if (rel_error == T) {
    final <- final %>%
      dplyr::mutate(relative_error = stats::qt(c(.975), df = gl) * coef_var)
  }


  # Filtrar filas que no son utiles
  if(!is.null(dominios) && !is.null(subpop)){
    final <-  final %>%
      dplyr::filter(!!rlang::parse_expr(subpop)  == 1) %>%
      dplyr::select(-!!rlang::parse_expr(subpop))
  }

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
#' create_prop(var = gasto_zona1, denominador = gastot_hd, disenio =  dc)
#'
#' enusc <- filter(enusc, Kish == 1)
#'
#' dc <- svydesign(ids = ~Conglomerado, strata = ~VarStrat, data = enusc, weights = ~Fact_Pers)
#' options(survey.lonely.psu = "certainty")
#' create_prop(var = VP_DC, denominador = hom_insg_taxi, disenio = dc)
#'
#' @export
#'

create_prop = function(var, denominador = NULL, dominios = NULL, subpop = NULL, disenio, ci = F, deff = F, ess = F, ajuste_ene = F,
                       rel_error = F, log_cv = F, unweighted = F, standard_eval = F){

  #  # Encapsular inputs para usarlos mas tarde
  if (standard_eval == F) {

    var <- rlang::enexpr(var)
    var <- rlang::expr_name(var)

    denominador <- rlang::enexpr(denominador)
    if(!is.null(denominador)){
      denominador <-  rlang::expr_name(denominador)
    }

    dominios <- rlang::enexpr(dominios)
    if(!is.null(dominios)){
      dominios <- rlang::expr_name(dominios)
    }
    subpop <- rlang::enexpr(subpop)
    if(!is.null(subpop)){
      subpop <- rlang::expr_name(subpop)
    }
  }

  if(!is.null(denominador)){
    final = create_ratio_internal(var,denominador, dominios, subpop, disenio, ci, deff, ess,  ajuste_ene, rel_error )
  }

  if(is.null(denominador)) {
    final = create_prop_internal(var,  dominios, subpop, disenio, ci, deff, ess,  ajuste_ene, rel_error, log_cv, unweighted)
  }
  return(final)
}


