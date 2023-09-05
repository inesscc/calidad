
tolower_strings <-  function(x) {
  if (!is.null(x))  {
    tolower(x)
  } else {
    NULL
  }
}



# get design variables
get_design_vars <- function(design) {
  if (as.character(design$call$ids)[[2]] != "1") {
    psu <- unificar_variables_upm(design)
    strata <-   unificar_variables_estrato(design)
    vars <- c(psu, strata)
  } else {
    vars <- NULL
  }
  return(vars)
}


# Turn on all the indicators needed for the eclac standard
eclac_standard <- function(eclac,  env = parent.frame(), proportion = FALSE  ) {

  if (eclac == TRUE & proportion == FALSE) {
    ess <- TRUE
    unweighted <- TRUE
    deff <- TRUE
    eclac_indicators = list("ess" = ess, "unweighted" = unweighted, "deff" = deff)

  } else if (eclac == TRUE & proportion == TRUE) {
    ess <- TRUE
    unweighted <- TRUE
    deff <- TRUE
    log_cv <- TRUE
    eclac_indicators = list("ess" = ess, "unweighted" = unweighted, "deff" = deff, "log_cv" = log_cv)

  } else if (eclac == FALSE & proportion == TRUE) {
    eclac_indicators = list("ess" = get("ess", envir = env),
                            "unweighted" = get("unweighted", envir = env),
                            "deff" = get("deff", envir = env),
                            "log_cv" =  get("log_cv", envir = env))

  } else {
    eclac_indicators = list("ess" = get("ess", envir = env),
                            "unweighted" = get("unweighted", envir = env),
                            "deff" = get("deff", envir = env))
  }

  return(eclac_indicators)
}

#--------------------------------

formula_to_string <- function(formula) {
  v <- deparse(formula) %>%
    stringr::str_remove("~")
  return(v)
}


#--------------------------------

fix_repeated_columns <- function(table, v) {
  v <- deparse(v) %>%
    stringr::str_remove("~")

  aparicion <- 0
  i <- 1
  for (col in names(table)) {
    if (col == v) {
      aparicion <- aparicion + 1
      if (col == v & aparicion == 2) {
        names(table)[i] <- "est"

      }
    }
    i = i + 1
  }

  return(table)
}



#--------------------------------------------------
add_class <-  function(object, new_class) {
  class(object)  <- append(class(object), new_class, after = FALSE)
  return(object)
  }



#------------------------------------------
convert_ratio_to_df <- function(table, domains) {
  if (is.null(domains)) {

    stat = table[[1]][1]

    names(stat) = "stat"

    se = survey::SE(table)[[1]]

    names(se) = "se"

    table <- data.frame(stat, se)
    rownames(table) <- NULL
  } else {
    table <- table
  }

  return(table)
}

#--------------------------------------------------------------


get_unweighted <- function(table, disenio, var, domains) {

  domains <- create_groupby_vars(domains)

  if (!is.null(domains)) {
    unweighted_cases <- get_sample_size(disenio$variables, c(domains, var) ) %>%
      dplyr::mutate_at(dplyr::vars(all_of(domains)), as.character)  %>%
      dplyr::filter(!!rlang::parse_expr(var) == 1 ) %>%
      dplyr::rename(unweighted = "n")


    unweighted_cases <- table %>%
      dplyr::left_join(unweighted_cases %>% dplyr::select(c(all_of(domains), "unweighted" )),
                       by = domains)

  } else {
    unweighted_cases <- disenio$variables %>%
      dplyr::filter(!!rlang::parse_expr(var) == 1 ) %>%
      nrow()

    unweighted_cases <- table %>%
      dplyr::bind_cols(unweighted = unweighted_cases )

  }

  return(unweighted_cases)

}



#-----------------------------------------------------------------------

get_log_cv <- function(data) {
  data <- data %>%
    dplyr::mutate(log_cv = .data$se / (-log(.data$objetivo)*.data$objetivo))
  return(data)
}



#-----------------------------------------------------------------------


se_message <- function(design) {
  if (as.character(design$call$ids)[[2]] == "1") {
    warning("se calculated without complex design")
  }

}


#-----------------------------------------------------------------------

#' Standardize the name of design variables
#'
#' Rename design variables, so we can use the later
#' @param design \code{dataframe}
#' @return design survey

standardize_design_variables <- function(design) {

  # Cambiar nombre de UPM y estrato solo si el disenio fue declarado con ellas
  if (as.character(design$call$ids)[[2]] != "1") {
    # Create variables only when they dont't already exist
    if (tolower(unificar_variables_upm(design)) != "varunit") {
      design$variables$varunit = design$variables[[unificar_variables_upm(design)]]
    }
    if (tolower(unificar_variables_estrato(design)) != "varstrat") {
      design$variables$varstrat = design$variables[[unificar_variables_estrato(design)]]
    }

    if (tolower(unificar_variables_factExp(design)) != "fe" ) {
      design$variables$fe = design$variables[[unificar_variables_factExp(design)]]

    }
  }

  return(design)

}



#-----------------------------------------------------------------------
filter_design <- function(disenio, subpop) {
  if (!is.null(subpop)) {
    disenio <- disenio[disenio$variables[[subpop]] == 1]
  }
  return(disenio)
}



#-----------------------------------------------------------------------
#' standardize and sort column names
#'
#' Receive the survey table in raw state and sort it
#' @param data \code{dataframe} with results
#' @param var \code{string} with the objective variable
#' @param denom denominator
#' @return \code{dataframe} with standardized data


standardize_columns <- function(data, var, denom) {
  # If there is not denominator, we use a random character
  if (!is.null(denom)) {
    ratio_name <- paste0(var, "/", denom)
  } else {
    ratio_name <- "perro123"
    denom <- "perro123"
  }

  # # when you have objective variable and est
  if(sum(names(data) %in% c(var,"est")) == 2){

    data[var] = NULL

  }

  names(data) <- names(data) %>%
    tolower() %>%
    stringr::str_replace(pattern =  ratio_name, "stat") %>%
    stringr::str_replace(pattern =  tolower(var), "stat") %>%
    stringr::str_remove(pattern =  "\\.stat"  ) %>%
    stringr::str_replace(pattern =  "mean|total|^est", "stat")

  if (!is.null(data$deff) ) {
    data <- data %>%
      dplyr::relocate(deff, .after = last_col())

  }

  # Special case: National level with denominator. This case is different to the case with domains. Survey returns the following structure for ratio and
  # se: denom and denom.1
  if (sum(names(data) %in% c(denom, paste(denom, 1, sep = "."))) == 2 & !is.null(denom)  ) {
    data = data %>%
      dplyr::rename(  "stat" = !!rlang::parse_expr(denom)) %>%
      dplyr::rename(  "se" = !!rlang::parse_expr( paste(denom, 1, sep = ".")))
  }

  rownames(data) = NULL

  return(data)
}


#-----------------------------------------------------------------------

create_output <- function(table, domains, gl, n, cv, env = parent.frame()) {

  domains_original <- get("domains",envir = env)

 # return(domains_original)

  # tabla con desagregación
  if (nrow(data.frame(table)) > 1 | !is.null(domains_original)) {

    final <- table %>%
      dplyr::mutate_at(.vars = dplyr::vars(all_of(domains) ), .funs = as.character) %>%
      dplyr::left_join(gl %>% dplyr::select(c(all_of(domains), "df")),
                       by = domains) %>%
      dplyr::left_join(n %>% dplyr::select(c(all_of(domains), "n")),
                       by = domains) %>%
      dplyr::left_join(cv %>% dplyr::select(c(all_of(domains), "cv")),
                       by = domains)
  } else {

    var <- get("var",envir = env)

    gl <- gl %>% dplyr::select(-dplyr::any_of(var))
    n <- n %>% dplyr::select(-dplyr::any_of(var))

    final <- data.frame(table)

    final <- dplyr::bind_cols(final, "df" = gl , "n" = n, "cv" = cv[1])

## en ocaciones survey te entrega el error estandar con el nombre de la variable objetivo.
    if(names(final)[2] == var){
      names(final)[2] <- "se"
    }

  }

  return(final)
}



#-----------------------------------------------------------------------
#' Get the coefficient of variation
#'
#' Receive a table created with survey and return the coefficient of variation for each cell
#' @param table \code{dataframe} with results
#' @param design design
#' @param domains \code{vector} with domains
#' @param type_est type of estimation: all or size.
#' @param env parent environment
#' @import haven
#' @return \code{dataframe} with results including including CV

get_cv <- function(table, design, domains, type_est = "all", env = parent.frame()) {

  # weird case: national estimations for ine df-approach. In this case there is one element inside domains, but that is
  # because the strategy used before this function
  if (length(domains) == 1 && type_est == "size" && get("df_type", env) == "ine") {
    cv <- cv(table, design = design)

  } else if (!is.null(domains)) { # it considers domains
    cv <- cv(table, design = design)

    cv <- table %>%
      dplyr::select(all_of(domains)) %>%
      dplyr::bind_cols(cv = cv) %>%
      dplyr::mutate_at(.vars = dplyr::vars(all_of(domains) ), .funs = as.character)

  } else { # national level for all kind of estimations
    cv <- cv(table, design = design)
  }



  return(cv)
}


#-----------------------------------------------------------------------

#' Get degrees of freedom
#'
#' Receive data and domains. Returns a data frame with the psu, strata and df for each cell
#' @param data \code{dataframe}
#' @param domains \code{string} with domains
#' @param df_type \code{string} Use degrees of freedom calculation approach from INE Chile or eclac, by default "ine".
#' @return \code{dataframe} with results including degrees of freedom


get_df <- function(data, domains, df_type = "eclac"){
  design <- data
  data <- data$variables

### Si no hay diseño, no se calcula nada
  if (as.character(design$call$ids)[[2]] == "1") {
    gl <- data %>%
      dplyr::group_by_at(.vars  = domains) %>%
      dplyr::summarise(upm = NA,
                       vartstrat = NA,
                       df = NA
      ) %>%
      dplyr::mutate_at(.vars = dplyr::vars(all_of(domains) ), .funs = as.character)

    return(gl)
  }


  if (!is.null(domains)) {

    if(df_type == "ine"){
      #  Get estimation variable for the case size-INE. We need this variable to filter the table in order to exclude zero values
      estimation_var <- domains[length(domains)]

      gl <- calcular_upm(design$variables, domains)  %>%
        dplyr::left_join(calcular_estrato(design$variables, domains), by = domains)  %>%
        dplyr::mutate(df = .data$upm - .data$varstrat)   %>%
        dplyr::filter(!!rlang::parse_expr(estimation_var) == 1)   %>% # the zero cases are deleted
        dplyr::mutate_at(.vars = dplyr::vars(all_of(domains) ), .funs = as.character) %>%
        dplyr::ungroup()   %>%
        dplyr::select(-c("upm","varstrat"))

    } else if(df_type == "eclac"){
      gl <- calcular_upm(design$variables, domains) %>%
        dplyr::left_join(calcular_estrato(design$variables, domains), by = domains) %>%
        dplyr::mutate(df = .data$upm - .data$varstrat)  %>%
        dplyr::mutate_at(.vars = dplyr::vars(all_of(domains) ), .funs = as.character)  %>%
        dplyr::ungroup() %>%
        dplyr::select(-c("upm","varstrat"))
    }

### sin desagregación
  } else {

    if(df_type == "ine"){

      estimation_var <- domains[length(domains)]

    gl <- data %>%
      dplyr::filter(!!rlang::parse_expr(estimation_var) == 1) %>%
      dplyr::summarise(upm = length(unique(.data$varunit)),
                varstrat = length(unique(.data$varstrat)),
                df = .data$upm - .data$varstrat) %>%
      dplyr::select(-c(estimation_var,"upm","varstrat"))

  #  print(paste("fe",gl))

    } else if(df_type == "eclac"){
       # varstrat <- length(unique(design$variables$varstrat))
       # varunit <- length(unique(design$variables$varunit))
       # gl <- varunit - varstrat

     gl <- calcular_estrato(design$variables,domains = NULL) %>%
               dplyr::bind_cols(calcular_upm(design$variables,domains = NULL)) %>%
               dplyr::mutate(df = .data$upm - .data$varstrat) %>%
               dplyr::select(-c("upm","varstrat"))

     return(gl)
    }

  }

  return(gl)

}

#-----------------------------------------------------------------------



create_groupby_vars <- function(domains) {
  if (!is.null(domains)) {
    agrupacion <-  strsplit(domains, split = "\\+")[[1]] %>%
      trimws(which = "both")
  } else {
    agrupacion <- NULL
  }

  return(agrupacion)
}


#-----------------------------------------------------------------------



concat_domains <- function(domains, subpop) {
  domains_form <-  paste(domains, subpop, sep = "+")
  return(domains_form)
}



#-----------------------------------------------------------------------



convert_to_formula <- function(var) {
  if (!is.null(var)) {
    var_form <- paste0("~",var) %>%
      stats::as.formula()

  } else {
   var_form <- NULL
  }
  return(var_form)

}


#-----------------------------------------------------------------------



check_subpop_var <- function(subpop, disenio) {

  if (!is.null(subpop)) {
    es_prop <- disenio$variables %>%
      dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop) == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))

    if (sum(is.na(disenio$variables[[subpop]] > 0 ))) stop("subpop contains NAs!")

    if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("subpop must be a dummy variable!")
  }

}


#-----------------------------------------------------------------------




check_input_var <- function(var, disenio, estimation = "mean") {

  # Chequear que la variable no sea character
  if (is.character(disenio$variables[[var]]) == T) stop("You are using a character vector!")

  #Chequear que la variable sea dummy. Si es una dummy, aparece un warning. Solo aplica para el caso de una estimación de medias
  if (estimation == "mean") {
    es_prop <- disenio$variables %>%
      dplyr::mutate(es_prop = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0 | is.na(!!rlang::parse_expr(var)),
                                             1, 0))

    if (sum(es_prop$es_prop) == nrow(disenio$variables)) warning("It seems yor are using a proportion variable!")

  # En el caso de proporción, se exige que la variable sea dummy
  } else if (estimation == "prop" | estimation == "size" ) {
    es_prop <- disenio$variables %>%
      dplyr::mutate(es_prop = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0 | is.na(!!rlang::parse_expr(var)),
                                             1, 0))

    if (sum(es_prop$es_prop) != nrow(disenio$variables)) stop("It seems yor are not using a dummy variable!")


  }


}



#-----------------------------------------------------------------------



unificar_variables_upm = function(disenio){
  as.character(disenio$call[[names(disenio$call)[grepl("^i",names(disenio$call))]]])[2]

}


#-----------------------------------------------------------------------



### funcion par homologar variables estratos ####
unificar_variables_estrato = function(disenio){
  as.character(disenio$call[[names(disenio$call)[grepl("^s",names(disenio$call))]]])[2]
}

#-----------------------------------------------------------------------



### funcion par homologar variables factor expansion ####
unificar_variables_factExp = function(disenio){
  as.character(disenio$call[[names(disenio$call)[grepl("^w",names(disenio$call))]]])[2]
}

#-----------------------------------------------------------------------

#' Calculates multiple estimations. Internal wrapper for survey package
#'
#' Generates a table with estimates for a given aggregation
#'
#' @param var \code{string} objective variable
#' @param domains \code{domains}
#' @param complex_design design from \code{survey}
#' @param estimation \code{string} indicating if the mean must be calculated
#' @param env \code{environment} parent frame
#' @param fun function required regarding the estimation
#' @param denom denominator. This parameter works for the ratio estimation
#' @param env parent environment
#' @param type_est type of estimation: all or size
#' @return \code{dataframe} containing  main results from survey
#' @import survey

get_survey_table <-  function(var, domains, complex_design, estimation = "mean", env = parent.frame(), fun, denom = NULL, type_est = "all") {


  # El primer if es para domains
  if (!is.null(domains)) {

    if (estimation == "mean") { # para estimaciones de media, proporción y tamaños

      estimacion <-  survey::svyby(formula =  var,
                                   by = domains,
                                   design = complex_design,
                                   FUN = fun,
                                   deff = get("deff", env))


      # This is a patch because a problem with df_type = INE. We needed to add the var to domains in order to get the DF and sample size according to
      # INE approach. This decision  produces an error in the deff calculation. So we implement here a specific procedure for the size function. The idea is
      # to avoid any modification in the rest of the code.
      if (type_est == "size" &&  get("df_type", env) == "ine" ) {

        names(estimacion)[names(estimacion) == formula_to_string(var) ] <- "est"
        estimacion[formula_to_string(var)] <- 1

      }


      # sometimes survey outputs two columns with the same name. In those cases we keep the first occurrence and the second one is modified
      estimacion <- fix_repeated_columns(estimacion, v = var)

      # drop rows with zero values
      string_var <- formula_to_string(var)

      estimacion <- estimacion %>%
        dplyr::filter(!!rlang::parse_expr(string_var)  != 0)


    } else if (estimation == "ratio")  {

      estimacion <- survey::svyby(var, denominator = denom,
                                  design =  complex_design,
                                  by = domains ,
                                  FUN = fun,
                                  deff = get("deff", env))

      return(estimacion)

    } else { # para calcular la mediana

      estimacion <- survey::svyby(var,
                                  by = domains,
                                  FUN = survey::svyquantile,
                                  design = complex_design,
                                  quantiles = 0.5,
                                  method="constant",
                                  interval.type = "quantile",
                                  ties="discrete")
    }
    # Esto corresponde al caso sin desagregacion
  } else {
    if (estimation == "mean") { # para calcular la media

        estimacion <- fun(var, complex_design, deff = get("deff", env))

    } else if (estimation == "ratio") {

        estimacion <- survey::svyratio(var, denominator = denom, design = complex_design, deff = get("deff", env))


    } else { # para calcular la mediana

      estimacion <-  svyquantile(var,
                                 design = complex_design,
                                 quantiles = 0.5,
                                 method="constant",
                                 interval.type = "quantile",
                                 ties="discrete")
    }

  }

  return(estimacion)
}

#-----------------------------------------------------------------------


calcular_tabla_ratio <-  function(var,denominator, domains = NULL, complex_design, env = parent.frame()) {
  if (!is.null(domains)) {
    estimacion <- survey::svyby(var, denominator = denominator,design =  complex_design, by = domains , FUN = svyratio, deff = get("deff", env))
  } else {
    estimacion <- survey::svyratio(var, denominator = denominator, design = complex_design, deff = get("deff", env))
  }
  return(estimacion)
}


#-----------------------------------------------------------------------



get_sample_size <- function(data, domains = NULL, df_type = "eclac", env = parent.frame()) {

  if(df_type == "ine"){

    #  Get estimation variable for the case size-INE. We need this variable to filter the table in order to exclude zero values
    estimation_var <- domains[length(domains)]

    data %>%
      dplyr::group_by_at(.vars = domains) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::filter(!!rlang::parse_expr(estimation_var) == 1) %>%
      dplyr::mutate_at(domains, as.character)

  }else if(df_type == "eclac"){

    data %>%
      dplyr::group_by_at(.vars = domains) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::mutate_at(domains, as.character)

  }

}

# -----------------------------------------------------------------------

unweighted_cases <- function(data, domains, var) {
  symbol_var <- rlang::parse_expr(var)

  data %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::group_by_at(.vars = domains) %>%
    dplyr::summarise(n = dplyr::n())
}

#----------------------------------------------------------------------



chequear_var_disenio <- function(data) {

  if (sum(grepl(pattern = "varunit" , x = names(data))) == 0) {
    stop("La columna que contiene informacion de las UPMs debe llamarse varunit!")
  }

  if (sum(grepl(pattern = "varstrat" , x = names(data))) == 0) {
    stop("La columna que contiene informacion de los estratos debe llamarse varstrat!")
  }

}


#-----------------------------------------------------------------------


calcular_upm <- function(data, domains, var = NULL ) {
    listado <- c("varunit", domains)
    if (is.null(var)) {
      data %>%
        dplyr::group_by_at(.vars = listado) %>%
        dplyr::summarise(conteo = dplyr::n()) %>%
        dplyr::mutate(tiene_info = dplyr::if_else(.data$conteo > 0, 1, 0))  %>%
        dplyr::group_by_at(.vars = domains) %>%
        dplyr::summarise(upm = sum(.data$tiene_info))
    } else {
      symbol_var <- rlang::parse_expr(var)
      data %>%
        dplyr::mutate(!!symbol_var := as.numeric(!!symbol_var)) %>%
        dplyr::group_by_at(.vars = listado) %>%
        dplyr::summarise(conteo = sum(!!symbol_var)) %>%
        dplyr::mutate(tiene_info = dplyr::if_else(.data$conteo > 0, 1, 0))  %>%
        dplyr::group_by_at(.vars = domains) %>%
        dplyr::summarise(upm = sum(.data$tiene_info))
    }


}
#-----------------------------------------------------------------------


calcular_estrato <- function(data, domains, var = NULL ) {


  listado <- c("varstrat", domains)
  if (is.null(var)) {
    data %>%
      dplyr::group_by_at(.vars = listado) %>%
      dplyr::summarise(conteo = dplyr::n()) %>%
      dplyr::mutate(tiene_info = dplyr::if_else(.data$conteo > 0, 1, 0)) %>%
      dplyr::group_by_at(.vars = domains) %>%
      dplyr::summarise(varstrat = sum(.data$tiene_info))
  } else {
    symbol_var <- rlang::parse_expr(var)
    data %>%
      dplyr::mutate(!!symbol_var := as.numeric(!!symbol_var)) %>%
      dplyr::group_by_at(.vars = listado) %>%
      dplyr::summarise(conteo = sum(!!symbol_var)) %>%
      dplyr::mutate(tiene_info = dplyr::if_else(.data$conteo > 0, 1, 0)) %>%
      dplyr::group_by_at(.vars = domains) %>%
      dplyr::summarise(varstrat = sum(.data$tiene_info))
  }
}

#----------------------------------------------------------------------------


# deprecated

calcular_gl_total <- function(variables, datos) {
  upm <- purrr::map(variables, ~calcular_upm(datos, .x) %>%
                      dplyr::rename(variable := dplyr::all_of(.x) ) %>%
                      dplyr::mutate(variable = paste0(.x, variable))) %>%
    purrr::reduce(dplyr::bind_rows)

  estratos <- purrr::map(variables, ~calcular_estrato(datos, .x) %>%
                           dplyr::rename(variable := dplyr::all_of(.x) ) %>%
                           dplyr::mutate(variable = paste0(.x, variable))) %>%
    purrr::reduce(dplyr::bind_rows)

  gl <- upm %>%
    dplyr::left_join(estratos, by = "variable") %>%
    dplyr::mutate(gl = .data$upm - .data$varstrat)
  return(gl)

}



#------------------------------




get_ci <-  function(data,  ajuste_ene) {

  # Se calculan los intervalos de la manera tradicional en la generalidad de los casos
  if (ajuste_ene == FALSE) {

    final <- data %>%
      dplyr::mutate(t = stats::qt(c(.975), df = .data$df),
                    lower = .data$stat - .data$se*t,
                    upper = .data$stat + .data$se*t)
    # Estos corresponde al ajuste de la ENE: el t se fija en 2
  } else if (ajuste_ene == TRUE) {

    final <- data %>%
      dplyr::mutate(t = 2,
                    lower = .data$stat - .data$se*t,
                    upper = .data$stat + .data$se*t)
  }

  return(final)
}

#---------------------------------------------------------------------
# Obtain deff

get_deff <- function(var, design, survey_est) {

  df_survey <- as.data.frame(survey_est)
  N <- sum(design$variables$fe)
  n <- nrow(design$variables)
  B <- as.numeric(df_survey[1])

  complex_variance <- df_survey[, 2] ** 2

  random_variance <- (1/N) * (1 / (n - 1)) * (1 - n/N) * sum(design$variables$fe * (design$variables[[var]] - B) ^ 2)

  return(list(complex_variance, B, var))
  deff <- complex_variance / random_variance

}





#----------------------------------------------------------------------

convert_to_integer <- function(domains, disenio) {
  # Evaluar si alguna de las variables de dominio es un factor
  variables_dominio <- stringr::str_split(domains, pattern = "\\+")[[1]] %>%
    trimws(which = "both")
  es_factor <- purrr::map(variables_dominio, ~is.factor(disenio$variables[[.x]]))

  # Si existe una variable factor, se convierten todas a integer
  if (sum(es_factor > 0)) {
    disenio$variables <- disenio$variables %>%
      dplyr::mutate_at(dplyr::vars(variables_dominio), as.integer)

    warning("labels removed!")

  }

  return(disenio)
}

#-----------------------------------------------------------------
#
get_ess <- function(ess, env = parent.frame() ) {

  final <- get("final", env)
  deff <- get("deff", env)

  if (ess == TRUE) {

    if (deff == FALSE) {
      warning("to get effective sample size use deff = T")
    } else {
      final <- final %>%
        dplyr::mutate(ess = .data$n / deff)
    }
  }
  return(final)
}


#--------------------------------------------------------------------




#' internal function to calculate ratios estimations
#'
#' @param var numeric variable within the \code{dataframe}, is the numerator of the ratio to be calculated.
#' @param denominator numeric variable within the \code{dataframe}, is the denominator of the ratio to be calculated.
#' @param domains domains to be estimated separated by the + character.
#' @param disenio complex design created by \code{survey} package
#' @param subpop integer dummy variable to filter the dataframe
#' @param ci \code{boolean} indicating if the confidence intervals must be calculated
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @param deff \code{boolean} Design effect
#' @param ess \code{boolean} Effective sample size
#' @param rel_error \code{boolean} Relative error
#' @param rm.na \code{boolean} indicating if NA values must be removed
#' @param unweighted \code{boolean} Add non weighted count if it is required
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'

create_ratio_internal <- function(var,denominator, domains = NULL, subpop = NULL, disenio, ci = FALSE, deff = FALSE, ess = FALSE,
                                  ajuste_ene = FALSE, unweighted = FALSE, rel_error = FALSE, rm.na = FALSE) {

  # get design variables
  design_vars <- get_design_vars(disenio )

  # Crear listado de variables que se usan en los dominios
  agrupacion <- create_groupby_vars(domains)

  # Select relevant columns
  disenio <- disenio[ ,  c(agrupacion, var, subpop, design_vars, denominator)]

  # Chequear que la variable objetivo y la variable subpop cumplan con ciertas condiciones
  check_input_var(var, disenio, estimation = "ratio")
  check_subpop_var(subpop, disenio)

  # Lanzar warning del error estándar cuando no se usa el diseño
  se_message(disenio)

  # Homologar nombres de variables  del diseño
  disenio <- standardize_design_variables(disenio)

  # Convertir everything tolower to avoid problems
  names(disenio$variables) <- tolower(names(disenio$variables))
  lower_params <- purrr::map(list("var" = var, "subpop" = subpop, "domains" = domains, "denominator" = denominator ), tolower_strings)

  var <- lower_params$var
  subpop <- lower_params$subpop
  domains <- lower_params$domains
  denominator <- lower_params$denominator

  # Sacar los NA si el usuario lo requiere
  if (rm.na == TRUE) {
    disenio <- disenio[!is.na(disenio$variables[[var]])]
  }

  # Filtrar diseño, si el usuario agrega el parámetro subpop
  disenio <- filter_design(disenio, subpop)

  #Convertir los inputs en formulas para adecuarlos a survey
  var_form <- convert_to_formula(var)
  domains_form <- convert_to_formula(domains)
  denominator_form <- convert_to_formula(denominator)

  tabla <- get_survey_table(var_form, domains_form, disenio, fun = survey::svyratio, estimation = "ratio", denom = denominator_form)

  # Crear listado de variables que se usan para el cálculo
  agrupacion <- create_groupby_vars(domains)

  #Calcular el tamanio muestral de cada grupo
  n <- get_sample_size(disenio$variables, agrupacion)


  #Calcular los grados de libertad de todos los cruces
  gl <- get_df(disenio, agrupacion)

  #Extrear el coeficiente de variacion
  cv <- get_cv(tabla, disenio, agrupacion)


  # Convert to df if there is not any domain
  tabla <- convert_ratio_to_df(tabla, domains)


  #Unir toda la informacion en una tabla final
  final <- create_output(tabla, agrupacion,  gl, n, cv)

  # Ordenar las columnas y estandarizar los nombres de las variables
  final <- standardize_columns(final, var, denominator )



  # Add unweighted counting
  if (unweighted) {
    final <- get_unweighted(final, disenio, var, agrupacion)
  }

  # Add confidence intervals
  if (ci == TRUE) {
    final <- get_ci(final,  ajuste_ene = ajuste_ene)
  }

  # add relative error, if the user uses this parameter
  if (rel_error == TRUE) {
    final <- final %>%
      dplyr::mutate(relative_error = stats::qt(c(.975), df = .data$df) * cv)
  }

  # add the ess if the user uses this parameter
  final <- get_ess(ess)

  return(final)

}

#-----------------------------------------------------------------------


#' internal function to calculate proportion estimations
#'
#' @param var integer dummy variable within the  \code{dataframe}
#' @param domains domains to be estimated separated by the + character.
#' @param subpop integer dummy variable to filter the dataframe
#' @param disenio complex design created by \code{survey} package
#' @param ci \code{boolean} indicating if the confidence intervals must be calculated
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @param standard_eval \code{boolean} indicating if the function is inside another function, by default it is TRUE, avoid problems with lazy eval.
#' @param deff \code{boolean} Design effect
#' @param rm.na \code{boolean} indicating if NA values must be removed
#' @param env parent environment to get some variables
#' @param log_cv \code{boolean} indicating if the log cv must be returned
#' @param ess \code{boolean} Effective sample size
#' @param rel_error \code{boolean} Relative error
#' @param unweighted \code{boolean} Add non weighted count if it is required
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'

create_prop_internal <- function(var, domains = NULL, subpop = NULL, disenio, ci = FALSE, deff = FALSE, ess = FALSE, ajuste_ene = FALSE,
                                 rel_error = FALSE, log_cv = FALSE, unweighted = FALSE, standard_eval = TRUE, rm.na = FALSE, env =  parent.frame()) {


  # get design variables
  design_vars <- get_design_vars(disenio )

  # Crear listado de variables que se usan en los dominios
  agrupacion <- create_groupby_vars(domains)

  # Select relevant columns
  disenio <- disenio[ ,  c(agrupacion,var, subpop, design_vars  ) ]

  # Chequear que la variable objetivo y la variable subpop cumplan con ciertas condiciones
  check_input_var(var, disenio, estimation = "prop")
  check_subpop_var(subpop, disenio)

  # Lanzar warning del error estándar cuando no se usa el diseño
  se_message(disenio)

  # Homologar nombres de variables  del diseño
  disenio <- standardize_design_variables(disenio)


  # Convertir everithing tolower to avoid problems
  names(disenio$variables) <- tolower(names(disenio$variables))
  lower_params <- purrr::map(list("var" = var, "subpop" = subpop, "domains" = domains ),  tolower_strings )
  var <- lower_params$var
  subpop <- lower_params$subpop
  domains <- lower_params$domains


  # Sacar los NA si el usuario lo requiere
  if (rm.na == TRUE) {
    disenio <- disenio[!is.na(disenio$variables[[var]])]
  }

  # Filtrar diseño, si el usuario agrega el parámetro subpop
  disenio <- filter_design(disenio, subpop)

  #Convertir los inputs en formulas para adecuarlos a survey
  var_form <- convert_to_formula(var)

  # Convertir en formula para survey
  domains_form <- convert_to_formula(domains)

  tabla <- get_survey_table(var_form, domains_form, disenio, fun = survey::svymean)

  # Crear listado de variables que se usan en los dominios
  agrupacion <- create_groupby_vars(domains)

  #Calcular el tamanio muestral de cada grupo
  n <- get_sample_size(disenio$variables, agrupacion)

  #Calcular los grados de libertad de todos los cruces
  gl <- get_df(disenio, agrupacion)

  #Extrear el coeficiente de variacion
  cv <- get_cv(tabla, disenio, agrupacion)

  #Unir toda la informacion en una tabla final
  final <- create_output(tabla, agrupacion,  gl, n, cv)

  # Ordenar las columnas y estandarizar los nombres de las variables
  final <- standardize_columns(final, var, denom = get("denominator", env) )


  # Add unweighted counting
  if (unweighted) {
    final <- get_unweighted(final, disenio, var, agrupacion)
  }

  # Add confidence intervals
  if (ci == TRUE) {
    final <- get_ci(final,  ajuste_ene = ajuste_ene)
  }


  # add relative error, if the user uses this parameter
  if (rel_error == TRUE) {
    final <- final %>%
      dplyr::mutate(relative_error = stats::qt(c(.975), df = .data$df) * cv)
  }


  # add log cv, if the user uses this parameter
  if (log_cv) {
    final <- final %>%
      dplyr::mutate(log_cv = .data$se / (-log(.data$stat)*.data$stat))
  }

  # add the ess if the user uses this parameter
  final <- get_ess(ess)

  return(final)




}



