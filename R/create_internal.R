


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
    table <- data.frame(table[1], table[2])
    rownames(table) <- NULL
  } else {
    table <- table
  }

  return(table)
}

#--------------------------------------------------------------

get_unweighted <- function(table, disenio, var, domains) {

  if (!is.null(domains)) {
    unweighted_cases <- get_sample_size(disenio$variables, c(domains, var) ) %>%
      dplyr::mutate_at(dplyr::vars(domains), as.character)  %>%
      dplyr::filter(!!rlang::parse_expr(var) == 1 ) %>%
      dplyr::rename(unweighted = .data$n)


    unweighted_cases <- table %>%
      dplyr::left_join(unweighted_cases %>% dplyr::select(c(domains, "unweighted" )),
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

#' Homologa el nombre de las variables disenio
#'
#' Cambia el nombre de las variables de disenio, para poder utilizarlas más adelante
#' @param design dataframe con los resultados
#' @return disenio con los nombres homologados

standardize_design_variables <- function(design) {

  # Cambiar nombre de UPM y estrato solo si el disenio fue declarado con ellas
  if (as.character(design$call$ids)[[2]] != "1") {
    design$variables$varunit = design$variables[[unificar_variables_upm(design)]]
    design$variables$varstrat = design$variables[[unificar_variables_estrato(design)]]
  }

  design$variables$fe = design$variables[[unificar_variables_factExp(design)]]

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
#' Ordena nombre de columnas y estandariza el orden
#'
#' Recibe la tabla en estado bruto y la ordena
#' @param data dataframe con los resultados
#' @param var variable objetivo
#' @param denom denominator
#' @return dataframe con todos los datos ordenados


standardize_columns <- function(data, var, denom) {

  # If there is not denominator, we use a random character
  if (!is.null(denom)) {
    ratio_name <- paste0(var, "/", denom)
  } else {
    ratio_name <- "perro"
    denom <- "perro"
  }

  # print(names(data))
  # print(var)

  # # when you have objective variable and est
  if(sum(names(data) %in% c(var,"est")) == 2){

    data[var] = NULL

  }

  names(data) <- names(data) %>%
    tolower() %>%
    stringr::str_replace(pattern =  ratio_name, "stat") %>%
    stringr::str_replace(pattern =  tolower(var), "stat") %>%
    stringr::str_remove(pattern =  "\\.stat"  ) %>%
    stringr::str_replace(pattern =  "mean|total|est", "stat")

  if (!is.null(data$deff) ) {
    data <- data %>%
      dplyr::relocate(deff, .after = last_col())

  }

  rownames(data) = NULL

  return(data)
}


#-----------------------------------------------------------------------

create_output <- function(table, domains, gl, n, cv, env = parent.frame()) {

  # tabla con desagregación
  if (nrow(data.frame(table)) > 1 ) {

    final <- table %>%
      dplyr::mutate_at(.vars = dplyr::vars(domains), .funs = as.character) %>%
      dplyr::left_join(gl %>% dplyr::select(c(domains, "df")),
                       by = domains) %>%
      dplyr::left_join(n %>% dplyr::select(c(domains, "n")),
                       by = domains) %>%
      dplyr::left_join(cv %>% dplyr::select(c(domains, "cv")),
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
#' Calcula el coeficiente de variación
#'
#' Recibe una tabla creada con survey y devuelve el coeficiente de variación para cada celda
#' @param table objeto creado con survey
#' @param design diseño complejo creado con survey
#' @param domains listado de variables para desagregar
#' @import haven
#' @return dataframe con la información de cv

get_cv <- function(table, design, domains) {

  if (!is.null(domains)) {
    cv <- cv(table, design = design)

    cv <- table %>%
      dplyr::select(domains) %>%
      dplyr::bind_cols(cv = cv) %>%
      dplyr::mutate_at(.vars = dplyr::vars(domains), .funs = as.character)

  } else {
    cv <- cv(table, design = design)
  }

  return(cv)
}


#-----------------------------------------------------------------------

#' Cálcula los grados de libertad para cada estimación
#'
#' Recibe datos y los domains. Devuelve un data frame con las upm, varstrat y gl para cada celda
#' @param data dataframe
#' @param domains \code{string} with domains
#' @param df_type \code{string} Use degrees of freedom calculation approach from INE Chile or CEPAL, by default "ine".
#' @return dataframe con grados de libertad


get_df <- function(data, domains,df_type = "cepal"){
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
      dplyr::mutate_at(.vars = dplyr::vars(domains), .funs = as.character)

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
        dplyr::mutate_at(.vars = dplyr::vars(domains), .funs = as.character) %>%
        dplyr::ungroup()   %>%
        dplyr::select(-c("upm","varstrat"))

    } else if(df_type == "cepal"){
      gl <- calcular_upm(design$variables, domains) %>%
        dplyr::left_join(calcular_estrato(design$variables, domains), by = domains) %>%
        dplyr::mutate(df = .data$upm - .data$varstrat)  %>%
        dplyr::mutate_at(.vars = dplyr::vars(domains), .funs = as.character)  %>%
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

    } else if(df_type == "cepal"){
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

#' Concatena los domains y la subpoblación con signo +
#'
#' Recibe strings con domains y subpoblación y devuelve un string concatenado con caracter +
#'
#' @param domains domains en formato string
#'
#' @return listado de variables en formato string


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

#' Concatena los domains y la subpoblación con signo +
#'
#' Recibe strings con domains y subpoblación y devuelve un string concatenado con caracter +
#'
#' @param domains domains en formato string
#' @param subpop subpoblación ingresada por el usuario en formato string
#'
#' @return string concatenado de domains y subpoblación

concat_domains <- function(domains, subpop) {
  domains_form <-  paste(domains, subpop, sep = "+")
  return(domains_form)
}



#-----------------------------------------------------------------------

#' Convierte un string en una fórmula
#'
#' Recibe un string y lo convierte en un formato de fórmula
#'
#' @param var sting con el nombre de la variable
#'
#' @return variable en formato fórmula


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

#' Evalúa algunos requisitos básicos de la variable de subpop
#'
#' Evalúa si la variable es dummy
#'
#' @param subpop string of the subpopulation filter
#' @param disenio complex design
#'
#' @return warning or stop


check_subpop_var <- function(subpop, disenio) {

  if (!is.null(subpop)) {
    es_prop <- disenio$variables %>%
      dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop) == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))

    if (sum(is.na(disenio$variables[[subpop]] > 0 ))) stop("subpop contains NAs!")

    if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("subpop must be a dummy variable!")
  }

}


#-----------------------------------------------------------------------

#' Evalúa algunos requisitos básicos de la variable objetivo
#'
#' Evalúa si la variable es caracter y si es una variable de proporción en caso de que la estimación sea de media
#'
#' @param var string of the objetive variable
#' @param disenio complex design
#' @param estimation type of estimation
#'
#' @return warning or stop



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

#' Homologa nombre de variable que hace referencia a los conglomerados, con el objetivo de evitar posible errores.
#'
#' Identifica el nombre de la variable asignada para los conglomerados en el disenio complejo, lo que permite reasignar variable con nombre estandar utilizado por las 4 funciones de creacion de insumos.
#'
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#'
#' @return \code{vector} que contiene la variable con los conglomerados.
#' @import survey


unificar_variables_upm = function(disenio){
  as.character(disenio$call[[names(disenio$call)[grepl("^i",names(disenio$call))]]])[2]

}

# agregar comentarios

#-----------------------------------------------------------------------

#' Homologa nombre de variable que hace referencia a los estratos de conglomerados, con el objetivo de evitar posible errores.
#'
#' Identifica el nombre de la variable asignada para los estratos de conglomerados en el disenio complejo, lo que permite reasignar variable con nombre estandar utilizado por las 4 funciones de creacion de insumos.
#'
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#'
#' @return \code{vector} que contiene la variable con los estratos de conglomerados.
#' @import survey

### funcion par homologar variables estratos ####
unificar_variables_estrato = function(disenio){
  as.character(disenio$call[[names(disenio$call)[grepl("^s",names(disenio$call))]]])[2]
}

#-----------------------------------------------------------------------

#' Homologa nombre de variable que hace referencia al factor de expansion utilizado por el usuario, con el objetivo de evitar posible errores.
#'
#' Identifica el nombre de la variable asignada para el factor de expansion en el disenio complejo, lo que permite reasignar variable con nombre estandar utilizado por las 4 funciones de creacion de insumos.
#'
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#'
#' @return \code{vector} que contiene la variable con los datos del factor de expansion.
#' @import survey

### funcion par homologar variables factor expansion ####
unificar_variables_factExp = function(disenio){
  as.character(disenio$call[[names(disenio$call)[grepl("^w",names(disenio$call))]]])[2]
}

#-----------------------------------------------------------------------

#' Calculates multiple estimations. Internal wrapper for survey package
#'
#' Genera una tabla con estimaciones para una agregacion determinada
#'
#' @param var variable objetivo dentro de un \code{dataframe}. Debe anteponerse ~
#' @param domains domains de estimacion separados por signo +. Debe anteponerse ~
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#' @param estimation \code{string} indicating if the mean must be calculated
#' @param env \code{environment} toma el ambiente de la funcion contenedora, para usar los elementos requeridos
#' @param fun Function required regarding the estimation
#' @param denom denominator. This parameter works for the ratio estimation
#' @return \code{dataframe} que contiene variables de agregacion, variable objetivo y error estandar
#' @import survey

calcular_tabla <-  function(var, domains, disenio, estimation = "mean", env = parent.frame(), fun, denom = NULL) {


  # El primer if es para domains
  if (!is.null(domains)) {

    if (estimation == "mean") { # para estimaciones de media, proporción y tamaños

      estimacion <-  survey::svyby(formula = var,
                                   by = domains,
                                   design = disenio,
                                   FUN = fun,
                                   deff = get("deff", env))
      # sometimes survey outputs two coluns with the same name. In those cases we keep the first occurrence and the second one is modified
      estimacion <- fix_repeated_columns(estimacion, v = var)

      # drop rows with zero values
      string_var <- formula_to_string(var)

      estimacion <- estimacion %>%
        dplyr::filter(!!rlang::parse_expr(string_var)  != 0)


    } else if (estimation == "ratio")  {

      estimacion <- survey::svyby(var, denominator = denom,
                                  design =  disenio,
                                  by = domains ,
                                  FUN = fun,
                                  deff = get("deff", env))

      return(estimacion)

    } else { # para calcular la mediana

      estimacion <- survey::svyby(var,
                                  by = domains,
                                  FUN = survey::svyquantile,
                                  design = disenio,
                                  quantiles = 0.5,
                                  method="constant",
                                  interval.type = "quantile",
                                  ties="discrete")
    }
    # Esto corresponde al caso sin desagregacion
  } else {
    if (estimation == "mean") { # para calcular la media

        estimacion <- fun(var, disenio, deff = get("deff", env))

    } else if (estimation == "ratio") {

        estimacion <- survey::svyratio(var, denominator = denom, design = disenio, deff = get("deff", env))


    } else { # para calcular la mediana

      estimacion <-  svyquantile(var,
                                 design = disenio,
                                 quantiles = 0.5,
                                 method="constant",
                                 interval.type = "quantile",
                                 ties="discrete")
    }

  }

  return(estimacion)
}

#-----------------------------------------------------------------------

#' Calcula ratio a partir de cierta agregacion
#'
#' Genera una tabla con estimaciones para una agregacion determinada
#'
#' @param var variable objetivo o numerador del ratio a calcular, dentro de un \code{dataframe}. Debe anteponerse ~
#' @param denominador variable denominador del ratio a calcular, dentro de un \code{dataframe}. Debe anteponerse ~
#' @param domains domains de estimacion separados por signo +. Debe anteponerse ~
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#' @param env \code{environment} toma el ambiente de la funcion contenedora, para usar los elementos requeridos
#' @return \code{dataframe} que contiene variables de agregacion, variable objetivo y error estandar
#' @import survey

calcular_tabla_ratio <-  function(var,denominador, domains = NULL, disenio, env = parent.frame()) {
  if (!is.null(domains)) {
    estimacion <- survey::svyby(var, denominator = denominador,design =  disenio, by = domains , FUN = svyratio, deff = get("deff", env))
  } else {
    estimacion <- survey::svyratio(var, denominator = denominador, design = disenio, deff = get("deff", env))
  }
  return(estimacion)
}


#-----------------------------------------------------------------------

#' Calcula tamanio muestral para las medias
#'
#' Genera una tabla con el conteo de cada cada una de los domains del tabulado.
#' La funcion contempla un caso para proporcion y un caso para promedio
#' @param data \code{dataframe} que contiene los datos que se estan evaluando
#' @param domains vector de caracteres que contiene los domains a evaluar
#' @param df_type \code{string} Use degrees of freedom calculation approach from INE Chile or CEPAL, by default "ine".
#' @param env parent environment
#' @return \code{dataframe} que contiene la frecuencia de todos los domains a evaluar

get_sample_size <- function(data, domains = NULL, df_type = "cepal", env = parent.frame()) {

  if(df_type == "ine"){

    #  Get estimation variable for the case size-INE. We need this variable to filter the table in order to exclude zero values
    estimation_var <- domains[length(domains)]

    data %>%
      dplyr::group_by_at(.vars = domains) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::filter(!!rlang::parse_expr(estimation_var) == 1) %>%
      dplyr::mutate_at(domains, as.character)

  }else if(df_type == "cepal"){

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
#' Chequea que las variables de disenio tengan el nombre correcto
#'
#' Comprueba que las variables de disenio se llamen varstrat y varunit. En caso de que no se cumpla, la ejecucion se detiene y se genera un error
#'
#' @param data \code{dataframe} que contiene la tabla con la cual se esta trabajando
#' @return un mensaje de error
#'


chequear_var_disenio <- function(data) {

  if (sum(grepl(pattern = "varunit" , x = names(data))) == 0) {
    stop("La columna que contiene informacion de las UPMs debe llamarse varunit!")
  }

  if (sum(grepl(pattern = "varstrat" , x = names(data))) == 0) {
    stop("La columna que contiene informacion de los estratos debe llamarse varstrat!")
  }

}


#-----------------------------------------------------------------------

#' Calcula el numero de UPM
#'
#' Genera una tabla con el conteo de UPM para cada uno de los domains del tabulado.
#' La columna que contiene la informacion de las UPMs debe llamarse varunit
#' La funcion contempla un caso para proporcion y un caso para promedio
#'
#' @param data \code{dataframe} que contiene los datos que se estan evaluando
#' @param domains vector de caracteres que contiene los domains a evaluar
#' @param var string que contiene el nombre de la variable de proporcion que se evalua.
#' @return \code{dataframe} que contiene la frecuencia de todos los domains a evaluar
#'

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

#' Calcula el numero de estratos
#'
#' Genera una tabla con el conteo de estratos para cada uno de los domains del tabulado.
#' La columna que contiene la informacion de los estratos debe llamarse varstrat
#' La funcion contempla un caso para proporcion y un caso para promedio

#' @importFrom rlang .data
#' @importFrom rlang  :=
#' @param data \code{dataframe} que contiene los datos que se estan evaluando
#' @param var variable objetivo. Debe ser un integer que toma los valores 1 o 0
#' @param domains vector de caracteres que contiene los domains a evaluar
#' @return \code{dataframe} que contiene la frecuencia de todos los domains a evaluar

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

#' Calcula los grados de libertad para un estimaciones de total
#'
#' Genera una tabla con el conteo de grados de libertad para cada uno de los domains del tabulado. Es un wrapper que reune a las funciones calcular_upm y calcular_estrato
#'
#' @param datos \code{dataframe} que contiene los datos que se estan evaluando. Se obtiene a partir del disenio muestral
#' @param variables variables objetivo. vector de strings que contiene los nombres de las variables
#' @return \code{dataframe} que contiene la frecuencia de todos los domains a evaluar

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

#' Genera intervalos de confianza para todos los domains estimados
#'
#' Usa la tabla creada para calcular el estandar y le agrega dos columnas con el limite inferior y superior del intervalo de confianza
#'
#' @param data \code{dataframe} con todos los datos necesarios para calcular el estandar
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @return \code{dataframe} que contiene todos los elementos del estandar, junto a tres columnas nuevas que contienen el limite inferior, el limite superior y el valor t
#'


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
#' @param denominador numeric variable within the \code{dataframe}, is the denominator of the ratio to be calculated.
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
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'

create_ratio_internal <- function(var,denominador, domains = NULL, subpop = NULL, disenio, ci = FALSE, deff = FALSE, ess = FALSE,
                                  ajuste_ene = FALSE, unweighted = FALSE, rel_error = FALSE, rm.na = FALSE) {


  # Chequear que la variable objetivo y la variable subpop cumplan con ciertas condiciones
  check_input_var(var, disenio, estimation = "ratio")
  check_subpop_var(subpop, disenio)

  # Lanzar warning del error estándar cuando no se usa el diseño
  se_message(disenio)


  # Homologar nombres de variables  del diseño
  disenio <- standardize_design_variables(disenio)

  # Sacar los NA si el usuario lo requiere
  if (rm.na == TRUE) {
    disenio <- disenio[!is.na(disenio$variables[[var]])]
  }

  # Filtrar diseño, si el usuario agrega el parámetro subpop
  disenio <- filter_design(disenio, subpop)

  #Convertir los inputs en formulas para adecuarlos a survey
  var_form <- convert_to_formula(var)
  domains_form <- convert_to_formula(domains)
  denominador_form <- convert_to_formula(denominador)

  tabla <- calcular_tabla(var_form, domains_form, disenio, fun = survey::svyratio, estimation = "ratio", denom = denominador_form)

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
  final <- standardize_columns(final, var, denominador )

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

  # Chequear que la variable objetivo y la variable subpop cumplan con ciertas condiciones
  check_input_var(var, disenio, estimation = "prop")
  check_subpop_var(subpop, disenio)

  # Lanzar warning del error estándar cuando no se usa el diseño
  se_message(disenio)


  # Homologar nombres de variables  del diseño
  disenio <- standardize_design_variables(disenio)

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

  tabla <- calcular_tabla(var_form, domains_form, disenio, fun = survey::svymean)

  # Crear listado de variables que se usan para el cálculo
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
  final <- standardize_columns(final, var, denom = get("denominador", env) )

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



