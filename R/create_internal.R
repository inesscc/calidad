

add_class <-  function(object, new_class) {
  class(object)  <- append(class(object), new_class, after = F)
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
    unweighted_cases <- get_sample_size(var,disenio$variables, c(domains, var) ) %>%
      dplyr::mutate_at(dplyr::vars(domains), as.character)  %>%
      dplyr::filter(!!rlang::parse_expr(var) == 1 ) %>%
      dplyr::rename(unweighted = n)


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
    dplyr::mutate(log_cv = se / (-log(objetivo)*objetivo))
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
#' @return dataframe con todos los datos ordenados


standardize_columns <- function(data, var, denom = denominador) {

  # If there is not denominator, we use a random character
  if (!is.null(denom)) {
    ratio_name <- paste0(var, "/", denom)
  } else {
    ratio_name <- "perro"
    denom <- "perro"
  }


  names(data) <- names(data) %>%
    tolower() %>%
    stringr::str_replace(pattern =  ratio_name, "stat") %>%
    stringr::str_replace(pattern =  var, "stat") %>%
    stringr::str_remove(pattern =  "\\.stat"  ) %>%
    stringr::str_replace(pattern =  "mean", "stat")

  if (!is.null(data$deff) ) {
    data <- data %>%
      dplyr::relocate(deff, .after = last_col())

  }

  return(data)
}


#-----------------------------------------------------------------------
#' Une información de indicadores y genera tabla final
#'
#' Recibe los indicadores de calidad calculados previamente y los une en una tabla
#' @param table objeto creado con survey
#' @param domains listado de variables para desagregar
#' @return dataframe con toda la información para estándar INE

create_output <- function(table, domains, gl, n, cv) {

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
    final <- data.frame(table)

    final <- dplyr::bind_cols(final, "df" = gl , "n" = n, "cv" = cv[1])
    names(final)[2] <- "se"
    return(final)

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
#' Recibe datos y los dominios. Devuelve un data frame con las upm, varstrat y gl para cada celda
#' @param var variable objetivo
#' @param data dataframe
#' @param domains dominios en formato string
#' @df_type \code{string} Use degrees of freedom calculation approach from INE Chile or CEPAL, by default "ine".
#' @return dataframe con grados de libertad


get_df <- function(var,data, domains,df_type = "cepal"){
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

    domains_ine <- c(var,domains)

    if(df_type == "ine"){

      gl <- calcular_upm(design$variables, domains_ine) %>%
        dplyr::left_join(calcular_estrato(design$variables, domains_ine), by = domains_ine) %>%
        dplyr::mutate(df = .data$upm - .data$varstrat)  %>%
        dplyr::filter(!!rlang::parse_expr(var) == 1)  %>%
        dplyr::mutate_at(.vars = dplyr::vars(domains_ine), .funs = as.character) %>%
        dplyr::ungroup() %>%
        dplyr::select(-c(var,"upm","varstrat"))

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

    gl <- data %>%
      dplyr::group_by(!!rlang::parse_expr(var)) %>%
      dplyr::summarise(upm = length(unique(varunit)),
                varstrat = length(unique(varstrat)),
                df = upm-varstrat) %>%
      dplyr::filter(!!rlang::parse_expr(var) == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(var,"upm","varstrat"))

    } else if(df_type == "cepal"){

       varstrat <- length(unique(design$variables$varstrat))
       varunit <- length(unique(design$variables$varunit))
       gl <- varunit - varstrat
    }

  }

  return(gl)

}

#-----------------------------------------------------------------------

#' Concatena los dominios y la subpoblación con signo +
#'
#' Recibe strings con dominios y subpoblación y devuelve un string concatenado con caracter +
#'
#' @param domains dominios en formato string
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

#' Concatena los dominios y la subpoblación con signo +
#'
#' Recibe strings con dominios y subpoblación y devuelve un string concatenado con caracter +
#'
#' @param domains dominios en formato string
#' @param subpop subpoblación ingresada por el usuario en formato string
#'
#' @return string concatenado de dominios y subpoblación

concat_domains <- function(domains, subpop) {
  dominios_form <-  paste(domains, subpop, sep = "+")
  return(dominios_form)
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
#' @param var sting con el nombre de la variable
#' @param var disenio complejo
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
#' @param var sting con el nombre de la variable
#' @param var disenio complejo
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
  } else if (estimation == "prop") {
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

#' Calcula medias a partir de cierta agregacion
#'
#' Genera una tabla con estimaciones para una agregacion determinada
#'
#' @param var variable objetivo dentro de un \code{dataframe}. Debe anteponerse ~
#' @param dominios dominios de estimacion separados por signo +. Debe anteponerse ~
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#' @param media \code{boolean} indicating if the mean must be calculated
#' @param env \code{environment} toma el ambiente de la funcion contenedora, para usar los elementos requeridos
#' @return \code{dataframe} que contiene variables de agregacion, variable objetivo y error estandar
#' @import survey

calcular_tabla <-  function(var, dominios, disenio, estimation = "mean", env = parent.frame(), fun, denom = denominador) {

  # El primer if es para dominios
  if (!is.null(dominios)) {

    if (estimation == "mean") { # para estimaciones de media, proporción y tamaños

      estimacion <-  survey::svyby(formula = var,
                                   by = dominios,
                                   design = disenio,
                                   FUN = fun,
                                   deff = get("deff", env))


    } else if (estimation == "ratio")  {

      estimacion <- survey::svyby(var, denominator = denom,
                                  design =  disenio,
                                  by = dominios ,
                                  FUN = fun,
                                  deff = get("deff", env))

      return(estimacion)

    } else { # para calcular la mediana

      estimacion <- survey::svyby(var,
                                  by = dominios,
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
#' @param dominios dominios de estimacion separados por signo +. Debe anteponerse ~
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#' @param env \code{environment} toma el ambiente de la funcion contenedora, para usar los elementos requeridos
#' @return \code{dataframe} que contiene variables de agregacion, variable objetivo y error estandar
#' @import survey

calcular_tabla_ratio <-  function(var,denominador, dominios = NULL, disenio, env = parent.frame()) {
  if (!is.null(dominios)) {
    estimacion <- survey::svyby(var, denominator = denominador,design =  disenio, by = dominios , FUN = svyratio, deff = get("deff", env))
  } else {
    estimacion <- survey::svyratio(var, denominator = denominador, design = disenio, deff = get("deff", env))
  }
  return(estimacion)
}


#-----------------------------------------------------------------------

#' Calcula tamanio muestral para las medias
#'
#' Genera una tabla con el conteo de cada cada una de los dominios del tabulado.
#' La funcion contempla un caso para proporcion y un caso para promedio
#' @param var string que contiene el nombre de la variable de proporcion que se evalua.
#' @param data \code{dataframe} que contiene los datos que se estan evaluando
#' @param domains vector de caracteres que contiene los dominios a evaluar
#' @df_type \code{string} Use degrees of freedom calculation approach from INE Chile or CEPAL, by default "ine".
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar

get_sample_size <- function(var, data, domains, df_type = "cepal") {

  if(!is.null(domains)){

    if(df_type == "ine"){

      #### version INE con dominios
      dom1 <- c(var, domains)
      symbol_var <- rlang::parse_expr(var)

      data %>%
     #   dplyr::mutate(!!symbol_var := as.numeric(!!symbol_var)) %>% # para prevenir problemas
        dplyr::group_by(.dots = as.list(dom1)) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::ungroup() %>% #%>%     dplyr::select(-var)
        dplyr::filter(!!symbol_var == 1) %>%
        dplyr::select(-symbol_var) %>%
        dplyr::mutate_at(dplyr::vars(domains), as.character)

    }else if(df_type == "cepal"){

      dom1 <- c(var, domains)
      symbol_var <- rlang::parse_expr(var)

      data %>%
      #  dplyr::mutate(!!symbol_var := as.numeric(!!symbol_var)) %>% # para prevenir problemas
        dplyr::group_by(.dots = as.list(dom1)) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::ungroup() %>% #%>%     dplyr::select(-var)
        dplyr::group_by(!!rlang::parse_expr(domains)) %>%
        dplyr::summarise(n = sum(n)) %>%
        dplyr::mutate_at(dplyr::vars(domains), as.character)

    }

    ### sin dominios
  }else{

    if(df_type == "ine"){

      #### version INE con dominios
      symbol_var <- rlang::parse_expr(var)

      data %>%
    #    dplyr::mutate(!!symbol_var := as.numeric(!!symbol_var)) %>% # para prevenir problemas
        dplyr::group_by(.dots = as.list(var)) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::ungroup() %>% #
        dplyr::filter(!!symbol_var == 1) %>%
        dplyr::select(-symbol_var)

    }else if(df_type == "cepal"){

      data %>%
     #   dplyr::mutate(!!symbol_var := as.numeric(!!symbol_var)) %>% # para prevenir problemas
        dplyr::group_by(.dots = as.list(var)) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::summarise(n = sum(n))

    }

  }

}

# -----------------------------------------------------------------------

unweighted_cases <- function(data, dominios, var) {
  symbol_var <- rlang::parse_expr(var)

  data %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::group_by_at(.vars = dominios) %>%
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
#' Genera una tabla con el conteo de UPM para cada uno de los dominios del tabulado.
#' La columna que contiene la informacion de las UPMs debe llamarse varunit
#' La funcion contempla un caso para proporcion y un caso para promedio
#'
#' @param data \code{dataframe} que contiene los datos que se estan evaluando
#' @param dominios vector de caracteres que contiene los dominios a evaluar
#' @param var string que contiene el nombre de la variable de proporcion que se evalua.
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'

calcular_upm <- function(data, dominios, var = NULL ) {
    listado <- c("varunit", dominios)
    if (is.null(var)) {
      data %>%
        dplyr::group_by_at(.vars = listado) %>%
        dplyr::summarise(conteo = dplyr::n()) %>%
        dplyr::mutate(tiene_info = dplyr::if_else(.data$conteo > 0, 1, 0))  %>%
        dplyr::group_by_at(.vars = dominios) %>%
        dplyr::summarise(upm = sum(.data$tiene_info))
    } else {
      symbol_var <- rlang::parse_expr(var)
      data %>%
        dplyr::mutate(!!symbol_var := as.numeric(!!symbol_var)) %>%
        dplyr::group_by_at(.vars = listado) %>%
        dplyr::summarise(conteo = sum(!!symbol_var)) %>%
        dplyr::mutate(tiene_info = dplyr::if_else(.data$conteo > 0, 1, 0))  %>%
        dplyr::group_by_at(.vars = dominios) %>%
        dplyr::summarise(upm = sum(.data$tiene_info))
    }


}
#-----------------------------------------------------------------------

#' Calcula el numero de estratos
#'
#' Genera una tabla con el conteo de estratos para cada uno de los dominios del tabulado.
#' La columna que contiene la informacion de los estratos debe llamarse varstrat
#' La funcion contempla un caso para proporcion y un caso para promedio

#' @importFrom rlang .data
#' @importFrom rlang  :=
#' @param data \code{dataframe} que contiene los datos que se estan evaluando
#' @param var variable objetivo. Debe ser un integer que toma los valores 1 o 0
#' @param dominios vector de caracteres que contiene los dominios a evaluar
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar

calcular_estrato <- function(data, dominios, var = NULL ) {


  listado <- c("varstrat", dominios)
  if (is.null(var)) {
    data %>%
      dplyr::group_by_at(.vars = listado) %>%
      dplyr::summarise(conteo = dplyr::n()) %>%
      dplyr::mutate(tiene_info = dplyr::if_else(.data$conteo > 0, 1, 0)) %>%
      dplyr::group_by_at(.vars = dominios) %>%
      dplyr::summarise(varstrat = sum(.data$tiene_info))
  } else {
    symbol_var <- rlang::parse_expr(var)
    data %>%
      dplyr::mutate(!!symbol_var := as.numeric(!!symbol_var)) %>%
      dplyr::group_by_at(.vars = listado) %>%
      dplyr::summarise(conteo = sum(!!symbol_var)) %>%
      dplyr::mutate(tiene_info = dplyr::if_else(.data$conteo > 0, 1, 0)) %>%
      dplyr::group_by_at(.vars = dominios) %>%
      dplyr::summarise(varstrat = sum(.data$tiene_info))
  }
}

#----------------------------------------------------------------------------

#' Calcula los grados de libertad para un estimaciones de total
#'
#' Genera una tabla con el conteo de grados de libertad para cada uno de los dominios del tabulado. Es un wrapper que reune a las funciones calcular_upm y calcular_estrato
#'
#' @param datos \code{dataframe} que contiene los datos que se estan evaluando. Se obtiene a partir del disenio muestral
#' @param variables variables objetivo. vector de strings que contiene los nombres de las variables
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar

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

#' Genera intervalos de confianza para todos los dominios estimados
#'
#' Usa la tabla creada para calcular el estandar y le agrega dos columnas con el limite inferior y superior del intervalo de confianza
#'
#' @param data \code{dataframe} con todos los datos necesarios para calcular el estandar
#' @param env \code{environment} toma el ambiente de la funcion contenedora, para usar los elementos requeridos
#' @param tipo \code{string} que indica cual es el tipo de estimacion que se realiza.
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @return \code{dataframe} que contiene todos los elementos del estandar, junto a tres columnas nuevas que contienen el limite inferior, el limite superior y el valor t
#'


calcular_ic <-  function(data,  ajuste_ene) {


  # Se calculan los intervalos de la manera tradicional en la generalidad de los casos
  if (ajuste_ene == F) {

    final <- data %>%
      dplyr::mutate(t = stats::qt(c(.975), df = df),
                    li = .data$stat - .data$se*t,
                    ls = .data$stat + .data$se*t)
    # Estos corresponde al ajuste de la ENE: el t se fija en 2
  } else if (ajuste_ene == T) {

    final <- data %>%
      dplyr::mutate(t = 2,
                    li = .data$stat - .data$se*t,
                    ls = .data$stat + .data$se*t)
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




#---------------------------------------------------------------------

#' Calcula medianas con metodo replicado
#'
#' Se usa para acortar un poco el codigo de la funcion mediana
#'
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#' @param var variable a estimar
#' @param dominios dominios para desagregar
#' @param sub se usa para filtrar cuando el usuario lo requiere
#' @param env ambiente en el cual se crean algunas variable relevantes
#' @return \code{vector} que contiene la variable con los conglomerados.
#' @import iterators



calcular_medianas_internal <- function(var, dominios, disenio, sub = F, env = parent.frame()) {


  #Si el usuario pone una subpoblacion, se hace un filtro en el disenio para agilizar el calculo
  if (sub == T) {
    filtro <-  rlang::parse_expr(get("subpop", env))
    disenio <- subset(disenio,   rlang::eval_tidy(filtro) == 1)

  }

  # Generar un vector con la desagregacion necesaria
  doms <- as.character(dominios)
  doms <- stringr::str_split(doms[[2]], "\\+")
  doms <- stringr::str_remove_all(doms[[1]], " ")

  # Identificar cuales son las categorias de cada una de las variables de desagregacion
  categorias <- purrr::map(doms, ~sort(unique(as.character(disenio$variables[[.x]]) )))

  # Generar el iterador, segun el numero de desagregaciones pedidas por el usuario. Ademas, se calcula el numero de combinaciones de celdas.
  # Se permite hasta 5 desagregaciones. Sobre ese nivel la función se cae.
  if (length(categorias) == 1) {
    it <- itertools::ihasNext(itertools::product(categorias[[1]]))
    combinaciones <- length(categorias[[1]])

  } else if (length(categorias) == 2) {
    it <- itertools::ihasNext(itertools::product(categorias[[1]], categorias[[2]]))
    combinaciones <- length(categorias[[1]]) * length(categorias[[2]])

  } else if (length(categorias) == 3) {
    it <- itertools::ihasNext(itertools::product(categorias[[1]], categorias[[2]], categorias[[3]] ))
    combinaciones <- length(categorias[[1]]) * length(categorias[[2]]) * length(categorias[[3]])


  } else if (length(categorias) == 4) {
    it <- itertools::ihasNext(itertools::product(categorias[[1]], categorias[[2]], categorias[[3]], categorias[[4]]))
    combinaciones <- length(categorias[[1]]) * length(categorias[[2]]) * length(categorias[[3]], length(categorias[[4]]))


  } else if (length(categorias) == 5) {
    it <- itertools::ihasNext(itertools::product(categorias[[1]], categorias[[2]], categorias[[3]], categorias[[4]], categorias[[5]] ))
    combinaciones <- length(categorias[[1]]) * length(categorias[[2]]) * length(categorias[[3]], length(categorias[[4]], length(categorias[[5]])))

  }
  # Crear una matriz para guardar resultados
  acumulado <- data.frame(matrix(9999, ncol = 5, nrow = combinaciones))

  type <- get("interval_type", env)


  i <- 1
  # Mientras exista un siguiente, el while sigue operando
  while (itertools::hasNext(it)) {
    x <- iterators::nextElem(it)


    exp <- rlang::parse_expr(paste(doms, "==",  x , collapse = " & "))

    # Se usa un trycath porque en ciertos casos, la función no puede realizar el cálculo
    output <- tryCatch(
      {
        # No se usa svyby, para evitar perder un tabulado completo cuando alguna de sus celdas no puede ser calculada.
        median <- svyquantile(var,
                              design = subset(disenio, rlang::eval_tidy(exp) ),
                              quantiles = 0.5,
                              method="constant",
                              interval.type = type, #  quantile probability
                              ties="discrete")
      },
      error=function(cond) {
        return(data.frame(X1 = NA, X2 = NA))
      }

    )

    acumulado[i, ] <- output[[1]] %>%
      as.data.frame() %>%
      dplyr::mutate(v = paste(x, collapse = "."))

    i <- i + 1


  }

  return(list(output[[1]], acumulado))

  final <- acumulado  %>%
    tidyr::separate(into = doms, col = .data$X3 , sep = "\\.") %>%
    dplyr::rename(se = .data$X2,
                  V1 = .data$X1) %>%
    dplyr::relocate(.data$V1, .data$se, .after = dplyr::last_col())

  return(final)
}

#----------------------------------------------------------------------

convert_to_integer <- function(dominios, disenio) {
  # Evaluar si alguna de las variables de dominio es un factor
  variables_dominio <- stringr::str_split(dominios, pattern = "\\+")[[1]] %>%
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

  if (ess == T) {

    if (deff == F) {
      warning("to get effective sample size use deff = T")
    } else {
      final <- final %>%
        dplyr::mutate(ess = n / deff)
    }
  }
  return(final)
}


#--------------------------------------------------------------------




#' internal function to calculate ratios estimations
#'
#' @param var numeric variable within the \code{dataframe}, is the numerator of the ratio to be calculated.
#' @param denominador numeric variable within the \code{dataframe}, is the denominator of the ratio to be calculated.
#' @param dominios domains to be estimated separated by the + character.
#' @param disenio complex design created by \code{survey} package
#' @param subpop integer dummy variable to filter the dataframe
#' @param ci \code{boolean} indicating if the confidence intervals must be calculated
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @param deff \code{boolean} Design effect
#' @param ess \code{boolean} Effective sample size
#' @param rel_error \code{boolean} Relative error
#'
#' @param log_cv \code{boolean} logarithmic coefficient of variation
#'
#' @param unweighted \code{boolean} Add non weighted count if it is required
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'

create_ratio_internal <- function(var,denominador, dominios = NULL, subpop = NULL, disenio, ci = F, deff = F, ess = F,
                                  ajuste_ene = F, unweighted = F, rel_error = F, rm.na = F) {




  # Chequear que la variable objetivo y la variable subpop cumplan con ciertas condiciones
  check_input_var(var, disenio, estimation = "ratio")
  check_subpop_var(subpop, disenio)

  # Lanzar warning del error estándar cuando no se usa el diseño
  se_message(disenio)


  # Homologar nombres de variables  del diseño
  disenio <- standardize_design_variables(disenio)

  # Sacar los NA si el usuario lo requiere
  if (rm.na == T) {
    disenio <- disenio[!is.na(disenio$variables[[var]])]
  }

  # Filtrar diseño, si el usuario agrega el parámetro subpop
  disenio <- filter_design(disenio, subpop)

  #Convertir los inputs en formulas para adecuarlos a survey
  var_form <- convert_to_formula(var)
  dominios_form <- convert_to_formula(dominios)
  denominador_form <- convert_to_formula(denominador)

  tabla <- calcular_tabla(var_form, dominios_form, disenio, fun = survey::svyratio, estimation = "ratio", denom = denominador_form)

  # Crear listado de variables que se usan para el cálculo
  agrupacion <- create_groupby_vars(dominios)

  #Calcular el tamanio muestral de cada grupo
  n <- get_sample_size(var,disenio$variables, agrupacion)


  #Calcular los grados de libertad de todos los cruces
  gl <- get_df(disenio, agrupacion)

  #Extrear el coeficiente de variacion
  cv <- get_cv(tabla, disenio, agrupacion)


  # Convert to df if there is not any domain
  tabla <- convert_ratio_to_df(tabla, dominios)


  #Unir toda la informacion en una tabla final
  final <- create_output(tabla, agrupacion,  gl, n, cv)

  # Ordenar las columnas y estandarizar los nombres de las variables
  final <- standardize_columns(final, var, denominador )

  # Add unweighted counting
  if (unweighted) {
    final <- get_unweighted(final, disenio, var, agrupacion)
  }

  # Add confidence intervals
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

  return(final)

}

#-----------------------------------------------------------------------


#' internal function to calculate proportion estimations
#'
#' @param var integer dummy variable within the  \code{dataframe}
#' @param dominios domains to be estimated separated by the + character.
#' @param subpop integer dummy variable to filter the dataframe
#' @param disenio complex design created by \code{survey} package
#' @param ci \code{boolean} indicating if the confidence intervals must be calculated
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @param standard_eval \code{boolean} indicating if the function is inside another function, by default it is TRUE, avoid problems with lazy eval.
#' @param deff \code{boolean} Design effect
#' @param ess \code{boolean} Effective sample size
#' @param rel_error \code{boolean} Relative error
#'
#' @param log_cv \code{boolean} logarithmic coefficient of variation
#'
#' @param unweighted \code{boolean} Add non weighted count if it is required
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'

create_prop_internal <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F, deff = F, ess = F, ajuste_ene = F,
                                 rel_error = F, log_cv = F, unweighted = F, standard_eval = T, rm.na = F, env =  parent.frame()) {

  # Chequear que la variable objetivo y la variable subpop cumplan con ciertas condiciones
  check_input_var(var, disenio, estimation = "prop")
  check_subpop_var(subpop, disenio)

  # Lanzar warning del error estándar cuando no se usa el diseño
  se_message(disenio)


  # Homologar nombres de variables  del diseño
  disenio <- standardize_design_variables(disenio)

  # Sacar los NA si el usuario lo requiere
  if (rm.na == T) {
    disenio <- disenio[!is.na(disenio$variables[[var]])]
  }

  # Filtrar diseño, si el usuario agrega el parámetro subpop
  disenio <- filter_design(disenio, subpop)

  #Convertir los inputs en formulas para adecuarlos a survey
  var_form <- convert_to_formula(var)

  # Convertir en formula para survey
  dominios_form <- convert_to_formula(dominios)

  tabla <- calcular_tabla(var_form, dominios_form, disenio, fun = survey::svymean)

  # Crear listado de variables que se usan para el cálculo
  agrupacion <- create_groupby_vars(dominios)

  #Calcular el tamanio muestral de cada grupo
  n <- get_sample_size(var,disenio$variables, agrupacion)

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

  return(final)




}



