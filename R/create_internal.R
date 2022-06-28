
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


standardize_columns <- function(data, var) {
  names(data) <- names(data) %>%
    tolower() %>%
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
#' @param data dataframe
#' @param domains dominios en formato string
#' @return dataframe con grados de libertad


get_df <- function(data, domains) {
  design <- data
  data <- data$variables

  # Si no hay diseño, no se calcula nada
 if (as.character(design$call$ids)[[2]] == "1") {
    gl <- data %>%
      dplyr::group_by(.dots = domains) %>%
      dplyr::summarise(upm = NA,
                       vartstrat = NA,
                       df = NA
                       ) %>%
      dplyr::mutate_at(.vars = dplyr::vars(domains), .funs = as.character)

    return(gl)
  }


  if (!is.null(domains)) {
    gl <- calcular_upm(data, dominios = domains) %>%
      dplyr::left_join(calcular_estrato(data, domains), by = domains) %>%
      dplyr::mutate(df = .data$upm - varstrat) %>%
      dplyr::mutate_at(.vars = dplyr::vars(domains), .funs = as.character)

  } else {
    varstrat <- length(unique(data$varstrat))
    varunit <- length(unique(data$varunit))
    gl <- varunit - varstrat

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



check_input_var <- function(var, disenio) {

  # Chequear que la variable no sea character
  if (is.character(disenio$variables[[var]]) == T) stop("You are using a character vector!")

  #Chequear que la variable sea dummy. Si es una dummy, aparece un warning
  es_prop <- disenio$variables %>%
    dplyr::mutate(es_prop = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0 | is.na(!!rlang::parse_expr(var)),
                                           1, 0))

  if (sum(es_prop$es_prop) == nrow(disenio$variables)) warning("It seems yor are using a proportion variable!")

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

calcular_tabla <-  function(var, dominios, disenio, media = T, env = parent.frame()) {


  # El primer if es para dominios
  if (!is.null(dominios)) {

    if (media == T) { # para calcular la media

      estimacion <- survey::svyby(var ,
                                  design = disenio,
                                  by = dominios,
                                  FUN = svymean,
                                  deff = get("deff", env))

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
    if (media == T) { # para calcular la media

      estimacion <- survey::svymean(var, disenio, deff = get("deff", env))


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
#'
#' @param data \code{dataframe} que contiene los datos que se estan evaluando
#' @param dominios vector de caracteres que contiene los dominios a evaluar
#' @param var string que contiene el nombre de la variable de proporcion que se evalua.
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'

calcular_n <- function(data, dominios, var = NULL) {

  # Esto es para el caso de proporcion
  if (is.null(var)) {
    sample_n <- data %>%
      dplyr::group_by(.dots = as.list(dominios)  ) %>%
      dplyr::summarise(n = dplyr::n())
    # Este es el caso de nivel
  } else {
    symbol_var <- rlang::parse_expr(var)
    sample_n <-  data %>%
      dplyr::mutate(!!symbol_var := as.numeric(!!symbol_var)) %>% # para prevenir problemas
      dplyr::group_by(.dots = as.list(dominios)) %>%
      dplyr::summarise(n = sum(!!symbol_var))
  }
  sample_n <- sample_n  %>%
    dplyr::mutate_at(.vars = dplyr::vars(dominios), .funs = as.character)

  return(sample_n)

}

# -----------------------------------------------------------------------

unweighted_cases <- function(data, dominios, var) {
  symbol_var <- rlang::parse_expr(var)

  data %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::group_by(.dots = as.list(dominios)  ) %>%
    dplyr::summarise(n = dplyr::n())
}

#-----------------------------------------------------------------------
#' Calcula tamanio muestral para la funcion de totales poblacionales
#'
#' Genera una tabla con el conteo de cada cada una de los dominios de las categorias ingresadas.
#'
#' @param x  vector de strings que contiene las variables para las cuales se calcula el tamanio muestra
#' @param datos \code{dataframe} que se esta utilizando. Se extrae del disenio muestral
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'

calcular_n_total <- function(x, datos) {
  datos %>%
    dplyr::group_by(.dots = as.list(x)) %>%
    dplyr::count() %>%
    dplyr::rename(variable := x) %>%
    dplyr::mutate(variable = paste0(x, .data$variable))
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
    listado <- c("varunit", as.list(dominios))
    if (is.null(var)) {
      data %>%
        dplyr::group_by(.dots = listado) %>%
        dplyr::summarise(conteo = dplyr::n()) %>%
        dplyr::mutate(tiene_info = dplyr::if_else(.data$conteo > 0, 1, 0))  %>%
        dplyr::group_by(.dots = as.list(dominios)) %>%
        dplyr::summarise(upm = sum(.data$tiene_info))
    } else {
      symbol_var <- rlang::parse_expr(var)
      data %>%
        dplyr::mutate(!!symbol_var := as.numeric(!!symbol_var)) %>%
        dplyr::group_by(.dots = listado) %>%
        dplyr::summarise(conteo = sum(!!symbol_var)) %>%
        dplyr::mutate(tiene_info = dplyr::if_else(.data$conteo > 0, 1, 0))  %>%
        dplyr::group_by(.dots = as.list(dominios)) %>%
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


  listado <- c("varstrat", as.list(dominios))
  if (is.null(var)) {
    data %>%
      dplyr::group_by( .dots = listado) %>%
      dplyr::summarise(conteo = dplyr::n()) %>%
      dplyr::mutate(tiene_info = dplyr::if_else(.data$conteo > 0, 1, 0)) %>%
      dplyr::group_by(.dots = as.list(dominios)) %>%
      dplyr::summarise(varstrat = sum(.data$tiene_info))
  } else {
    symbol_var <- rlang::parse_expr(var)
    data %>%
      dplyr::mutate(!!symbol_var := as.numeric(!!symbol_var)) %>%
      dplyr::group_by(.dots = listado) %>%
      dplyr::summarise(conteo = sum(!!symbol_var)) %>%
      dplyr::mutate(tiene_info = dplyr::if_else(.data$conteo > 0, 1, 0)) %>%
      dplyr::group_by(.dots = as.list(dominios)) %>%
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
  acumulado <- data.frame(matrix(9999, ncol = 3, nrow = combinaciones))

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

    acumulado[i, ] <- output %>%
      as.data.frame() %>%
      dplyr::mutate(v = paste(x, collapse = "."))

    i <- i + 1


  }


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
                                  ajuste_ene = F, rel_error = F ) {

  # Chequar que esten presentes las variables del disenio muestral. Si no se llaman varstrat y varunit, se
  #  detiene la ejecucion
  # chequear_var_disenio(disenio$variables)
  disenio$variables$varunit <- disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat <- disenio$variables[[unificar_variables_estrato(disenio)]]
  disenio$variables$fe = disenio$variables[[unificar_variables_factExp(disenio)]]


  ### filtramos base de disenioo por los casos que tengan datos tanto del denominador como del numerador. para
  ### calcular correctamente los GL y N
  disenio <- disenio[disenio$variables[[var]] != 0 | disenio$variables[[denominador]] != 0]

  # Chequear que la variable no sea character
  if (is.character(disenio$variables[[var]]) == T) stop("You are using a character vector!")

  # Chequear que la variable no sea character
  if (is.character(disenio$variables[[denominador]]) == T) stop("You are using a character vector for denominador!")

  #Convertir los inputs en formulas para adecuarlos a survey
  var <- paste0("~", var) %>%
    stats::as.formula()

  #Convertir los inputs en formulas para adecuarlos a survey
  denominador <- paste0("~", denominador) %>%
    stats::as.formula()

  # CON DESAGREGACIoN
  if (!is.null(dominios[[1]])) {

    # Sin subpop #
    if (is.null(subpop)) {
      dominios <- paste0("~", dominios) %>%
        stats::as.formula()

      # con subpop
    } else if (!is.null(subpop)) { # caso que tiene subpop

      dominios <- paste(dominios, subpop, sep = "+")
      dominios <- paste0("~", dominios) %>%
        stats::as.formula()
    }

    #Generar la tabla con los calculos
    tabla <- calcular_tabla_ratio(var, denominador, dominios, disenio)

    #Extraer nombres
    # nombres <- names(tabla)
    # agrupacion <-  nombres[-c(length(nombres) - 2, (length(nombres) - 1), length(nombres)) ]
    #var_ratio <- nombres[length(nombres) - 2]

    # Building of the estimated ratio
    var_ratio <- paste(as.character(var)[[2]], as.character(denominador)[[2]], sep = "/")

    agrupacion <-  strsplit(as.character(dominios)[[2]], split = "\\+")[[1]] %>%
      trimws(which = "both")

    #+ Calcular N
    n <- calcular_n(disenio$variables, agrupacion) %>%
      dplyr::mutate_at(dplyr::vars(agrupacion), as.character)

    #+ Calcular GL de todos los cruces
    gl <- calcular_upm(disenio$variables, agrupacion) %>%
      dplyr::left_join(calcular_estrato(disenio$variables, agrupacion), by = agrupacion) %>%
      dplyr::mutate(gl = .data$upm - .data$varstrat) %>%
      dplyr::mutate_at(dplyr::vars(agrupacion), as.character)

    #+ Calcurar CV
    tabla$cv = survey::cv(tabla)



    #* * Armar tabla final
    final <- tabla %>%
      dplyr::mutate_at(dplyr::vars(agrupacion), as.character) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl" )),
                       by = agrupacion) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n" )),
                       by = agrupacion)


    #Cambiar el nombre de la variable objetivo para que siempre sea igual.
    final <- final  %>%
      dplyr::rename(objetivo = var_ratio) %>%
      dplyr::filter(.data$objetivo > 0) # se eliminan los ceros de la tabla


    # Se renombra el error estándar
    names(final)[grep("objetivo", names(final)) +1] = "se"
    names(final) <- tolower(names(final))


    #intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final,tipo = "prop_agregado",  ajuste_ene = ajuste_ene)
    }

    # SIN DESAGREGACIoN #
  } else {

    # Con subpobp
    if (!is.null(subpop)) {

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecucion
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))

      if (sum(is.na(disenio$variables[[subpop]] > 0 ))) stop("subpop contains NAs!")
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("subpop must be a dummy variable!")

      # Aqui se filtra el disenio
      #  subpop_text <- rlang::expr_text(rlang::enexpr(subpop))
      disenio <- disenio[disenio$variables[[subpop]] == 1]

    }

    #Generar la tabla con los calculos
    tabla <- calcular_tabla_ratio(var, denominador, dominios, disenio)

    #+ Calcular N
    n <- nrow(disenio$variables)

    #+ Calcular GL
    varstrat <- length(unique(disenio$variables$varstrat))
    varunit <- length(unique(disenio$variables$varunit))
    gl <- varunit - varstrat

    #+ Calcular CV
    cv <- cv(tabla, design = disenio)

    #* * Armar tabla final
    if (deff == T) {
      final <- data.frame(tabla$ratio, survey::SE(tabla), deff = survey::deff(tabla))
      final$cv = cv[1]
      names(final) = c("objetivo", "se",  "deff", "cv")

    } else {
      final <- data.frame(tabla$ratio, survey::SE(tabla))
      final$cv = cv[1]
      names(final) = c("objetivo", "se", "cv")

    }

    # Armar tabla completa con todos los insumos
    final <- dplyr::bind_cols(final, "gl" = gl, "n" = n)
    #names(final)[2] <- "se"


    ##Cambiar el nombre de la variable objetivo para que siempre sea igual
    #final <- final %>%
    #  dplyr::rename(objetivo = mean)

    #intervalo de confianza solo si el usuario lo pide
    if (ci == T) {
      final <- calcular_ic(data = final, tipo = "prop_agregado",  ajuste_ene = ajuste_ene)

    }




  }

  # Reorder columns if it is neccesary
  if (deff == T) {
    final <- final %>%
      dplyr::relocate(deff, .after = dplyr::last_col())
  }

  # rename column name for CV. It has to be coef_var cause the evaluation function
  final <- final %>%
    dplyr::rename(coef_var = cv)

  # add relative error, if the user uses this parameter
  if (rel_error == T) {
    final <- final %>%
      dplyr::mutate(relative_error = stats::qt(c(.975), df = gl) * coef_var)
  }

  # add the ess if the user uses this parameter
  final <- get_ess(ess)

  if(!is.null(dominios) && !is.null(subpop)){

    final = final %>% dplyr::filter(!!rlang::parse_expr(subpop)  == 1) %>% dplyr::select(-!!rlang::parse_expr(subpop))

  }

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
                                 rel_error = F, log_cv = F, unweighted = F, standard_eval = T){


  # Chequar que esten presentes las variables del disenio muestral. Si no se llaman varstrat y varunit, se
  #  detiene la ejecucion
  # chequear_var_disenio(disenio$variables)
  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]
  disenio$variables$fe = disenio$variables[[unificar_variables_factExp(disenio)]]

  if (standard_eval == F) {
    #  # Encapsular inputs para usarlos mas tarde
    var <- rlang::enexpr(var)
    var <-  rlang::expr_name(var)
    dominios <- rlang::enexpr(dominios)
    if(!is.null(dominios)){
      dominios <-  rlang::expr_name(dominios)
    }

    subpop <- rlang::enexpr(subpop)
    if(!is.null(subpop)){
      subpop <-  rlang::expr_name(subpop)
    }

  }

  #Convertir los inputs en formulas para adecuarlos a survey
  var_form <- paste0("~",var) %>%
    stats::as.formula()


  # Guardar una variable para usarla más tarde
  var_string <- var

  # Test básica para evaluar que la variable no sea character
  if (is.character(disenio$variables[[var]]) == T) stop("You are using a character vector!")

  #Chequear que la variable sea de proporcion. Si no lo es, se interrumpe la ejecucion
  es_prop <- disenio$variables %>%
    dplyr::mutate(es_prop_var = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var)  == 0 | is.na(!!rlang::parse_expr(var)), 1, 0))

  if (sum(es_prop$es_prop_var) != nrow(es_prop)) stop("La variable no es de proporcion!")

  #COnvertir los inputs en formulas para adecuarlos a survey
  var <- paste0("~", var) %>%
    stats::as.formula()


  # ESTO CORRESPONDE AL CASO CON DESAGREGACIoN
  if (!is.null(dominios[[1]])) {

    # Esto corre para el caso en el que NO hay subpop
    if (is.null(subpop)) {
      dominios <- paste0("~", dominios) %>%
        stats::as.formula()

      # Esto corre para subpop
    } else if (!is.null(subpop)) { # caso que tiene subpop

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecucion
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))

      if (sum(is.na(disenio$variables[[subpop]] > 0 ))) stop("subpop contains NAs!")

      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("subpop must be a dummy variable!")

      dominios <- paste(dominios, subpop, sep = "+")
      dominios <- paste0("~", dominios) %>%
        stats::as.formula()
    }


    #Generar la tabla con los calculos
    tabla <- calcular_tabla(var, dominios, disenio)

    #Extraer nombres
    nombres <- names(tabla)
    # agrupacion <-  nombres[-c((length(nombres) - 2), (length(nombres) - 1), length(nombres)) ]
    var_prop <- var_string

    agrupacion <-  strsplit(as.character(dominios)[[2]], split = "\\+")[[1]] %>%
      trimws(which = "both")


    #Calcular el tamanio muestral de cada grupo
    n <- calcular_n(disenio$variables, agrupacion) %>%
      dplyr::mutate_at(dplyr::vars(agrupacion), as.character)


    #Calcular los grados de libertad de todos los cruces
    gl <- calcular_upm(disenio$variables, agrupacion) %>%
      dplyr::left_join(calcular_estrato(disenio$variables, agrupacion), by = agrupacion) %>%
      dplyr::mutate(gl = .data$upm - .data$varstrat) %>%
      dplyr::mutate_at(dplyr::vars(agrupacion), as.character)

    # Coeficiente de variación
    tabla$cv <- survey::cv(tabla)


    #Unir toda la informacion. Se hace con join para asegurar que no existan problemas en la union
    final <- tabla %>%
      dplyr::mutate_at(dplyr::vars(agrupacion), as.character) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl" )),
                       by = agrupacion) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n" )),
                       by = agrupacion)

    # Get unweighted counting if it is required by the user
    if (unweighted) {
      unweighted_cases <- calcular_n(disenio$variables, c(agrupacion, var_string) ) %>%
        dplyr::mutate_at(dplyr::vars(agrupacion), as.character)  %>%
        dplyr::filter(!!rlang::parse_expr(var_string) == 1 ) %>%
        dplyr::rename(unweighted = n)

      final <- final %>%
        dplyr::left_join(unweighted_cases %>% dplyr::select(c(agrupacion, "unweighted" )),
                         by = agrupacion)
    }


    var_string = var

    #Cambiar el nombre de la variable objetivo para que siempre sea igual.
    final <- final  %>%
      dplyr::rename(objetivo = var_prop) # %>%
    #dplyr::filter(.data$objetivo > 0) # se eliminan los ceros de la tabla



    # Ajustar nombres de la tabla, para que tengan un formato estándar
    names(final) <- names(final) %>%
      stringr::str_replace(pattern = paste0("DEff.", var_prop), "deff")


    final <- final %>%
      dplyr::rename(coef_var = cv)


    #Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final,tipo = "prop_agregado",  ajuste_ene = ajuste_ene)
    }


    # ESTO CORRESPONDE AL CASO SIN DESAGREGACIoN
  } else {

    # Si el usuario ingresa subpoblacion, se filtra la base de datos para la subpoblacion de referencia
    if (!is.null(subpop)) {

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecucion
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))


      if (sum(is.na(disenio$variables[[subpop]] > 0 ))) stop("subpop contains NAs!")
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("subpop must be a dummy variable!")

      # Aqui se filtra el disenio
      #  subpop_text <- rlang::expr_text(rlang::enexpr(subpop))
      disenio <- disenio[disenio$variables[[subpop]] == 1]

    }

    dominios_form = dominios

    #Generar la tabla con los calculos
    tabla <- calcular_tabla(var_form, dominios_form, disenio)

    # Tamanio muestral
    n <- nrow(disenio$variables)

    # get unweighted counting
    unweighted_cases <- disenio$variables %>%
      dplyr::filter(!!rlang::parse_expr(var_string) == 1 ) %>%
      nrow()

    # Calcular grados de libertad
    varstrat <- length(unique(disenio$variables$varstrat))
    varunit <- length(unique(disenio$variables$varunit))
    gl <- varunit - varstrat

    # Calcular coeficiente de variacion
    cv <- cv(tabla, design = disenio)

    # Armar tabla final
    final <- data.frame(tabla)

    # Armar tabla completa con todos los insumos
    final <- dplyr::bind_cols(final, "gl" = gl , "n" = n, "coef_var" = cv[1])
    names(final)[2] <- "se"


    if (unweighted) {
      final <- dplyr::bind_cols(final, "unweighted" =  unweighted_cases)

    }

    #Cambiar el nombre de la variable objetivo para que siempre sea igual
    final <- final %>%
      dplyr::rename(objetivo = mean)

    # Se calcula el intervalo de confianza solo si el usuario lo pide
    if (ci == T) {
      final <- calcular_ic(data = final, tipo = "prop_agregado",  ajuste_ene = ajuste_ene)

    }


  }


  #################
  # Shared outputs #
  #################

  # Ajustar nombres de la tabla, para que tengan un formato estándar. Se deja el deff al final
  if (deff == T) {

    final <- final %>%
      dplyr::relocate(deff, .after = dplyr::last_col())

  }

  # add relative error, if the user uses this parameter
  if (rel_error == T) {
    final <- final %>%
      dplyr::mutate(relative_error = stats::qt(c(.975), df = gl) * coef_var)
  }


  # add the ess if the user uses this parameter
  final <- get_ess(ess)

  # add log cv, if the user uses this parameter
  if (log_cv) {
    final <- final %>%
      dplyr::mutate(log_cv = se / (-log(objetivo)*objetivo))
  }

  if(!is.null(dominios) && !is.null(subpop)){
    final = final %>%
      dplyr::filter(!!rlang::parse_expr(subpop)  == 1) %>%
      dplyr::select(-!!rlang::parse_expr(subpop))
  }

  return(final)

}


