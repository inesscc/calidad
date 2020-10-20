#' Calcula medias a partir de cierta agregación
#'
#' Genera una tabla con estimaciones para una agregación determinada
#'
#' @param var variable objetivo dentro de un \code{dataframe}. Debe anteponerse ~
#' @param dominios dominios de estimación separados por signo +. Debe anteponerse ~
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#'
#' @return \code{dataframe} que contiene variables de agregación, variable objetivo y error estándar
#' @import survey
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' calcular_tabla(gastot_hd, zona+sexo, dc)
calcular_tabla <-  function(var, dominios, disenio) {

  if (!is.null(dominios)) {
    estimacion <- survey::svyby(var , design = disenio, by = dominios , FUN = svymean)
  } else {
    estimacion <- survey::svymean(var, disenio)
  }

  return(estimacion)
}





#-----------------------------------------------------------------------

#' Calcula tamaño muestral para las medias
#'
#' Genera una tabla con el conteo de cada cada una de los dominios del tabulado.
#' La función contempla un caso para proporción y un caso para promedio
#'
#' @param data \code{dataframe} que contiene los datos que se están evaluando
#' @param dominios vector de caracteres que contiene los dominios a evaluar
#' @param var string que contiene el nombre de la variable de proporción que se evalúa.
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' calcular_n(epf_personas, c("zona", "sexo"), var = NULL)
calcular_n <- function(data, dominios, var = NULL) {

  # Esto es para el caso de proporción
  if (is.null(var)) {
    data %>%
      dplyr::group_by(.dots = as.list(dominios)  ) %>%
      dplyr::summarise(n = dplyr::n())
  # Este es el caso de nivel
  } else {
    symbol_var <- rlang::parse_expr(var)
    data %>%
      dplyr::mutate(!!symbol_var := as.numeric(!!symbol_var)) %>% # para prevenir problemas
      dplyr::group_by(.dots = as.list(dominios)) %>%
      dplyr::summarise(n = sum(!!symbol_var))
  }
}

#-----------------------------------------------------------------------
#' Calcula tamaño muestral para la función de totales poblacionales
#'
#' Genera una tabla con el conteo de cada cada una de los dominios de las categorías ingresadas.
#'
#' @param x  vector de strings que contiene las variables para las cuales se calcula el tamaño muestra
#' @param datos \code{dataframe} que se está utilizando. Se extrae del disenio muestral
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' calcular_n_total(c("zona", "sexo"), var = dc$variables)

calcular_n_total <- function(x, datos) {
  datos %>%
    dplyr::group_by(.dots = as.list(x)) %>%
    dplyr::count() %>%
    dplyr::rename(variable := x) %>%
    dplyr::mutate(variable = paste0(x, variable))
}


#-----------------------------------------------------------------------

#' Calcula el número de UPM
#'
#' Genera una tabla con el conteo de UPM para cada uno de los dominios del tabulado.
#' La columna que contiene la información de las UPMs debe llamarse varunit
#' La función contempla un caso para proporción y un caso para promedio
#'
#' @param data \code{dataframe} que contiene los datos que se están evaluando
#' @param dominios vector de caracteres que contiene los dominios a evaluar
#' @param var string que contiene el nombre de la variable de proporción que se evalúa.
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' calcular_upm(epf_personas, c("zona", "sexo"), "ocupado")

calcular_upm <- function(data, dominios, var = NULL ) {

  #Chequear que existe variable varunit en el dataset
  if (sum(grepl(pattern = "varunit" , x = names(epf_personas))) == 0) {
    stop("¡La columna que contiene información de las UPMs debe llamarse varunit!")
  }


  listado <- c("varunit", as.list(dominios))
  if (is.null(var)) {
    data %>%
      dplyr::group_by(.dots = listado) %>%
      dplyr::summarise(conteo = dplyr::n()) %>%
      dplyr::mutate(tiene_info = dplyr::if_else(conteo > 0, 1, 0))  %>%
      dplyr::group_by(.dots = as.list(dominios)) %>%
      dplyr::summarise(upm = sum(tiene_info))
  } else {
    symbol_var <- rlang::parse_expr(var)
     data %>%
      dplyr::mutate(!!symbol_var := as.numeric(!!symbol_var)) %>%
      dplyr::group_by(.dots = listado) %>%
      dplyr::summarise(conteo = sum(!!symbol_var)) %>%
      dplyr::mutate(tiene_info = dplyr::if_else(conteo > 0, 1, 0))  %>%
      dplyr::group_by(.dots = as.list(dominios)) %>%
      dplyr::summarise(upm = sum(tiene_info))
  }
}
#-----------------------------------------------------------------------

#' Calcula el número de estratos
#'
#' Genera una tabla con el conteo de estratos para cada uno de los dominios del tabulado.
#' La columna que contiene la información de los estratos debe llamarse varstrat
#' La función contempla un caso para proporción y un caso para promedio
#'
#' @param data \code{dataframe} que contiene los datos que se están evaluando
#' @param var variable objetivo. Debe ser un integer que toma los valores 1 o 0
#' @param dominios vector de caracteres que contiene los dominios a evaluar
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' calcular_estrato(epf_personas, c("zona", "sexo"), "ocupado")

calcular_estrato <- function(data, dominios, var = NULL ) {

  #Chequear que existe variable varstrat en el dataset
  if (sum(grepl(pattern = "varstrat" , x = names(epf_personas))) == 0) {
    stop("¡La columna que contiene información de los estratos debe llamarse varstrat!")
  }

  listado <- c("varstrat", as.list(dominios))
  if (is.null(var)) {
    data %>%
      dplyr::group_by( .dots = listado) %>%
      dplyr::summarise(conteo = dplyr::n()) %>%
      dplyr::mutate(tiene_info = dplyr::if_else(conteo > 0, 1, 0)) %>%
      dplyr::group_by(.dots = as.list(dominios)) %>%
      dplyr::summarise(varstrat = sum(tiene_info))
  } else {
    symbol_var <- rlang::parse_expr(var)
     data %>%
      dplyr::mutate(!!symbol_var := as.numeric(!!symbol_var)) %>%
      dplyr::group_by(.dots = listado) %>%
      dplyr::summarise(conteo = sum(!!symbol_var)) %>%
      dplyr::mutate(tiene_info = dplyr::if_else(conteo > 0, 1, 0)) %>%
      dplyr::group_by(.dots = as.list(dominios)) %>%
      dplyr::summarise(varstrat = sum(tiene_info))
  }
}

#----------------------------------------------------------------------------

#' Calcula los grados de libertad para un estimaciones de total
#'
#' Genera una tabla con el conteo de grados de libertad para cada uno de los dominios del tabulado. Es un wrapper que reune a las funciones calcular_upm y calcular_estrato
#'
#' @param datos \code{dataframe} que contiene los datos que se están evaluando. Se obtiene a partir del diseño muestral
#' @param variables variables objetivo. vector de strings que contiene los nombres de las variables
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' calcular_gl_total(c("zona", "sexo"), dc$variables)

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
    dplyr::mutate(gl = upm - varstrat)
  return(gl)

}



#--------------------------------------------------------------------
#' Crea los insumos necesarios para hacer la evaluación de estimadores de la media
#'
#' Genera una tabla con los siguientes insumos: media, grados de libertad,
#' tamaño muestral y coeficiente de variación. La función contempla la posibilidad de desagregar la estimación en uno o más dominios.
#'
#' @param var variable objetivo dentro de un \code{dataframe}.
#' @param dominios dominios de estimación separados por signo +.
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' crear_insumos(gastot_hd, zona+sexo, dc)
#' @export

crear_insumos <- function(var, dominios = NULL, disenio) {

  #Chequear que la variable sea de continua. Si no lo es, aparece un warning
  enquo_var <-  rlang::enquo(var)

  es_prop <- disenio$variables %>%
    dplyr::mutate(es_prop = dplyr::if_else(!!enquo_var == 1 | !!enquo_var == 0, 1, 0))

  if (sum(es_prop$es_prop) == nrow(disenio$variables)) warning("¡Parece que tu variable es de proporción!")


  #Convertir los inputs en fórmulas para adecuarlos a survey
  var <- paste0("~", rlang::enexpr(var)) %>%
    as.formula()

  # ESTO CORRESPONDE AL CASO CON DESAGREGACIÓN
  if (!is.null(rlang::enexprs(dominios)[[1]])) {

    dominios <- paste0("~", rlang::enexprs(dominios)) %>%
      as.formula()

    #Generar la tabla con los cálculos
    tabla <- calcular_tabla(var, dominios, disenio)

  #Extraer nombres
  nombres <- names(tabla)
  agrupacion <-  nombres[c(-(length(nombres) - 1), -length(nombres)) ]


  #Calcular el tamaño muestral de cada grupo
  n <- calcular_n(disenio$variables, agrupacion)

  #Calcular los grados de libertad de todos los cruces
  gl <- calcular_upm(disenio$variables, agrupacion) %>%
    dplyr::left_join(calcular_estrato(disenio$variables, agrupacion), by = agrupacion) %>%
    dplyr::mutate(gl = upm - varstrat)

  #Extrear el coeficiente de variación
  cv <- cv(tabla, design = disenio) * 100

  cv <- tabla %>%
    dplyr::select(agrupacion) %>%
    dplyr::bind_cols(coef_var = cv)

  #Unir toda la información. Se hace con join para asegurar que no existan problemas en la unión
  final <- tabla %>%
    dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl")),
              by = agrupacion) %>%
    dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n")),
              by = agrupacion) %>%
    dplyr::left_join(cv %>% dplyr::select(c(agrupacion, "coef_var")),
              by = agrupacion)

  # ESTO CORRESPONDE AL CASO SIN DESAGREGACIÓN
  } else {
    #Generar la tabla con los cálculos
    tabla <- calcular_tabla(var, dominios, disenio)

    # Tamaño muestral
    n <- nrow(disenio$variables)

    # Calcular grados de libertad
    varstrat <- length(unique(disenio$variables$varstrat))
    varunit <- length(unique(disenio$variables$varunit))
    gl <- varunit - varstrat

    # Calcular coeficiente de variación
    cv <- cv(tabla, design = disenio) * 100

    # Armar tabla final
    final <- data.frame(tabla )

    # Armar tabla completa con todos los insumos
    final <- dplyr::bind_cols(final, "gl" = gl , "n" = n, "coef_var" = cv[1])
    names(final)[2] <- "se"

  }


  return(final)

}

#--------------------------------------------------------------------

#' Crea los insumos necesarios para hacer la evaluación de estimadores de totales
#'
#' Genera una tabla con los siguientes insumos: total expandido, grados de libertad,
#' tamaño muestral y coeficiente de variación. La función contempla la posibilidad de desagregar la estimación en uno o más dominios.
#'
#' @param var string. Variable para la cual se quiere calcular un total. Pueden introducirse varias variables separadas por un +, para obtener más totales en la tabla
#' @param dominios string. Dominios de estimación separados por signo +.
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' crear_insumos_tot(ocupado, zona+sexo, dc)
#' @export

crear_insumos_tot <- function(var, dominios = NULL, disenio) {


  # ESTO CORRESPONDE AL CASO CON DESAGREGACIÓN
  if (!is.null(rlang::enexprs(dominios)[[1]])) {

    # Generar un string con el nombre de la variable
    var_string <-  rlang::expr_name(rlang::enexpr(var))

    # Verificar que la variabe de entrada es correcta
    if (!is.numeric(disenio$variables[[var_string]])) stop("¡La variable debe ser numérica!")

    # Verificar que la variable es dummy
    test <- disenio$variable %>%
      dplyr::mutate(test = dplyr::if_else(!!rlang::enquo(var) == 1 | !!rlang::enquo(var) == 0, 1, 0)) %>%
      dplyr::summarise(pasa = sum(test))

    n_filas <- nrow(disenio$variable)
    if (n_filas != test$pasa) stop("¡Debes usar una variable dummy cuando desagregas!")


    #Identificar las variables ingresadas para la desagregación
    agrupacion <- rlang::expr_name(rlang::enexprs(dominios)[[1]]) %>%
      stringr::str_split(pattern = "\\+")
    agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
    agrup1 <- c(agrupacion, var_string)

    # Agregar ~ para adecuar a formato de survey
    dominios_form <- paste0("~", rlang::enexprs(dominios)) %>%
      as.formula()

    var_formula <- paste0("~", rlang::enexpr(var)) %>%
      as.formula()

    # Generar la tabla de estimaciones
    tabla1 <- survey::svyby(formula = var_formula, by = dominios_form, design = disenio, FUN = survey::svytotal)

    gl <- calcular_upm(disenio$variables, agrup1) %>%
      dplyr::left_join(calcular_estrato(disenio$variables, agrup1), by = agrup1) %>%
      dplyr::mutate(gl = upm - varstrat)  %>%
      dplyr::filter(!!rlang::enquo(var) == 1)  %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    cv <- survey::cv(tabla1, design = disenio) %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "variable") %>%
      tidyr::separate(variable, agrupacion) %>%
      dplyr::rename(coef_var = ".") %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    n <- calcular_n(disenio$variables, dominios = agrup1) %>%
      dplyr::filter(!!rlang::enquo(var) == 1) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    # Unir todo y generar la tabla final
    final <- tabla1 %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n")),
                       by = agrupacion) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl")),
                       by = agrupacion) %>%
      dplyr::left_join(cv, by = agrupacion)

  # ESTO CORRESPONDE AL CASO SIN DESAGRAGACIÓN
  } else {

    # Identificar las variables ingresadas por el usuario
    agrupacion <- rlang::expr_name(rlang::enexprs(var)[[1]]) %>%
      stringr::str_split(pattern = "\\+")
    agrup1 <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")


    # Convertir variables a string para homologar criterios
    disenio <- survey::svydesign(ids = ~varunit, strata = ~varstrat,
                         data = disenio$variables %>% dplyr::mutate_at(.vars = dplyr::vars(agrup1), list(as.character)),
                         weights = ~fe)

    # Acomodar a formato de survey
    var_formula <- paste0("~", rlang::enexprs(var)) %>%
      as.formula()

    # Tabla que se usa luego para calcular cv
    tabla1 <- survey::svytotal(x = var_formula, design = disenio )

    # Tabla con los totales
    totales <- as.data.frame(tabla1) %>%
      tibble::rownames_to_column(var = "variable")

    # Tamaño muestral
    n <- purrr::map(agrup1, calcular_n_total, datos = disenio$variables) %>%
      purrr::reduce(dplyr::bind_rows)

    # Grados de libertad
    gl <- calcular_gl_total(agrup1, disenio$variables)

    #Extrear el coeficiente de variación
    cv <- cv(tabla1, design = disenio) * 100
    cv <- as.data.frame(cv) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::rename(coef_var = cv)

    # COnstruir tabla final
    final <- totales %>%
      dplyr::left_join(n, by = "variable") %>%
      dplyr::left_join(gl %>% dplyr::select(-upm, -varstrat), by = "variable") %>%
      dplyr::left_join(cv, by = "variable")

  }
  names(final) <- tolower(names(final))
  return(final)

}


#-----------------------------------------------------------------------

#' Crea los insumos necesarios para hacer la evaluación de estimadores de proporción
#'
#' Genera una tabla con los siguientes insumos: media, grados de libertad,
#' tamaño muestral y error estándar. La función contempla la posibilidad de desagregar la estimación en uno o más dominios.
#'
#' @param var variable objetivo dentro de un \code{dataframe}.
#' @param dominios dominios de estimación separados por signo +.
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' crear_insumos_prop(ocupado, zona+sexo, dc)
#' @export


crear_insumos_prop <- function(var, dominios = NULL, disenio) {

  #Chequear que la variable sea de proporción. Si no lo es, se interrumpe la ejecución
  enquo_var <-  rlang::enquo(var)
  es_prop <- disenio$variables %>%
    dplyr::mutate(es_prop = dplyr::if_else(!!enquo_var == 1 | !!enquo_var == 0, 1, 0))

  if (sum(es_prop$es_prop) != nrow(disenio$variables)) stop("¡La variable no es de proporción!")


  #COnvertir los inputs en fórmulas para adecuarlos a survey
  var <- paste0("~", rlang::enexpr(var)) %>%
    as.formula()

  # ESTO CORRESPONDE AL CASO CON DESAGREGACIÓN
  if (!is.null(rlang::enexprs(dominios)[[1]])) {

  dominios <- paste0("~", rlang::enexprs(dominios)) %>%
    as.formula()

  #Generar la tabla con los cálculos
  tabla <- calcular_tabla(var, dominios, disenio)

  #Extraer nombres
  nombres <- names(tabla)
  agrupacion <-  nombres[c(-(length(nombres) - 1), -length(nombres)) ]
  var_prop <- nombres[length(nombres) - 1]

  #Calcular el tamaño muestral de cada grupo
  n <- calcular_n(disenio$variables, agrupacion, var_prop )

  #Calcular los grados de libertad de todos los cruces
  gl <- calcular_upm(disenio$variables, agrupacion, var_prop) %>%
    dplyr::left_join(calcular_estrato(disenio$variables, agrupacion, var_prop), by = agrupacion) %>%
    dplyr::mutate(gl = upm - varstrat)

  #Unir toda la información. Se hace con join para asegurar que no existan problemas en la unión
  final <- tabla %>%
    dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl" )),
              by = agrupacion) %>%
    dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n" )),
              by = agrupacion)

  #Cambiar el nombre de la variable objetivo para que siempre sea igual.
  final <- final  %>%
    dplyr::rename(objetivo = var_prop)

  # ESTO CORRESPONDE AL CASO SIN DESAGREGACIÓN
  } else {

  #Generar la tabla con los cálculos
  tabla <- calcular_tabla(var, dominios, disenio)

  # Tamaño muestral
  n <- nrow(disenio$variables)

  # Calcular grados de libertad
  varstrat <- length(unique(disenio$variables$varstrat))
  varunit <- length(unique(disenio$variables$varunit))
  gl <- varunit - varstrat

  # Calcular coeficiente de variación
  cv <- cv(tabla, design = disenio) * 100

  # Armar tabla final
  final <- data.frame(tabla )

  # Armar tabla completa con todos los insumos
  final <- dplyr::bind_cols(final, "gl" = gl, "n" = n)
  names(final)[2] <- "se"

  #Cambiar el nombre de la variable objetivo para que siempre sea igual
  final <- final %>%
    dplyr::rename(objetivo = mean)

  }

  return(final)

}







