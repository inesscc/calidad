
#-----------------------------------------------------------------------

#' Homologa nombre de variable que hace referencia a los conglomerados, con el objetivo de evitar posible errores.
#'
#' Identifica el nombre de la variable asignada para los conglomerados en el diseño complejo, lo que permite reasignar variable con nombre estandar utilizado por las 4 funciones de creación de insumos.
#'
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#'
#' @return \code{vector} que contiene la variable con los conglomerados.
#' @import survey
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' unificar_variables_estrato(dc)

unificar_variables_upm = function(disenio){
  stringr::str_replace(paste(disenio$call)[2],"~","")
}

#-----------------------------------------------------------------------

#' Homologa nombre de variable que hace referencia a los estratos de conglomerados, con el objetivo de evitar posible errores.
#'
#' Identifica el nombre de la variable asignada para los estratos de conglomerados en el diseño complejo, lo que permite reasignar variable con nombre estandar utilizado por las 4 funciones de creación de insumos.
#'
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#'
#' @return \code{vector} que contiene la variable con los estratos de conglomerados.
#' @import survey
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' unificar_variables_estrato(dc)

### función par homologar variables estratos ####
unificar_variables_estrato = function(disenio){
  stringr::str_replace(paste(disenio$call)[3],"~","")
}

#-----------------------------------------------------------------------

#' Homologa nombre de variable que hace referencia al factor de expansión utilizado por el usuario, con el objetivo de evitar posible errores.
#'
#' Identifica el nombre de la variable asignada para el factor de expansión en el diseño complejo, lo que permite reasignar variable con nombre estandar utilizado por las 4 funciones de creación de insumos.
#'
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#'
#' @return \code{vector} que contiene la variable con los datos del factor de expansión.
#' @import survey
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' unificar_variables_estrato(dc)

### función par homologar variables factor expansión ####
unificar_variables_factExp = function(disenio){
  stringr::str_replace(paste(disenio$call)[5],"~","")
}

#-----------------------------------------------------------------------


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
calcular_tabla <-  function(var, dominios, disenio, media = T) {

  # El primer if es para dominios
  if (!is.null(dominios)) {
    if (media == T) { # para calcular la media
      estimacion <- survey::svyby(var ,
                                  design = disenio,
                                  by = dominios,
                                  FUN = svymean)
    } else { # para calcular la mediana

      estimacion <- median <- survey::svyby(var,
                                            by = dominios,
                                            FUN = survey::svyquantile,
                                            design = disenio,
                                            quantile = 0.5,
                                            method="constant",
                                            interval.type = "quantile",
                                            ties="discrete")
    }
  # Esto corresponde al caso sin desagregación
  } else {
    if (media == T) { # para calcular la media
      estimacion <- survey::svymean(var, disenio)
    } else { # para calcular la mediana

      estimacion <-  svyquantile(var,
                                  design = disenio,
                                  quantile = 0.5,
                                  method="constant",
                                  interval.type = "quantile",
                                  ties="discrete")
    }

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

#----------------------------------------------------------------------
#' Chequea que las variables de diseño tengan el nombre correcto
#'
#' Comprueba que las variables de diseño se llamen varstrat y varunit. En caso de que no se cumpla, la ejecución se detiene y se genera un error
#'
#' @param data \code{dataframe} que contiene la tabla con la cual se está trabajando
#' @return un mensaje de error
#'
#' @examples
#' chequear_var_disenio(data = var = dc$variables)


chequear_var_disenio <- function(data) {

  if (sum(grepl(pattern = "varunit" , x = names(data))) == 0) {
    stop("¡La columna que contiene información de las UPMs debe llamarse varunit!")
  }

  if (sum(grepl(pattern = "varstrat" , x = names(data))) == 0) {
    stop("¡La columna que contiene información de los estratos debe llamarse varstrat!")
  }

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
  if (sum(grepl(pattern = "varunit" , x = names(data))) == 0) {
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
  if (sum(grepl(pattern = "varstrat" , x = names(data))) == 0) {
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



#------------------------------

#' Genera intervalos de confianza para todos los dominios estimados
#'
#' Usa la tabla creada para calcular el estándar y le agrega dos columnas con el límite inferior y superior del intervalo de confianza
#'
#' @param data \code{dataframe} con todos los datos necesarios para calcular el estándar
#' @param env \code{environment} toma el ambiente de la función contenedora, para usar los elementos requeridos
#' @param tipo \code{string} que indica cuál es el tipo de estimación que se realiza.
#' @return \code{dataframe} que contiene todos los elementos del estándar, junto a tres columnas nuevas que contienen el límite inferior, el límite superior y el valor t
#'
#' @examples
#' calcular_intervalos(tabla, tipo = media_agregado)


calcular_ic <-  function(data, env = parent.frame(), tipo = "resto") {

  est <- switch(tipo, "resto" =  get("var_string", env),
                "media_agregado" = "mean",
                "prop_agregado" = "objetivo",
                "total_agregado" = "total")

  final <- data %>%
    dplyr::mutate(t = qt(c(.975), df = gl),
                  li = !!rlang::parse_expr(est) - se*t,
                  ls = !!rlang::parse_expr(est) + se*t)

  return(final)

}

#--------------------------------------------------------------------
#' Crea los insumos necesarios para hacer la evaluación de estimadores de la media
#'
#' Genera una tabla con los siguientes insumos: media, grados de libertad,
#' tamaño muestral y coeficiente de variación. La función contempla la posibilidad de desagregar la estimación en uno o más dominios.
#'
#' @param var variable objetivo dentro de un \code{dataframe}.
#' @param dominios dominios de estimación separados por signo +.
#' @param subpop integer dummy que permite filtrar por una subpoblación
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#' @param ci \code{boolean} que indica si los intervalos de confianza deben calcularse
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' crear_insumos(gastot_hd, zona+sexo, dc)
#' @export

crear_insumos_media <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F) {

  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]

  # Encapsular inputs para usarlos después
  enquo_var <-  rlang::enquo(var)
  var_string <-  rlang::expr_name(rlang::enexpr(var))

  # Chequear que la variable no sea character
  if (is.character(disenio$variables[[var_string]]) == T) stop("¡Estás usando una variable character!")

  #Chequear que la variable sea continua. Si no lo es, aparece un warning
  es_prop <- disenio$variables %>%
    dplyr::mutate(es_prop = dplyr::if_else(!!enquo_var == 1 | !!enquo_var == 0, 1, 0))

  if (sum(es_prop$es_prop) == nrow(disenio$variables)) warning("¡Parece que tu variable es de proporción!")


  #Convertir los inputs en fórmulas para adecuarlos a survey
  var <- paste0("~", rlang::enexpr(var)) %>%
    as.formula()

  # ESTO CORRESPONDE AL CASO CON DESAGREGACIÓN
  if (!is.null(rlang::enexprs(dominios)[[1]])) {

    # Esto corre para el caso en el que NO hay subpop
    if (is.null(rlang::enexpr(subpop))) {

      dominios <- paste0("~", rlang::enexprs(dominios)) %>%
        as.formula()

      #Generar la tabla con los cálculos
      tabla <- calcular_tabla(var, dominios, disenio)

      # Esto corre para subpop
    } else if (!is.null(rlang::enexpr(subpop))) { # caso que tiene subpop

      # Chequear que la variable de subpop es una dummy. Si no se cumple, se interrumpe la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::enquo(subpop)  == 1 | !!rlang::enquo(subpop) == 0, 1, 0))
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      dominios <-   paste(rlang::enexprs(dominios), rlang::enexprs(subpop), sep = "+")
      dominios <- paste0("~", dominios) %>%
        as.formula()

      #Generar la tabla con los cálculos
      tabla <- calcular_tabla(var, dominios, disenio) %>%
        dplyr::filter(!!rlang::enquo(subpop) == 1)
    }

    #Extraer nombres
    nombres <- names(tabla)
    agrupacion <-  nombres[c(-(length(nombres) - 1), -length(nombres)) ]

    #Calcular el tamaño muestral de cada grupo
    n <- calcular_n(disenio$variables, agrupacion) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    #Calcular los grados de libertad de todos los cruces
    gl <- calcular_upm(disenio$variables, agrupacion) %>%
      dplyr::left_join(calcular_estrato(disenio$variables, agrupacion), by = agrupacion) %>%
      dplyr::mutate(gl = upm - varstrat) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)


    #Extrear el coeficiente de variación
    cv <- cv(tabla, design = disenio) * 100

    cv <- tabla %>%
      dplyr::select(agrupacion) %>%
      dplyr::bind_cols(coef_var = cv) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    #Unir toda la información. Se hace con join para asegurar que no existan problemas en la unión
    final <- tabla %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl")),
                       by = agrupacion) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n")),
                       by = agrupacion) %>%
      dplyr::left_join(cv %>% dplyr::select(c(agrupacion, "coef_var")),
                       by = agrupacion)

    # Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final)
    }

    # ESTO CORRESPONDE AL CASO SIN DESAGREGACIÓN
  } else {


    # Si el usuario ingresa subpoblación, se filtra la base de datos para la subpoblación de referencia
    if (!is.null(rlang::enexpr(subpop))) {

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::enquo(subpop)  == 1 | !!rlang::enquo(subpop) == 0, 1, 0))
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      # Aquí se filtra el diseño
      subpop_text <- rlang::expr_text(rlang::enexpr(subpop))
      disenio <- disenio[disenio$variables[[subpop_text]] == 1]
      #ff

    }

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
    final <- data.frame(tabla)

    # Armar tabla completa con todos los insumos
    final <- dplyr::bind_cols(final, "gl" = gl , "n" = n, "coef_var" = cv[1])
    names(final)[2] <- "se"

    # Se calcular el intervalo de confianza solo si el usuario lo pide
    if (ci == T) {
      final <- calcular_ic(data = final, tipo = "media_agregado")
    }


  }


  return(final)

}


#--------------------------------------------------------------------

#' Crea los insumos para el caso específico de estimaciones de suma
#'
#' Genera una tabla con los siguientes insumos: suma, grados de libertad,
#' tamaño muestral y coeficiente de variación. La función tiene la posibilidad de utilizar una subpoblación. La variable de subpoblación debe ser dummy.
#'
#' @param var variable objetivo dentro de un \code{dataframe}.
#' @param dominios dominios de estimación separados por signo +.
#' @param subpop integer dummy que permite filtrar por una subpoblación
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#' @param ci \code{boolean} que indica si los intervalos de confianza deben calcularse
#' @return \code{dataframe} que contiene todos los insumos necesarios para evaluar una suma
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' crear_insumos_suma(gastot_hd, zona+sexo, subpop = ocupado, disenio = dc)
#' @export

crear_insumos_tot_con <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F) {

  # chequear_var_disenio(disenio$variables)

  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]

  # COnvertir a string todas las variables de entrada
  var_string <-  rlang::expr_name(rlang::enexpr(var)) %>%
    stringr::str_remove("~")
  dominios_string <-  rlang::expr_name(rlang::enexpr(dominios))%>%
    stringr::str_remove("~")
  subpop_string <-  rlang::expr_name(rlang::enexpr(subpop))%>%
    stringr::str_remove("~")

  # Verificar que la variable de estimación sea numérica. Se interrumpe si no es numérica
  if (!is.numeric(disenio$variables[[var_string]]) ) stop("Debes usar una variable numérica")


  # Pasar la variable objetivo al formato de survey
  var_formula <- paste0("~", var_string) %>%
    as.formula()

  # ESTO CORRESPONDE AL CASO EN EL QUE HAY DESAGREGACIÓN
  if (dominios_string != "NULL") {

    # Esto corre para el caso en el que NO hay subpop
    if (subpop_string == "NULL") {


      #Identificar las variables ingresadas para la desagregación
      agrupacion <- dominios_string %>%
        stringr::str_split(pattern = "\\+")

      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrup1 <- c(agrupacion, var_string)

      # Agregar ~ para adecuar a formato de survey
      dominios_form <- paste0("~", dominios_string) %>%
        as.formula()

      # Esto corre para subpop
    } else if (subpop_string != "NULL") { # caso que tiene subpop

      # Chequear que la variable de subpop es una dummy. Si no se cumple, se interrumpe la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::enquo(subpop)  == 1 | !!rlang::enquo(subpop) == 0, 1, 0))
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      #Identificar las variables ingresadas para la desagregación
      agrupacion <- dominios_string %>%
        stringr::str_split(pattern = "\\+")
      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrupacion <- c(subpop_string, agrupacion)
      agrup1 <- c(agrupacion, var_string)


      dominios_form <- paste(subpop_string, dominios_string, sep =  "+")
      dominios_form <- paste0("~", dominios_form) %>%
        as.formula()

    }


    tabla <- survey::svyby(formula = var_formula, by = dominios_form, design = disenio, FUN = survey::svytotal)

    gl <- calcular_upm(disenio$variables, agrupacion) %>%
      dplyr::left_join(calcular_estrato(disenio$variables, agrupacion), by = agrupacion) %>%
      dplyr::mutate(gl = upm - varstrat)  %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    cv <- survey::cv(tabla, design = disenio) %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "variable") %>%
      tidyr::separate(variable, agrupacion) %>%
      dplyr::rename(coef_var = ".") %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character) %>%
      dplyr::mutate(coef_var = coef_var * 100)

    n <- calcular_n(disenio$variables, dominios = agrupacion) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    # Unir todo y generar la tabla final
    final <- tabla %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n")),
                       by = agrupacion) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl")),
                       by = agrupacion) %>%
      dplyr::left_join(cv, by = agrupacion)

    #Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final)
    }


    # ESTE ES EL CASO NO AGREGADO
  } else {

    tabla <- survey::svytotal(x = var_formula, design = disenio )

    # Tabla con los totales
    totales <- as.data.frame(tabla) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::rename(se = var_string)


    # Tamaño muestral
    n <- nrow(disenio$variables) %>%
      as.data.frame() %>%
      dplyr::mutate(variable = var_string) %>%
      dplyr::rename(n = ".")

    # Grados de libertad
    upm <- length(unique(disenio$variables$varunit))
    varstrat <- length(unique(disenio$variables$varstrat))
    gl <- cbind(upm, varstrat)
    gl <- gl %>%
      as.data.frame() %>%
      dplyr::mutate(variable = var_string,
                    gl =  upm - varstrat)

    # Coeficiente de variación
    cv <- cv(tabla, design = disenio) * 100
    cv <- as.data.frame(cv) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::rename(coef_var = var_string)

    # COnstruir tabla final
    final <- totales %>%
      dplyr::left_join(n, by = "variable") %>%
      dplyr::left_join(gl %>% dplyr::select(-upm, -varstrat), by = "variable") %>%
      dplyr::left_join(cv, by = "variable")


    # Se calcula el intervalo de confianza solo si el usuario lo pide
    if (ci == T) {
      # intervalos <- tabla %>%
      #   confint(df = degf(disenio)) %>%
      #   as.data.frame() %>%
      #   dplyr::rename(li = "2.5 %",
      #                 ls = "97.5 %") %>%
      #   tibble::remove_rownames()
      # final <- final %>%
      #   bind_cols(intervalos)
      final <- calcular_ic(data = final, tipo = "total_agregado")
    }


  }


  final
}



#--------------------------------------------------------------------

#' Crea los insumos necesarios para hacer la evaluación de estimadores de totales
#'
#' Genera una tabla con los siguientes insumos: total expandido, grados de libertad,
#' tamaño muestral y coeficiente de variación. La función contempla la posibilidad de desagregar la estimación en uno o más dominios.
#'
#' @param var string. Variable para la cual se quiere calcular un total. Pueden introducirse varias variables separadas por un +, para obtener más totales en la tabla
#' @param dominios string. Dominios de estimación separados por signo +.
#' @param subpop integer dummy que permite filtrar por una subpoblación
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#' @param ci \code{boolean} que indica si los intervalos de confianza deben calcularse
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' crear_insumos_tot(ocupado, zona+sexo, dc)
#' @export

crear_insumos_tot <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F) {

  # Generar un string con el nombre de la variable. Se usa más adelante
  var_string <-  rlang::expr_name(rlang::enexpr(var))

  # Chequar que estén presentes las variables del diseño muestral. Si no se llaman varstrat y varunit, se
  #  detiene la ejecución
  # chequear_var_disenio(disenio$variables)

  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]
  disenio$variables$fe = disenio$variables[[unificar_variables_factExp(disenio)]]

  # ESTO CORRESPONDE AL CASO CON DESAGREGACIÓN
  if (!is.null(rlang::enexprs(dominios)[[1]])) {

    # Verificar que la variabe de entrada es correcta
    if (!is.numeric(disenio$variables[[var_string]])) stop("¡La variable debe ser numérica!")

    # Verificar que la variable es dummy
    test <- disenio$variable %>%
      dplyr::mutate(test = dplyr::if_else(!!rlang::enquo(var) == 1 | !!rlang::enquo(var) == 0, 1, 0)) %>%
      dplyr::summarise(pasa = sum(test))

    n_filas <- nrow(disenio$variable)
    if (n_filas != test$pasa) stop("¡Debes usar una variable dummy cuando desagregas!")


    # Esto corre para el caso en el que NO hay subpop
    if (is.null(rlang::enexpr(subpop))) {

      #Identificar las variables ingresadas para la desagregación
      agrupacion <- rlang::expr_name(rlang::enexprs(dominios)[[1]]) %>%
        stringr::str_split(pattern = "\\+")
      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrup1 <- c(agrupacion, var_string)

      # Agregar ~ para adecuar a formato de survey
      dominios_form <- paste0("~", rlang::enexprs(dominios)) %>%
        as.formula()

      # Esto corre para subpop
    } else if (!is.null(rlang::enexpr(subpop))) { # caso que tiene subpop

      # Chequear que la variable de subpop es una dummy. Si no se cumple, se interrumpe la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::enquo(subpop)  == 1 | !!rlang::enquo(subpop) == 0, 1, 0))
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      #Identificar las variables ingresadas para la desagregación
      agrupacion <- rlang::expr_name(rlang::enexprs(dominios)[[1]]) %>%
        stringr::str_split(pattern = "\\+")
      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrupacion <- c(rlang::quo_text(rlang::enquo(subpop)), agrupacion  )
      agrup1 <- c(agrupacion, var_string)

      #dominios_form <- paste(agrupacion, "+")
      dominios_form <- paste(rlang::enexpr(subpop), rlang::enexprs(dominios), sep =  "+")
      dominios_form <- paste0("~", dominios_form) %>%
        as.formula()

    }
    # Pasar a la variable objetivo al formato de survey
    var_formula <- paste0("~", rlang::enexpr(var)) %>%
      as.formula()

    # Generar la tabla de estimaciones
    tabla <- survey::svyby(formula = var_formula, by = dominios_form, design = disenio, FUN = survey::svytotal)

    gl <- calcular_upm(disenio$variables, agrup1) %>%
      dplyr::left_join(calcular_estrato(disenio$variables, agrup1), by = agrup1) %>%
      dplyr::mutate(gl = upm - varstrat)  %>%
      dplyr::filter(!!rlang::enquo(var) == 1)  %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)


    cv <- survey::cv(tabla, design = disenio) %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "variable") %>%
      tidyr::separate(variable, agrupacion) %>%
      dplyr::rename(coef_var = ".") %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character) %>%
      dplyr::mutate(coef_var = coef_var * 100)


    n <- calcular_n(disenio$variables, dominios = agrup1) %>%
      dplyr::filter(!!rlang::enquo(var) == 1) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    # Unir todo y generar la tabla final
    final <- tabla %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n")),
                       by = agrupacion) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl")),
                       by = agrupacion) %>%
      dplyr::left_join(cv, by = agrupacion)


    #Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final)
    }


    # ESTO CORRESPONDE AL CASO SIN DESAGRAGACIÓN
  } else {

    # Identificar las variables ingresadas por el usuario
    agrupacion <- rlang::expr_name(rlang::enexprs(var)[[1]]) %>%
      stringr::str_split(pattern = "\\+")
    agrup1 <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")


    # Convertir variables a string. Esto se hace debido a que survey tiene distintos tratamientos para variables numéricas o de string
    disenio <- survey::svydesign(ids = ~varunit, strata = ~varstrat,
                                 data = disenio$variables %>% dplyr::mutate_at(.vars = dplyr::vars(agrup1), list(as.character)),
                                 weights = ~fe)

    # Acomodar a formato de survey
    var_formula <- paste0("~", rlang::enexprs(var)) %>%
      as.formula()

    # Si el usuario ingresa subpoblación, se filtra la base de datos para la subpoblación de referencia
    if (!is.null(rlang::enexpr(subpop))) {

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::enquo(subpop)  == 1 | !!rlang::enquo(subpop) == 0, 1, 0))
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      # Aquí se filtra el diseño
      subpop_text <- rlang::expr_text(rlang::enexpr(subpop))
      disenio <- disenio[disenio$variables[[subpop_text]] == 1]

    }


    # Tabla que se usa luego para calcular cv
    tabla <- survey::svytotal(x = var_formula, design = disenio )

    # Tabla con los totales
    totales <- as.data.frame(tabla) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::rename(se = SE)


    # Tamaño muestral
    n <- purrr::map(agrup1, calcular_n_total, datos = disenio$variables) %>%
      purrr::reduce(dplyr::bind_rows)

    # Grados de libertad
    gl <- calcular_gl_total(agrup1, disenio$variables)

    #Extrear el coeficiente de variación
    cv <- cv(tabla, design = disenio) * 100
    cv <- as.data.frame(cv) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::rename(coef_var = cv)

    # COnstruir tabla final
    final <- totales %>%
      dplyr::left_join(n, by = "variable") %>%
      dplyr::left_join(gl %>% dplyr::select(-upm, -varstrat), by = "variable") %>%
      dplyr::left_join(cv, by = "variable")


    # Se calcula el intervalo de confianza solo si el usuario lo pide
    if (ci == T) {
      final <- calcular_ic(data = final, tipo = "total_agregado")

    }

  }

  # Las filas en las que no exsiten casos generan valores NA. Esos casos se eliminan
  names(final) <- tolower(names(final))
  final <- final %>%
    dplyr::filter(!is.nan(coef_var))

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
#' @param subpop integer dummy que permite filtrar por una subpoblación
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#' @param ci \code{boolean} que indica si los intervalos de confianza deben calcularse
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' crear_insumos_prop(ocupado, zona+sexo, dc)
#' @export

crear_insumos_prop <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F) {
  # Chequar que estén presentes las variables del diseño muestral. Si no se llaman varstrat y varunit, se
  #  detiene la ejecución
  # chequear_var_disenio(disenio$variables)
  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]

  # Encapsular inputs para usarlos más tarde
  var_string <-  rlang::expr_name(rlang::enexpr(var))
  enquo_var <-  rlang::enquo(var)

  # Chequear que la variable no sea character
  if (is.character(disenio$variables[[var_string]]) == T) stop("¡Estás usando una variable character!")

  #Chequear que la variable sea de proporción. Si no lo es, se interrumpe la ejecución
  es_prop <- disenio$variables %>%
    dplyr::mutate(es_prop_var = dplyr::if_else(!!enquo_var == 1 | !!enquo_var == 0 | is.na(!!enquo_var), 1, 0))

  if (sum(es_prop$es_prop_var) != nrow(es_prop)) stop("¡La variable no es de proporción!")

  #COnvertir los inputs en fórmulas para adecuarlos a survey
  var <- paste0("~", rlang::enexpr(var)) %>%
    as.formula()

  # ESTO CORRESPONDE AL CASO CON DESAGREGACIÓN
  if (!is.null(rlang::enexprs(dominios)[[1]])) {

    # Esto corre para el caso en el que NO hay subpop
    if (is.null(rlang::enexpr(subpop))) {
      dominios <- paste0("~", rlang::enexprs(dominios)) %>%
        as.formula()

      # Esto corre para subpop
    } else if (!is.null(rlang::enexpr(subpop))) { # caso que tiene subpop

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::enquo(subpop)  == 1 | !!rlang::enquo(subpop) == 0, 1, 0))

      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      dominios <- paste(rlang::enexprs(dominios), rlang::enexpr(subpop), sep = "+")
      dominios <- paste0("~", dominios) %>%
        as.formula()
    }

    #Generar la tabla con los cálculos
    tabla <- calcular_tabla(var, dominios, disenio)

    #Extraer nombres
    nombres <- names(tabla)
    agrupacion <-  nombres[c(-(length(nombres) - 1), -length(nombres)) ]
    var_prop <- nombres[length(nombres) - 1]

    #Calcular el tamaño muestral de cada grupo
    n <- calcular_n(disenio$variables, agrupacion) %>%
      dplyr::mutate_at(dplyr::vars(agrupacion), as.character)


    #Calcular los grados de libertad de todos los cruces
    gl <- calcular_upm(disenio$variables, agrupacion) %>%
      dplyr::left_join(calcular_estrato(disenio$variables, agrupacion), by = agrupacion) %>%
      dplyr::mutate(gl = upm - varstrat) %>%
      dplyr::mutate_at(dplyr::vars(agrupacion), as.character)

    #Unir toda la información. Se hace con join para asegurar que no existan problemas en la unión
    final <- tabla %>%
      dplyr::mutate_at(dplyr::vars(agrupacion), as.character) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl" )),
                       by = agrupacion) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n" )),
                       by = agrupacion)


    #Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final)
    }

    #Cambiar el nombre de la variable objetivo para que siempre sea igual.
    final <- final  %>%
      dplyr::rename(objetivo = var_prop) %>%
      dplyr::filter(objetivo > 0) # se eliminan los ceros de la tabla




    # ESTO CORRESPONDE AL CASO SIN DESAGREGACIÓN
  } else {

    # Si el usuario ingresa subpoblación, se filtra la base de datos para la subpoblación de referencia
    if (!is.null(rlang::enexpr(subpop))) {

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::enquo(subpop)  == 1 | !!rlang::enquo(subpop) == 0, 1, 0))
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      # Aquí se filtra el diseño
      subpop_text <- rlang::expr_text(rlang::enexpr(subpop))
      disenio <- disenio[disenio$variables[[subpop_text]] == 1]

    }

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


    # Se calcula el intervalo de confianza solo si el usuario lo pide
    if (ci == T) {
      final <- calcular_ic(data = final, tipo = "prop_agregado")

    }

  }

  return(final)

}



#-----------------------------------------------------------------------

#' Crea los insumos necesarios para hacer la evaluación de estimación de mediana
#'
#' Genera una tabla con los siguientes insumos: mediana, grados de libertad,
#' tamaño muestral y error estándar. La función contempla la posibilidad de desagregar la estimación en uno o más dominios.
#'
#' @param var variable objetivo dentro de un \code{dataframe}.
#' @param dominios dominios de estimación separados por signo +.
#' @param subpop integer dummy que permite filtrar por una subpoblación
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#' @param ci \code{boolean} que indica si los intervalos de confianza deben calcularse
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' dc_rep <-  as.svrepdesign(dc , type = "subbootstrap", replicates=10)
#' crear_insumos_mediana(gastot_hd, zona+sexo, dc)
#' @export


crear_insumos_mediana <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F, replicas = 10) {


  # Ajustar nombre de variables del diseño muestral
  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]

  # Generar el diseño replicado
  disenio <-  as.svrepdesign(disenio, type = "subbootstrap", replicates = replicas)

  # Encapsular inputs para usarlos después
  enquo_var <-  rlang::enquo(var)
  var_string <-  rlang::expr_name(rlang::enexpr(var))

  # Chequear que la variable no sea character
  if (is.character(disenio$variables[[var_string]]) == T) stop("¡Estás usando una variable character!")

  #Chequear que la variable sea continua. Si no lo es, aparece un warning
  es_prop <- disenio$variables %>%
    dplyr::mutate(es_prop = dplyr::if_else(!!enquo_var == 1 | !!enquo_var == 0, 1, 0))

  if (sum(es_prop$es_prop) == nrow(disenio$variables)) warning("¡Parece que tu variable es de proporción!")


  #Convertir los inputs en fórmulas para adecuarlos a survey
  var <- paste0("~", rlang::enexpr(var)) %>%
    as.formula()

  # ESTO CORRESPONDE AL CASO CON DESAGREGACIÓN
  if (!is.null(rlang::enexprs(dominios)[[1]])) {

    # Esto corre para el caso en el que NO hay subpop
    if (is.null(rlang::enexpr(subpop))) {

      dominios <- paste0("~", rlang::enexprs(dominios)) %>%
        as.formula()

      #Generar la tabla con los cálculos
      tabla <- calcular_tabla(var, dominios, disenio, media = F)

      # Esto corre para subpop
    } else if (!is.null(rlang::enexpr(subpop))) { # caso que tiene subpop

      # Chequear que la variable de subpop es una dummy. Si no se cumple, se interrumpe la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::enquo(subpop)  == 1 | !!rlang::enquo(subpop) == 0, 1, 0))
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      dominios <-   paste(rlang::enexprs(dominios), rlang::enexprs(subpop), sep = "+")
      dominios <- paste0("~", dominios) %>%
        as.formula()

      #Generar la tabla con los cálculos
      tabla <- calcular_tabla(var, dominios, disenio, media = F) %>%
        dplyr::filter(!!rlang::enquo(subpop) == 1)
    }

    #Extraer nombres
    nombres <- names(tabla)
    agrupacion <-  nombres[c(-(length(nombres) - 1), -length(nombres)) ]

    #Calcular el tamaño muestral de cada grupo
    n <- calcular_n(disenio$variables, agrupacion) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    #Calcular los grados de libertad de todos los cruces
    gl <- calcular_upm(disenio$variables, agrupacion) %>%
      dplyr::left_join(calcular_estrato(disenio$variables, agrupacion), by = agrupacion) %>%
      dplyr::mutate(gl = upm - varstrat) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)


    #Extrear el coeficiente de variación
    cv <- cv(tabla, design = disenio) * 100

    cv <- tabla %>%
      dplyr::select(agrupacion) %>%
      dplyr::bind_cols(coef_var = cv) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    #Unir toda la información. Se hace con join para asegurar que no existan problemas en la unión
    final <- tabla %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl")),
                       by = agrupacion) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n")),
                       by = agrupacion) %>%
      dplyr::left_join(cv %>% dplyr::select(c(agrupacion, "coef_var")),
                       by = agrupacion) %>%
      dplyr::rename(!!enquo_var := V1)

    # Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final)
    }

    # ESTO CORRESPONDE AL CASO SIN DESAGREGACIÓN
  } else {


    # Si el usuario ingresa subpoblación, se filtra la base de datos para la subpoblación de referencia
    if (!is.null(rlang::enexpr(subpop))) {

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::enquo(subpop)  == 1 | !!rlang::enquo(subpop) == 0, 1, 0))
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      # Aquí se filtra el diseño
      subpop_text <- rlang::expr_text(rlang::enexpr(subpop))
      disenio <- disenio[disenio$variables[[subpop_text]] == 1]
      #ff

    }

    #Generar la tabla con los cálculos
    tabla <- calcular_tabla(var, dominios, disenio, media = F)

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

    # Se calcular el intervalo de confianza solo si el usuario lo pide
    if (ci == T) {
      final <- calcular_ic(data = final, tipo = "media_agregado")
    }


  }


  return(final)


  }

