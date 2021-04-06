
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
#'
calcular_tabla <-  function(var, dominios, disenio, media = T) {

  # El primer if es para dominios
  if (!is.null(dominios)) {
    if (media == T) { # para calcular la media
      estimacion <- survey::svyby(var ,
                                  design = disenio,
                                  by = dominios,
                                  FUN = svymean)
    } else { # para calcular la mediana

      estimacion <- survey::svyby(var,
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

#' Calcula ratio a partir de cierta agregación
#'
#' Genera una tabla con estimaciones para una agregación determinada
#'
#' @param var variable objetivo o numerador del ratio a calcular, dentro de un \code{dataframe}. Debe anteponerse ~
#' @param denominador variable denominador del ratio a calcular, dentro de un \code{dataframe}. Debe anteponerse ~
#' @param dominios dominios de estimación separados por signo +. Debe anteponerse ~
#' @param disenio disenio complejo creado mediante el paquete \code{survey}
#'
#' @return \code{dataframe} que contiene variables de agregación, variable objetivo y error estándar
#' @import survey
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf, weights = ~fe)
#' calcular_tabla_ratio(var = ~gasto_div1, denominador = ~gasto, zona+sexo, dc)
calcular_tabla_ratio <-  function(var,denominador, dominios = NULL, disenio) {
  if (!is.null(dominios)) {
    estimacion <- survey::svyby(var, denominator = denominador,design =  disenio, by = dominios , FUN = svyratio)
  } else {
    estimacion <- survey::svyratio(var, denominator = denominador, design = disenio)
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


calcular_ic <-  function(data, env = parent.frame(), tipo = "resto", ajuste_ene) {

    est <- switch(tipo, "resto" =  get("var", env),
                  "media_agregado" = "mean",
                  "prop_agregado" = "objetivo",
                  "total_agregado" = "total",
                  "mediana_agregado" = "median")

    # Se calculan los intervalos de la manera tradicional en la generalidad de los casos
    if (ajuste_ene == F) {

      final <- data %>%
        dplyr::mutate(t = qt(c(.975), df = gl),
                      li = !!rlang::parse_expr(est) - se*t,
                      ls = !!rlang::parse_expr(est) + se*t)
  # Estos corresponde al ajuste de la ENE: el t se fija en 2
  } else if (ajuste_ene == T) {

    final <- data %>%
      dplyr::mutate(t = 2,
                    li = !!rlang::parse_expr(est) - se*t,
                    ls = !!rlang::parse_expr(est) + se*t)
  }

  return(final)
}


#---------------------------------------------------------------------

calcular_medianas_internal <- function(var, dominios, disenio, sub = F, env = parent.frame()) {

  #Si el usuario pone una subpoblación, se hace un filtro en el diseño para agilizar el cálculo
  if (sub == T) {
    filtro <-  rlang::parse_expr(get("subpop", env))
    disenio <- subset(disenio,   rlang::eval_tidy(filtro) == 1)

  }

  # Generar un vector con la desagregación necesaria
  doms <- as.character(dominios)
  doms <- stringr::str_split(doms[[2]], "\\+")
  doms <- stringr::str_remove_all(doms[[1]], " ")

  # Identificar cuáles son las categorías de cada una de las variables de desagregación
  categorias <- purrr::map(doms, ~sort(unique(as.character(disenio$variables[[.x]]) )))

  # Generar el iterador, según el número de desagregaciones pedidas por el usuario. Además, se calcula el número de combinaciones de celdas.
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

  i <- 1
  while (itertools::hasNext(it)) {
    x <- iterators::nextElem(it)


    exp <- rlang::parse_expr(paste(doms, "==",  x , collapse = " & "))
    output <- tryCatch(
      {
        median <- svyquantile(var,
                              design = subset(disenio, rlang::eval_tidy(exp) ),
                              quantile = 0.5,
                              method="constant",
                              interval.type = "quantile",
                              ties="discrete")
      },
      error=function(cond) {
        return(data.frame(X1 = NA, X2 = NA))
      }

    )

    acumulado[i, ] <- output %>%
      as.data.frame() %>%
      dplyr::mutate(v = paste(x, collapse = "-"))

    i <- i + 1


  }
  final <- acumulado  %>%
    tidyr::separate(into = doms, col = X3 , sep = "-") %>%
    dplyr::rename(se = X2,
                  V1 = X1) %>%
    dplyr::relocate(V1, se, .after = dplyr::last_col())

  return(final)
}

#--------------------------------------------------------------------

#' Create the inputs to make quality evaluation of mean estimations
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
#' @param anidar \code{boolean} Indicating if the function is wrapped inside a function, if \code{TRUE} avoid lazy eval errors
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_mean(gastot_hd, zona+sexo,  disenio = dc)
#' @export

create_mean = function(var, dominios = NULL, subpop = NULL, disenio, ci = F, ajuste_ene = F, anidar = F) {

  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]

  if(anidar == F){

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

  # Chequear que la variable no sea character
  if (is.character(disenio$variables[[var]]) == T) stop("¡Estás usando una variable character!")

  #Chequear que la variable sea continua. Si no lo es, aparece un warning
  es_prop <- disenio$variables %>%
    dplyr::mutate(es_prop = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0, 1, 0))

  if (sum(es_prop$es_prop) == nrow(disenio$variables)) warning("¡Parece que tu variable es de proporción!")


  #Convertir los inputs en fórmulas para adecuarlos a survey
  var_form <- paste0("~",var) %>%
    as.formula()

  # ESTO CORRESPONDE AL CASO CON DESAGREGACIÓN
  if (!is.null(dominios[[1]])) {

    # Esto corre para el caso en el que NO hay subpop
    if (is.null(subpop)) {

      dominios_form <- paste0("~", dominios) %>%
        as.formula()

      #Generar la tabla con los cálculos
      tabla <- calcular_tabla(var_form, dominios_form, disenio)

      # Esto corre para subpop
    } else if (!is.null(subpop)) { # caso que tiene subpop

      # Chequear que la variable de subpop es una dummy. Si no se cumple, se interrumpe la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop) == 1 | !!rlang::parse_expr(subpop) == 0 |
                                                        is.na(!!rlang::parse_expr(subpop)), 1, 0))
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      dominios_form <-   paste(dominios, subpop, sep = "+")
      dominios_form <- paste0("~", dominios_form) %>%
        as.formula()

      #Generar la tabla con los cálculos
      tabla <- calcular_tabla(var_form, dominios_form, disenio) %>%
        dplyr::filter(!!rlang::parse_expr(subpop) == 1)
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

    names(final)[grep(var,names(final))] = "mean"

    if (ci == T) {
      #var_string = var
      final <- calcular_ic(final, tipo = "media_agregado", ajuste_ene = ajuste_ene)
    }



    # ESTO CORRESPONDE AL CASO SIN DESAGREGACIÓN
  } else {


    # Si el usuario ingresa subpoblación, se filtra la base de datos para la subpoblación de referencia
    if (!is.null(subpop)) {

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop) == 1 | !!rlang::parse_expr(subpop) == 0 |
                                                        is.na(!!rlang::parse_expr(subpop)), 1, 0))

      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      # Aquí se filtra el diseño
      # subpop <- rlang::expr_text(rlang::enexpr(subpop))
      # filtro <-  paste(subpop, "== 1")
      # disenio <- subset(disenio, !!rlang::parse_expr(filtro))

      disenio <- disenio[disenio$variables[[subpop]] == 1]
    }

    dominios_form = dominios
    #Generar la tabla con los cálculos
    tabla <- calcular_tabla(var_form, dominios_form, disenio)

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
      ##   var_string = var
      final <- calcular_ic(data = final, tipo = "media_agregado",  ajuste_ene = ajuste_ene)
    }


  }

  if(!is.null(dominios) && !is.null(subpop)){
    final = final %>% dplyr::filter(!!rlang::parse_expr(subpop)  == 1) %>% dplyr::select(-!!rlang::parse_expr(subpop))
  }
  return(final)
}


#--------------------------------------------------------------------


#' Create the inputs to make quality evaluation for total estimations of continuous variables
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
#' @param anidar \code{boolean} Indicating if the function is wrapped inside a function, if \code{TRUE} avoid lazy eval errors
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_tot_con(gastot_hd, zona+sexo, subpop = ocupado, disenio = dc)
#' @export

create_tot_con <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F, ajuste_ene = F, anidar = F) {

  # chequear_var_disenio(disenio$variables)

  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]

  if(anidar == F){

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

  # Verificar que la variable de estimación sea numérica. Se interrumpe si no es numérica
  if (!is.numeric(disenio$variables[[var]]) ) stop("Debes usar una variable numérica")

  # Pasar la variable objetivo al formato de survey
  var_form <- paste0("~", var) %>%
    as.formula()

  # ESTO CORRESPONDE AL CASO EN EL QUE HAY DESAGREGACIÓN
  if (!is.null(dominios)) {

    # Esto corre para el caso en el que NO hay subpop
    if (is.null(subpop)) {

      #Identificar las variables ingresadas para la desagregación
      agrupacion <- dominios %>%
        stringr::str_split(pattern = "\\+")

      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrup1 <- c(agrupacion, var)

      # Agregar ~ para adecuar a formato de survey
      dominios_form <- paste0("~", dominios) %>%
        as.formula()

      # Esto corre para subpop
    } else if (!is.null(subpop)) { # caso que tiene subpop

      # Chequear que la variable de subpop es una dummy. Si no se cumple, se interrumpe la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))

      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      #Identificar las variables ingresadas para la desagregación
      agrupacion <- dominios %>%
        stringr::str_split(pattern = "\\+")
      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrupacion <- c(subpop, agrupacion)
      agrup1 <- c(agrupacion, var)


      dominios_form <- paste(subpop, dominios, sep =  "+")
      dominios_form <- paste0("~", dominios_form) %>%
        as.formula()
    }

    tabla <- survey::svyby(formula = var_form, by = dominios_form, design = disenio, FUN = survey::svytotal)

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

    names(final)[grep(var,names(final))] = "total"

    #Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final, tipo = "total_agregado",ajuste_ene = ajuste_ene)
    }


    # ESTE ES EL CASO NO AGREGADO
  } else {

    tabla <- survey::svytotal(x = var_form, design = disenio )

    # Tabla con los totales
    totales <- as.data.frame(tabla) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::rename(se = var)


    # Tamaño muestral
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

    # Coeficiente de variación
    cv <- cv(tabla, design = disenio) * 100
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
  if(!is.null(dominios) && !is.null(subpop)){
    final = final %>% dplyr::filter(!!rlang::parse_expr(subpop) == 1) %>% dplyr::select(-!!rlang::parse_expr(subpop))
  }

  return(final)
}


#--------------------------------------------------------------------


#' Create the inputs to make quality evaluation for total estimations
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
#' @param anidar \code{boolean} Indicating if the function is wrapped inside a function, if \code{TRUE} avoid lazy eval errors
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_tot(ocupado, zona+sexo, disenio = dc)
#' @export

create_tot <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F, ajuste_ene = F, anidar = F) {

  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]
  disenio$variables$fe = disenio$variables[[unificar_variables_factExp(disenio)]]

  if(anidar == F){

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


  # ESTO CORRESPONDE AL CASO CON DESAGREGACIÓN
  if (!is.null(dominios)) {

    # Verificar que la variabe de entrada es correcta
    if (!is.numeric(disenio$variables[[var]])) stop("¡La variable debe ser numérica!")

    # Verificar que la variable es dummy
    test <- disenio$variable %>%
      dplyr::mutate(test = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0, 1, 0)) %>%
      dplyr::summarise(pasa = sum(test))

    n_filas <- nrow(disenio$variable)
    if (n_filas != test$pasa) stop("¡Debes usar una variable dummy cuando desagregas!")

    # Esto corre para el caso en el que NO hay subpop
    if (is.null(subpop)) {

      #Identificar las variables ingresadas para la desagregación
      agrupacion <- dominios %>%
        stringr::str_split(pattern = "\\+")
      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrup1 <- c(agrupacion, var)

      # Agregar ~ para adecuar a formato de survey
      dominios_form <- paste0("~", dominios) %>%
        as.formula()

      # Esto corre para subpop
    } else if (!is.null(rlang::enexpr(subpop))) { # caso que tiene subpop

      # Chequear que la variable de subpop es una dummy. Si no se cumple, se interrumpe la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      #Identificar las variables ingresadas para la desagregación
      agrupacion <- dominios %>%
        stringr::str_split(pattern = "\\+")
      agrupacion <- stringr::str_remove_all(string =  agrupacion[[1]], pattern = " ")
      agrupacion <- c(subpop, agrupacion  )
      agrup1 <- c(agrupacion, var)

      #dominios_form <- paste(agrupacion, "+")
      dominios_form <- paste(subpop, dominios, sep =  "+")
      dominios_form <- paste0("~", dominios_form) %>%
        as.formula()

    }
    # Pasar a la variable objetivo al formato de survey
    var_form <- paste0("~",var) %>%
      as.formula()

    # Generar la tabla de estimaciones
    tabla <- survey::svyby(formula = var_form, by = dominios_form, design = disenio, FUN = survey::svytotal)

    gl <- calcular_upm(disenio$variables, agrup1) %>%
      dplyr::left_join(calcular_estrato(disenio$variables, agrup1), by = agrup1) %>%
      dplyr::mutate(gl = upm - varstrat)  %>%
      dplyr::filter(!!rlang::parse_expr(var) == 1)  %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)


    cv <- survey::cv(tabla, design = disenio) %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "variable") %>%
      tidyr::separate(variable, agrupacion) %>%
      dplyr::rename(coef_var = ".") %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character) %>%
      dplyr::mutate(coef_var = coef_var * 100)


    n <- calcular_n(disenio$variables, dominios = agrup1) %>%
      dplyr::filter(!!rlang::parse_expr(var) == 1) %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character)

    # Unir todo y generar la tabla final
    final <- tabla %>%
      dplyr::mutate_at(.vars = dplyr::vars(agrupacion), .funs = as.character) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n")),
                       by = agrupacion) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl")),
                       by = agrupacion) %>%
      dplyr::left_join(cv, by = agrupacion)

    names(final)[grep(var,names(final))] = "total"

    #Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final, tipo = "total_agregado",ajuste_ene = ajuste_ene)
    }

    # ESTO CORRESPONDE AL CASO SIN DESAGRAGACIÓN
  } else {

    n_cat = length(unique(disenio$variable[[var]]))

    if (n_cat > 50 ) stop("¡La variable puede ser continua, posee mas de 50 categorias!")


    # Identificar las variables ingresadas por el usuario
    agrupacion <- var %>%
      stringr::str_split(pattern = "\\+")
    agrup1 <- stringr::str_remove_all(string =  agrupacion, pattern = " ")


    # Convertir variables a string. Esto se hace debido a que survey tiene distintos tratamientos para variables numéricas o de string
    disenio <- survey::svydesign(ids = ~varunit, strata = ~varstrat,
                                 data = disenio$variables %>% dplyr::mutate_at(.vars = dplyr::vars(agrup1), list(as.character)),
                                 weights = ~fe)

    # Acomodar a formato de survey
    var_form <- paste0("~",var) %>%
      as.formula()

    # Si el usuario ingresa subpoblación, se filtra la base de datos para la subpoblación de referencia
    if (!is.null(subpop)) {

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      # Aquí se filtra el diseño
      disenio <- disenio[disenio$variables[[subpop]] == 1]

    }


    # Tabla que se usa luego para calcular cv
    tabla <- survey::svytotal(x = var_form, design = disenio )

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
      final <- calcular_ic(data = final, tipo = "total_agregado",  ajuste_ene = ajuste_ene)

    }

  }

  # Las filas en las que no exsiten casos generan valores NA. Esos casos se eliminan
  names(final) <- tolower(names(final))
  final <- final %>%
    dplyr::filter(!is.nan(coef_var))


  if(!is.null(dominios) && !is.null(subpop)){
    final = final %>% dplyr::filter(!!rlang::parse_expr(subpop)  == 1) %>% dplyr::select(-!!rlang::parse_expr(subpop))
  }

  return(final)
}


#-----------------------------------------------------------------------


#' Create the inputs to make quality evaluation for median estimations
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
#' #' @param anidar \code{boolean} Indicating if the function is wrapped inside a function, if \code{TRUE} avoid lazy eval errors
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' dc_rep <-  as.svrepdesign(dc , type = "subbootstrap", replicates=10)
#' create_median(gastot_hd, zona+sexo, disenio = dc)
#' @export


create_median <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F, replicas = 10,  ajuste_ene = F,anidar = F) {

  # Ajustar nombre de variables del diseño muestral
  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]

  if(anidar == F){

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


  # Generar el diseño replicado
  set.seed(1234)
  disenio <-  as.svrepdesign(disenio, type = "subbootstrap", replicates = replicas)

  # Chequear que la variable no sea character
  if (is.character(disenio$variables[[var]]) == T) stop("¡Estás usando una variable character!")

  #Chequear que la variable sea continua. Si no lo es, aparece un warning
  es_prop <- disenio$variables %>%
    dplyr::mutate(es_prop = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var) == 0, 1, 0))

  if (sum(es_prop$es_prop) == nrow(disenio$variables)) warning("¡Parece que tu variable es de proporción!")


  #Convertir los inputs en fórmulas para adecuarlos a survey
  var_form <- paste0("~", var) %>%
    as.formula()

  # ESTO CORRESPONDE AL CASO CON DESAGREGACIÓN
  if (!is.null(dominios)) {

    # Esto corre para el caso en el que NO hay subpop
    if (is.null(subpop)) {

      dominios_form <- paste0("~",dominios) %>%
        as.formula()

      #Generar la tabla con los cálculos

      tabla <- calcular_medianas_internal(var_form, dominios_form, disenio)

      # Esto corre para subpop
    } else if (!is.null(subpop)) { # caso que tiene subpop

      # Chequear que la variable de subpop es una dummy. Si no se cumple, se interrumpe la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop) == 1 | !!rlang::parse_expr(subpop) == 0 |
                                                        is.na(!!rlang::parse_expr(subpop)), 1, 0))

      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      # Agregar a los dominios, la variable subpop
      dominios_form <-   paste(dominios, subpop, sep = "+")
      dominios_form <- paste0("~", dominios_form) %>%
        as.formula()

      #Generar la tabla con los cálculos

      tabla <- calcular_medianas_internal(var_form, dominios_form, disenio, sub = T)

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
    #cv <- cv(tabla, design = disenio) * 100
    cv <- tabla$se / tabla$V1

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
      dplyr::rename(!!rlang::parse_expr(var) := V1)

    names(final)[grep(var,names(final))] = "median"

    # Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final, tipo = "mediana_agregado",ajuste_ene = ajuste_ene)
    }

    # ESTO CORRESPONDE AL CASO SIN DESAGREGACIÓN
  } else {


    # Si el usuario ingresa subpoblación, se filtra la base de datos para la subpoblación de referencia
    if (!is.null(subpop)) {

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop) == 1 | !!rlang::parse_expr(subpop) == 0 |
                                                        is.na(!!rlang::parse_expr(subpop)), 1, 0))

      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      disenio <- disenio[disenio$variables[[subpop]] == 1]
    }

    dominios_form = dominios
    #Generar la tabla con los cálculos
    tabla <- calcular_tabla(var_form, dominios_form, disenio, media = F)

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

    names(final)[grep("quantiles",names(final))] = "median"

    # Se calcular el intervalo de confianza solo si el usuario lo pide
    if (ci == T) {
      final <- calcular_ic(data = final, tipo = "mediana_agregado",  ajuste_ene = ajuste_ene)
    }

  }

  # Filtrar filas que no son útiles
  if(!is.null(dominios) && !is.null(subpop)){
    final <-  final %>%
      dplyr::filter(!!rlang::parse_expr(subpop)  == 1) %>%
      dplyr::select(-!!rlang::parse_expr(subpop))
  }

  return(final)

}


#-----------------------------------------------------------------------

#' internal function to calculate ratios estimations
#'
#' @param var numeric variable within the \code{dataframe}, is the numerator of the ratio to be calculated.
#' @param denominador numeric variable within the \code{dataframe}, is the denominator of the ratio to be calculated.
#' @param dominios domains to be estimated separated by the + character.
#' @param disenio complex design created by \code{survey} package
#' @param subpop integer dummy variable to filter the dataframe
#' @param ci \code{boolean} indicating if the confidence intervals must be calculated
#' @param ajuste_ene \code{boolean} indicating if an adjustment for the sampling-frame transition period must be used
#' @param anidar \code{boolean} indicating if the function is inside another function, by default it is TRUE, avoid problems with lazy eval.
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'
#' @examples
#'
#'
create_ratio_internal <- function(var,denominador, dominios = NULL, subpop = NULL, disenio, ci = F, ajuste_ene = F, anidar = T) {
# Chequar que estén presentes las variables del diseño muestral. Si no se llaman varstrat y varunit, se
#  detiene la ejecución
# chequear_var_disenio(disenio$variables)
disenio$variables$varunit <- disenio$variables[[unificar_variables_upm(disenio)]]
disenio$variables$varstrat <- disenio$variables[[unificar_variables_estrato(disenio)]]

# if(anidar == F){
#   #  # Encapsular inputs para usarlos más tarde
#   var <- rlang::enexpr(var)
#   denominador <- rlang::enexpr(denominador)
#
#   var <-  rlang::expr_name(var)
#   denominador <- rlang::expr_name(denominador)
#   var_string = var
#
#   dominios <- rlang::enexpr(dominios)
#   if(!is.null(dominios)){
#     dominios <-  rlang::expr_name(dominios)
#   }
#
#   subpop <- rlang::enexpr(subpop)
#   if(!is.null(subpop)){
#     subpop <-  rlang::expr_name(subpop)
#   }
#
# }

#### filtro para ratios que no son categorias complementarias
es_prop <- disenio$variables %>%
  dplyr::mutate(es_prop_var = dplyr::if_else(!!rlang::parse_expr(denominador) == 1 | !!rlang::parse_expr(denominador) == 0 | is.na(!!rlang::parse_expr(denominador)), 1, 0))

if(sum(es_prop$es_prop_var) == nrow(es_prop)){
  out = tryCatch({cor(disenio$variables[[var]][disenio$variables[[var]] == 1 | disenio$variables[[denominador]] == 1],
                      disenio$variables[[denominador]][disenio$variables[[denominador]] == 1 | disenio$variables[[var]] == 1])},
                 warning = function(cond){
                   return("warning")
                 })

  if(out != -1){

    disenio <- disenio[disenio$variables[[denominador]] == 1]

  }

}

# Chequear que la variable no sea character
if (is.character(disenio$variables[[var]]) == T) stop("¡Estás usando una variable character!")

# Chequear que la variable no sea character
if (is.character(disenio$variables[[denominador]]) == T) stop("¡Estás usando una variable para el denominador de character!")

#Convertir los inputs en fórmulas para adecuarlos a survey
var <- paste0("~", var) %>%
  as.formula()

#Convertir los inputs en fórmulas para adecuarlos a survey
denominador <- paste0("~", denominador) %>%
  as.formula()

# CON DESAGREGACIÓN
if (!is.null(dominios[[1]])) {

  # Sin subpop #
  if (is.null(subpop)) {
    dominios <- paste0("~", dominios) %>%
      as.formula()

    # con subpop
  } else if (!is.null(subpop)) { # caso que tiene subpop

    dominios <- paste(dominios, subpop, sep = "+")
    dominios <- paste0("~", dominios) %>%
      as.formula()
  }

  #Generar la tabla con los cálculos
  tabla <- calcular_tabla_ratio(var, denominador, dominios, disenio)

  #Extraer nombres
  nombres <- names(tabla)
  agrupacion <-  nombres[c(-(length(nombres) - 1), -length(nombres)) ]
  var_ratio <- nombres[length(nombres) - 1]

  #+ Calcular N
  n <- calcular_n(disenio$variables, agrupacion) %>%
    dplyr::mutate_at(dplyr::vars(agrupacion), as.character)

  #+ Calcular GL de todos los cruces
  gl <- calcular_upm(disenio$variables, agrupacion) %>%
    dplyr::left_join(calcular_estrato(disenio$variables, agrupacion), by = agrupacion) %>%
    dplyr::mutate(gl = upm - varstrat) %>%
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
    dplyr::filter(objetivo > 0) # se eliminan los ceros de la tabla

  names(final)[grep("objetivo",names(final)) +1] = "se"

  #¿ intervalos de confianza solo si el usuario lo requiere
  if (ci == T) {
    final <- calcular_ic(final,tipo = "prop_agregado",  ajuste_ene = ajuste_ene)
  }

  # SIN DESAGREGACIÓN #
} else {

  # Con subpobp
  if (!is.null(subpop)) {

    # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecución
    es_prop <- disenio$variables %>%
      dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))
    if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

    # Aquí se filtra el diseño
    #  subpop_text <- rlang::expr_text(rlang::enexpr(subpop))
    disenio <- disenio[disenio$variables[[subpop]] == 1]

  }

  #Generar la tabla con los cálculos
  tabla <- calcular_tabla_ratio(var, denominador, dominios, disenio)

  #+ Calcular N
  n <- nrow(disenio$variables)

  #+ Calcular GL
  varstrat <- length(unique(disenio$variables$varstrat))
  varunit <- length(unique(disenio$variables$varunit))
  gl <- varunit - varstrat

  #+ Calcular CV
  cv <- cv(tabla, design = disenio) * 100

  #* * Armar tabla final
  final <- data.frame(tabla$ratio,survey::SE(tabla))
  final$cv = cv[1]
  names(final) = c("objetivo", "se","cv")

  # Armar tabla completa con todos los insumos
  final <- dplyr::bind_cols(final, "gl" = gl, "n" = n)
  #names(final)[2] <- "se"

  ##Cambiar el nombre de la variable objetivo para que siempre sea igual
  #final <- final %>%
  #  dplyr::rename(objetivo = mean)

  #¿  intervalo de confianza solo si el usuario lo pide
  if (ci == T) {
    final <- calcular_ic(data = final, tipo = "prop_agregado",  ajuste_ene = ajuste_ene)

  }

}

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
#' @param anidar \code{boolean} indicating if the function is inside another function, by default it is TRUE, avoid problems with lazy eval.
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'
#' @examples
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
#' create_prop_internal(ocupado, zona+sexo, disenio = dc)

create_prop_internal <- function(var, dominios = NULL, subpop = NULL, disenio, ci = F, ajuste_ene = F, anidar = T){

  # Chequar que estén presentes las variables del diseño muestral. Si no se llaman varstrat y varunit, se
  #  detiene la ejecución
  # chequear_var_disenio(disenio$variables)
  disenio$variables$varunit = disenio$variables[[unificar_variables_upm(disenio)]]
  disenio$variables$varstrat = disenio$variables[[unificar_variables_estrato(disenio)]]



  if (anidar == F){
    #  # Encapsular inputs para usarlos más tarde
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

  if (is.character(disenio$variables[[var]]) == T) stop("¡Estás usando una variable character!")

  #Chequear que la variable sea de proporción. Si no lo es, se interrumpe la ejecución
  es_prop <- disenio$variables %>%
    dplyr::mutate(es_prop_var = dplyr::if_else(!!rlang::parse_expr(var) == 1 | !!rlang::parse_expr(var)  == 0 | is.na(!!rlang::parse_expr(var)), 1, 0))

  if (sum(es_prop$es_prop_var) != nrow(es_prop)) stop("¡La variable no es de proporción!")

  #COnvertir los inputs en fórmulas para adecuarlos a survey
  var <- paste0("~", var) %>%
    as.formula()

  # ESTO CORRESPONDE AL CASO CON DESAGREGACIÓN
  if (!is.null(dominios[[1]])) {

    # Esto corre para el caso en el que NO hay subpop
    if (is.null(subpop)) {
      dominios <- paste0("~", dominios) %>%
        as.formula()

      # Esto corre para subpop
    } else if (!is.null(subpop)) { # caso que tiene subpop

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))

      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      dominios <- paste(dominios, subpop, sep = "+")
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


    tabla$cv <- cv(tabla)

    #Unir toda la información. Se hace con join para asegurar que no existan problemas en la unión
    final <- tabla %>%
      dplyr::mutate_at(dplyr::vars(agrupacion), as.character) %>%
      dplyr::left_join(gl %>% dplyr::select(c(agrupacion, "gl" )),
                       by = agrupacion) %>%
      dplyr::left_join(n %>% dplyr::select(c(agrupacion, "n" )),
                       by = agrupacion)
    var_string = var


    #Cambiar el nombre de la variable objetivo para que siempre sea igual.
    final <- final  %>%
      dplyr::rename(objetivo = var_prop) %>%
      dplyr::filter(objetivo > 0) # se eliminan los ceros de la tabla


    #Se calculan los intervalos de confianza solo si el usuario lo requiere
    if (ci == T) {
      final <- calcular_ic(final,tipo = "prop_agregado",  ajuste_ene = ajuste_ene)
    }


    # ESTO CORRESPONDE AL CASO SIN DESAGREGACIÓN
  } else {

    # Si el usuario ingresa subpoblación, se filtra la base de datos para la subpoblación de referencia
    if (!is.null(subpop)) {

      # Chequear que subpop sea una variable dummy. Si no se cumple, se detiene la ejecución
      es_prop <- disenio$variables %>%
        dplyr::mutate(es_prop_subpop = dplyr::if_else(!!rlang::parse_expr(subpop)  == 1 | !!rlang::parse_expr(subpop) == 0, 1, 0))
      if (sum(es_prop$es_prop_subpop) != nrow(es_prop)) stop("¡subpop debe ser dummy!")

      # Aquí se filtra el diseño
      #  subpop_text <- rlang::expr_text(rlang::enexpr(subpop))
      disenio <- disenio[disenio$variables[[subpop]] == 1]

    }

    #Generar la tabla con los cálculos
    tabla <- calcular_tabla(var, dominios, disenio)

    # Tamaño muestral
    n <- nrow(disenio$variables)

    # Calcular grados de libertad
    varstrat <- length(unique(disenio$variables$varstrat))
    varunit <- length(unique(disenio$variables$varunit))
    gl <- varunit - varstrat

    # Calcular CV
    cv <- cv(tabla)

    # Armar tabla final
    final <- data.frame(tabla[1],survey::SE(tabla))
    final$cv = cv[1]
    names(final) = c("objetivo", "se","cv")

    # Armar tabla completa con todos los insumos
    final <- dplyr::bind_cols(final, "gl" = gl, "n" = n)
    names(final)[2] <- "se"

    #Cambiar el nombre de la variable objetivo para que siempre sea igual
    # final <- final %>%
    #  dplyr::rename(objetivo = mean)

    # Se calcula el intervalo de confianza solo si el usuario lo pide
    if (ci == T) {
      final <- calcular_ic(data = final, tipo = "prop_agregado",  ajuste_ene = ajuste_ene)

    }

  }

  if(!is.null(dominios) && !is.null(subpop)){
    final = final %>% dplyr::filter(!!rlang::parse_expr(subpop)  == 1) %>% dplyr::select(-!!rlang::parse_expr(subpop))
  }

  return(final)

}


#-----------------------------------------------------------------------


#' \code{create_ratio} generates a \code{dataframe} with the following elements: sum,
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
#' @param anidar \code{boolean} Indicating if the function is wrapped inside a function, if \code{TRUE} avoid lazy eval errors
#' @return \code{dataframe} that contains the inputs and all domains to be evaluated
#'
#' @example
#'
#' epf_gastos = epf_gastos %>% mutate(gasto_div1 = dplyr::if_else(d == "01", gasto, 0))
#' dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_gastos, weights = ~fe)
#' create_ratio(var = gasto_div1, denominador = gasto, disenio =  dc, dominios = zona)
#'
#' enusc = enusc %>% filter(enusc$Kish == 1) %>%  mutate(muj_insg_taxi = dplyr::if_else(enusc$P9_4_1 %in% c(1,2) & enusc$rph_sexo == 2,1 ,0),
#'                                                        hom_insg_taxi = dplyr::if_else(enusc$P9_4_1 %in% c(1,2) & enusc$rph_sexo == 1,1 ,0))
#' dc <- svydesign(ids = ~Conglomerado, strata = ~VarStrat, data = enusc, weights = ~Fact_Pers)
#' create_ratio(var = muj_insg_taxi, denominador = hom_insg_taxi, disenio = dc)
#'
#' create_ratio(var = VP_DC, denominador = NULL, disenio = dc, ci = T)
#'
#' @export
#'

create_ratio = function(var, denominador = NULL, dominios = NULL, subpop = NULL, disenio, ci = F, ajuste_ene = F,anidar = F){

  #  # Encapsular inputs para usarlos más tarde
  if(anidar == F){

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
    final = create_ratio_internal(var,denominador, dominios, subpop, disenio, ci, ajuste_ene)
  }

  if(is.null(denominador)){
    final = create_prop_internal(var, dominios, subpop, disenio, ci, ajuste_ene)
  }
  return(final)
}


