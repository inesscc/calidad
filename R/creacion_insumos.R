#' Calcula medias a partir de cierta agregación
#'
#' Genera una tabla con estimaciones para una agregación determinada
#'
#' @param var variable objetivo dentro de un \code{dataframe}. Debe anteponerse ~
#' @param dominios dominios de estimación separados por signo +. Debe anteponerse ~
#' @param diseño diseño complejo creado mediante el paquete \code{survey}
#'
#' @return \code{dataframe} que contiene variables de agregación, variable objetivo y error estándar
#'
#' @examples
#' calcular_tabla(~gastot_hd, ~zona+sexo, dc)
#' @export
calcular_tabla <-  function(var, dominios, diseño) {
  estimacion <- survey::svyby(var , design = diseño, by = dominios , FUN = svymean)
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
#' calcular_n(hog, c("zona", "sexo"), var = NULL)
calcular_n <- function(data, dominios, var = NULL) {
  if (is.null(var)) {
    data %>%
      dplyr::group_by_(.dots = as.list(dominios)  ) %>%
      dplyr::summarise(n = dplyr::n())
  } else {
    symbol_var <- rlang::sym(var)
    hog %>%
      dplyr::group_by_(.dots = as.list(dominios)  ) %>%
      dplyr::summarise(n = sum(!! symbol_var))
  }
}


#-----------------------------------------------------------------------

#' Calcula el número de UPM
#'
#' Genera una tabla con el conteo de UPM para cada uno de los dominios del tabulado.
#' La función contempla un caso para proporción y un caso para promedio
#'
#' @param data \code{dataframe} que contiene los datos que se están evaluando
#' @param var variable objetivo. Debe ser un integer que toma los valores 1 o 0
#' @param dominios vector de caracteres que contiene los dominios a evaluar
#' @param var string que contiene el nombre de la variable de proporción que se evalúa.
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' calcular_upm(hog, c("zona", "sexo"), "ocupado")

calcular_upm <- function(data, dominios, var = NULL ) {
  if (is.null(var)) {
    data %>%
      dplyr::group_by_("varunit", .dots = as.list(dominios)  ) %>%
      dplyr::summarise(conteo = dplyr::n()) %>%
      dplyr::mutate(tiene_info = dplyr::if_else(conteo > 0, 1, 0))  %>%
      dplyr::group_by_(.dots = as.list(dominios)) %>%
      dplyr::summarise(upm = sum(tiene_info))
  } else {
    symbol_var <- rlang::sym(var)
    data %>%
      dplyr::group_by_("varunit", .dots = as.list(dominios)  ) %>%
      dplyr::summarise(conteo = sum(!!symbol_var)) %>%
      dplyr::mutate(tiene_info = dplyr::if_else(conteo > 0, 1, 0))  %>%
      dplyr::group_by_(.dots = as.list(dominios)) %>%
      dplyr::summarise(upm = sum(tiene_info))
  }
}
#-----------------------------------------------------------------------

#' Calcula el número de estratos
#'
#' Genera una tabla con el conteo de estratos para cada uno de los dominios del tabulado.
#' La función contempla un caso para proporción y un caso para promedio
#'
#' @param data \code{dataframe} que contiene los datos que se están evaluando
#' @param var variable objetivo. Debe ser un integer que toma los valores 1 o 0
#' @param dominios vector de caracteres que contiene los dominios a evaluar
#' @param var string que contiene el nombre de la variable de proporción que se evalúa.
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' calcular_estrato(hog, c("zona", "sexo"), "ocupado")

calcular_estrato <- function(data, dominios, var = NULL ) {
  if (is.null(var)) {
    data %>%
      dplyr::group_by_("varstrat", .dots = as.list(dominios)  ) %>%
      dplyr::summarise(conteo = dplyr::n()) %>%
      dplyr::mutate(tiene_info = dplyr::if_else(conteo > 0, 1, 0)) %>%
      dplyr::group_by_(.dots = as.list(dominios)) %>%
      dplyr::summarise(varstrat = sum(tiene_info))
  } else {
    symbol_var <- rlang::sym(var)
    data %>%
      dplyr::group_by_("varstrat", .dots = as.list(dominios)  ) %>%
      dplyr::summarise(conteo = sum(!!symbol_var)) %>%
      dplyr::mutate(tiene_info = dplyr::if_else(conteo > 0, 1, 0)) %>%
      dplyr::group_by_(.dots = as.list(dominios)) %>%
      dplyr::summarise(varstrat = sum(tiene_info))
  }
}


#--------------------------------------------------------------------
#' Crea los insumos necesarios para hacer la evaluación de estimadores de la media
#'
#' Genera una tabla con los siguientes insumos: media, grados de libertad,
#' tamaño muestral y coeficiente de variación.
#'
#' @param var variable objetivo dentro de un \code{dataframe}. Debe anteponerse ~
#' @param dominios dominios de estimación separados por signo +. Debe anteponerse ~
#' @param diseño diseño complejo creado mediante el paquete \code{survey}
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' crear_insumos(~gastot_hd, ~zona+sexo, dc)
#'
crear_insumos <- function(var, dominios, diseño) {
  #Generar la tabla con los cálculos
  tabla <- calcular_tabla(var, dominios, diseño)

  #Extraer nombres
  nombres <- names(tabla)
  agrupacion <-  nombres[c(- (length(nombres) - 1), -length(nombres)) ]

  #Calcular el tamaño muestral de cada grupo
  n <- calcular_n(hog, agrupacion)

  #Calcular los grados de libertad de todos los cruces
  gl <- calcular_upm(hog, agrupacion) %>%
    dplyr::left_join(calcular_estrato(hog, agrupacion), by = agrupacion) %>%
    dplyr::mutate(gl = upm - varstrat)

  #Extrear el coeficiente de variación
  cv <- cv(tabla, design = diseño) * 100

  cv <- tabla %>%
    dplyr::select_(.dots =  agrupacion) %>%
    dplyr::bind_cols(coef_var = cv)

  #Unir toda la información. Se hace con join para asegurar que no existan problemas en la unión
  final <- tabla %>%
    dplyr::left_join(gl %>% dplyr::select_(.dots = as.list(c(agrupacion, "gl" ))),
              by = agrupacion) %>%
    dplyr::left_join(n %>% dplyr::select_(.dots = as.list(c(agrupacion, "n" ))),
              by = agrupacion) %>%
    dplyr::left_join(cv %>% dplyr::select_(.dots = as.list(c(agrupacion, "coef_var" ))),
              by = agrupacion)

  return(final)

}


#-------------------------------

#' Crea los insumos necesarios para hacer la evaluación de estimadores de proporción
#'
#' Genera una tabla con los siguientes insumos: media, grados de libertad,
#' tamaño muestral y error estándar
#'
#' @param var variable objetivo dentro de un \code{dataframe}. Debe anteponerse ~
#' @param dominios dominios de estimación separados por signo +. Debe anteponerse ~
#' @param diseño diseño complejo creado mediante el paquete \code{survey}
#' @return \code{dataframe} que contiene la frecuencia de todos los dominios a evaluar
#'
#' @examples
#' crear_insumos_prop(~ocupado, ~zona+sexo, dc)

crear_insumos_prop <- function(var, dominios, diseño) {
  #Generar la tabla con los cálculos
  tabla <- calcular_tabla(var, dominios, diseño)

  #Extraer nombres
  nombres <- names(tabla)
  agrupacion <-  nombres[c(- (length(nombres) - 1), -length(nombres)) ]
  var_prop <- nombres[length(nombres) - 1]

  #Calcular el tamaño muestral de cada grupo
  n <- calcular_n(hog, agrupacion, var_prop )

  #Calcular los grados de libertad de todos los cruces
  gl <- calcular_upm(hog, agrupacion, var_prop) %>%
    dplyr::left_join(calcular_estrato(hog, agrupacion, var_prop), by = agrupacion) %>%
    dplyr::mutate(gl = upm - varstrat)

  #Unir toda la información. Se hace con join para asegurar que no existan problemas en la unión
  final <- tabla %>%
    dplyr::left_join(gl %>% dplyr::select_(.dots = as.list(c(agrupacion, "gl" ))),
              by = agrupacion) %>%
    dplyr::left_join(n %>% dplyr::select_(.dots = as.list(c(agrupacion, "n" ))),
              by = agrupacion)

  #Cambiar el nombre de la variable objetivo para que siempre sea igual
  final <- final %>%
    dplyr::rename(objetivo = var_prop)
  return(final)

}

