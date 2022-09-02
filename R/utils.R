
### load data from user #####

load_object <- function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  tmp[[ls(tmp)[1]]]
}

### carga de datos locales ####

carga_datos_locales <- function(path){

if(grepl(".sav", path)){

  data <- haven::read_sav(path)

} else if(grepl(".rds", tolower(path))){

  data <-  readRDS(path)
} else if(grepl(".dta", tolower(path))){

  data <- haven::read_dta(path)
} else if(grepl(".sas", tolower(path))){

  data <- haven::read_sas(path)
}  else if(grepl(".xlsx", tolower(path))){

  data <- readxl::read_excel(path)
} else if(grepl(".csv", tolower(path))){

  data <-  read_delim(path, delim = ';')
}else if(grepl(".feather", tolower(path))){

  data <-  feather::read_feather(path)
}

names(data) <- tolower(names(data))

data

}

### crear tabulado html ####

tabla_html_shiny <- function(tabla,input) {

  # Esto se hace en el caso de que la etiqueta diga calidad
  if (input$SCHEME == "chile") {
    d <- tabla %>%
      dplyr::mutate_if(is.numeric, ~round(.x, 2)) %>%
      dplyr::mutate(
        calidad = kableExtra::cell_spec(.data$calidad, background  = dplyr::case_when(
          .data$calidad == "fiable" ~ "green",
          .data$calidad == "poco fiable" ~ "yellow",
          .data$calidad == "no fiable" ~ "red"
        ),
        color = "black")) %>%
      dplyr::mutate(
        n = kableExtra::cell_spec(.data$n, color= dplyr::case_when(
          .data$n < 60  ~ "red",
          .data$n >= 60 ~ "black"
        )),
        df = kableExtra::cell_spec(.data$df, color = dplyr::case_when(
          .data$df < 9  ~ "red",
          .data$df >= 9 ~ "black"
        ))) %>%
      kableExtra::kable(format.args = list(decimal.mark = ',', big.mark = "."),
                        format = "html",
                        escape = FALSE,
                        align = "c",
                        table.attr = "style = \"color: black;\"")  %>%
      kableExtra::kable_styling("striped",
                                full_width = FALSE,
                                html_font = "arial") %>%
      kableExtra::kable_paper("hover") %>%
      kableExtra::row_spec(0, bold = TRUE, color = "black")

  } else {
  d <- tabla %>%
      dplyr::mutate_if(is.numeric, ~round(.x, 2)) %>%
      dplyr::mutate(
        calidad = kableExtra::cell_spec(.data$calidad, background  = dplyr::case_when(
          .data$calidad == "Publicar" ~ "green",
          .data$calidad == "Revisar" ~ "yellow",
          .data$calidad == "Suprimir" ~ "red"
        ),
        color = "black")) %>%
      dplyr::mutate(
        n = kableExtra::cell_spec(.data$n, color= dplyr::case_when(
          .data$n < 60  ~ "red",
          .data$n >= 60 ~ "black"
        )),
        df = kableExtra::cell_spec(.data$df, color = dplyr::case_when(
          .data$df < 9  ~ "red",
          .data$df >= 9 ~ "black"
        ))) %>%
      kableExtra::kable(format.args = list(decimal.mark = ',', big.mark = "."),
                        format = "html",
                        escape = FALSE,
                        align = "c",
                        table.attr = "style = \"color: black;\"")  %>%
      kableExtra::kable_styling("striped",
                                full_width = FALSE,
                                html_font = "arial") %>%
      kableExtra::kable_paper("hover") %>%
      kableExtra::row_spec(0, bold = TRUE, color = "black")

  }

#  wt$close()

  return(d)
}
