#source("tema_elegante_ggplot.R")

create_plot <- function(tab,scheme) {
  tabulado <- tab

#  tabulado$calidad  <- stringr::str_to_title(tabulado$calidad)

  tabulado$calidad <- tabulado$calidad %>% stringr::str_to_title()

  labels <- unique(tabulado$calidad) %>%
    sort()

  pasted_labels <- labels %>%
    paste(collapse = " ")

  if(scheme == "chile"){
  colors <- switch(pasted_labels, "No Fiable" =  "red",
                   "Poco Fiable" = "yellow",
                   "Fiable" = "green",
                   "Fiable No Fiable" = c("green", "red"),
                   "Fiable No Fiable Poco Fiable" = c("green", "red", "yellow"),
                   "Fiable Poco Fiable" = c("green", "yellow"))

  } else {

    colors <- switch(pasted_labels, "Suprimir" =  "red",
                     "Revisar" = "yellow",
                     "Publicar" = "green",
                     "Publicar Suprimir" = c("green", "red"),
                     "Publicar Suprimir Revisar" = c("green", "red", "yellow"),
                     "Publicar Revisar" = c("green", "yellow"))
  }



  color.background = "#FFFFFF"
  tabulado  %>%
    dplyr::group_by(calidad) %>%
    dplyr::summarise(suma = n()) %>%
    dplyr::mutate(porcentaje = suma / sum(suma) * 100) %>%
    dplyr::mutate(row = 1,
                  calidad = forcats::fct_relevel(calidad, labels)) %>%
  #  dplyr::rename(Calidad = calidad, Porcentaje = porcentaje) %>%
    ggplot2::ggplot(aes(x = row, y = porcentaje, fill = calidad)) +
    ggplot2::geom_bar(stat = "identity" ) +
    ggplot2::scale_fill_manual(values= colors) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(porcentaje),"%")),
                       position = ggplot2::position_stack(vjust = 0.5), size = 10) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank()) +
    # Format the grid
    #  ggplot2::theme(panel.grid =  ggplot2::element_blank()) +
    #  ggplot2::theme(axis.ticks=  ggplot2::element_blank()) +

    ggplot2::theme(panel.background= ggplot2::element_rect(fill=color.background, color=color.background)) +
    ggplot2::theme(plot.background= ggplot2::element_rect(fill=color.background, color=color.background)) +
    ggplot2::theme(panel.border= ggplot2::element_blank()) +

    # Format the grid
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks= ggplot2::element_blank()) +

    # Format the legend, but hide by default
    ggplot2::theme(legend.background =  ggplot2::element_rect(fill=color.background),
                   legend.title= element_text(size = 15),
                   legend.text = element_text(size = 15)) +
    ggplot2::labs(x = "", y= "%", title = "Porcentaje de celdas fiables") +
    theme(plot.title = element_text(hjust = 0.5, size = 15))

}
