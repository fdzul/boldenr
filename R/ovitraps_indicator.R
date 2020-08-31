#' Generate a graph of ovitraps indicator,
#'
#' @param x is the dataset of ovitraps of the current year.
#' @param nom_loc is the name of locality.
#' @param all is the argument for define the plot of all localities or one locality.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a plot of ovitraps indicator.
#'
#' @export
#'
#' @references
#'
#'
#' @examples 1+1
#'
#' @references xxxxx
#'
#' @seealso \link[boldenr]{epidemiological_channel} and \link[boldenr]{entomological_channel}
#'
#' @details xxxx
ovitraps_indicator <- function(x, nom_loc = NULL, all){

    ##  Step 2. modify the values of the localidad variable
    x$Localidad <- ifelse(x$Localidad %in% c("Ampliación Las Bajadas",
                                             "Bajos Del Jobo (Puente Moreno)",
                                             "Campestre Las Bajadas",
                                             "Colinas De Santa Fe",
                                             "Colonia Chalchihuecan",
                                             "Colonia El Renacimiento",
                                             "Dos Lomas",
                                             "Las Amapolas",
                                             "Las Bajadas",
                                             "Mata De Pita",
                                             "Río Medio [Granja]",
                                             "Santa Teresa",
                                             "Valente Díaz",
                                             "Boca Del Río"),
                          "Veracruz",
                          x$Localidad)
    x$Localidad <- ifelse(x$Localidad == "Fraccionamiento Ciudad Olmeca",
                          "Coatzacoalcos", x$Localidad)
    x$Localidad <- ifelse(x$Localidad == "Fraccionamiento Puente Moreno",
                          "Martínez De La Torre", x$Localidad)
    x$Localidad <- ifelse(x$Localidad == "Paso Nacional", "Alvarado", x$Localidad)
    x$sec_manz <- paste(x$Sector, x$Manzana, sep = "")
    y <- x %>%
        dplyr::filter(!is.na(Huevecillos)) %>%
        dplyr::group_by(Semana.Epidemiologica, Localidad) %>%
        dplyr::summarise(n_blocks = data.table::uniqueN(sec_manz),
                         n_block_positive = data.table::uniqueN(sec_manz[Huevecillos > 0]),
                         #n_block_negative = uniqueN(sec_manz[Huevecillos <= 0]),
                         n_ovitraps = dplyr::n(),
                         n_ovitraps_positive = sum(Huevecillos > 0, na.rm = TRUE),
                         #n_ovitraps_negative = sum(Huevecillos <= 0),
                         sum_ovitrap_positive = sum(Huevecillos, na.rm = TRUE),
                         avg_HMP = round(sum(Huevecillos)/data.table::uniqueN(sec_manz[Huevecillos > 0]),1),
                         avg_HOP = round(sum(Huevecillos)/sum(Huevecillos > 0),1),
                         perc_OP = round((sum(Huevecillos > 0)/dplyr::n())*100,1),
                         perc_MP = round((data.table::uniqueN(sec_manz[Huevecillos > 0])/data.table::uniqueN(sec_manz))*100, 1))
    y$Localidad <- factor(y$Localidad)
    y <- tidyr::gather_(data = y,
                       key = "Indicador",
                       value = "Promedio",
                       c("avg_HOP", "avg_HMP"))
    y <- tidyr::gather_(data = y,
                       key = "Scale2",
                       value = "Porcentaje",
                       c("perc_OP", "perc_MP"))
    y$Indicador <- factor(y$Indicador,
                          levels = c("avg_HMP","avg_HOP")[c(2,1)],
                          labels = c("Manzana", "Ovitrampa")[c(2,1)])
    y$Indicador2 <- factor(y$Scale2,
                           levels = c("perc_MP","perc_OP")[c(2,1)],
                           labels = c("Manzana", "Ovitrampa")[c(2,1)])
    ##
    if(all == FALSE){
        y <- y %>% dplyr::filter(Localidad == nom_loc)
        ggplot(data = y[y$Indicador == "Manzana",],
               aes(x = as.factor(Semana.Epidemiologica),
                   y = Promedio,
                   #colour = Indicador,
                   group = Indicador)) +
            geom_line(size = 2.5,
                      alpha = 0.6,
                      col = "darkred") +
            geom_point(shape = 21,
                       fill = "yellow",
                       size = 3,
                       stroke = 3,
                       alpha = 0.6,
                       colour = "red") +
            #facet_wrap(~Localidad, scales = "free_y") +
            geom_bar(data = y[y$Indicador2 == "Ovitrampa",],
                     aes(x = as.factor(Semana.Epidemiologica),
                         y = Porcentaje/0.25),
                     stat = "identity",
                     size = 0.4,
                     position = "identity",
                     col = "black",
                     fill = "gray",
                     alpha = 0.2) +
            geom_text(data = y[y$Indicador2 == "Ovitrampa",],
                      aes(x = as.factor(Semana.Epidemiologica),
                          label = Porcentaje,
                          y = Porcentaje/0.25),
                      vjust = 1.3,
                      nudge_y = -0.1) +
            theme_linedraw() +
            scale_y_continuous(sec.axis = sec_axis(~.*0.25,
                                                   name = "Porcentaje de Ovitrampas Positivas (Barras)")) +
            theme(axis.title.y = element_text(color = "darkred",
                                              size=13),
                  axis.title.y.right = element_text(color = "gray50",
                                                    size=13)) +
            ylab("Numero Promedio de Ovitrampas (Líneas)") +
            xlab("Semanas Epidemiologicas")
    } else {
        ggplot(data = y[y$Indicador == "Ovitrampa",],
               aes(x = Semana.Epidemiologica,
                   y = Promedio,
                   #colour = Indicador,
                   group = Indicador)) +
            geom_line(size = 1.3,
                      col = "darkred") +
            geom_point(shape = 21,
                       fill = "yellow",
                       stroke = 2,
                       colour = "red",
                       alpha = 0.70) +
            facet_wrap(Localidad, scales = "free_y") +
            geom_bar(data = y[y$Indicador2 == "Ovitrampa",],
                     aes_(x = Semana.Epidemiologica,
                         y = Porcentaje/0.25),
                     stat = "identity",
                     size = 0.2,
                     position = "identity",
                     col = "black",
                     fill = "steelblue",
                     alpha = 0.1) +
            scale_x_continuous(limits = c(min(y$Semana.Epidemiologica),
                                          max(y$Semana.Epidemiologica)),
                               breaks = seq(from = 0,to = max(y$Semana.Epidemiologica), by = 2)) +
            scale_y_continuous(sec.axis = sec_axis(~.*0.25,
                                                   name = "Porcentaje de Ovitrampas Positivas (Barras)")) +
            theme_linedraw() +
            ylab("Numero Promedio de Ovitrampas (Líneas)") +
            xlab("Semanas Epidemiologicas")
    }

}
