#' Generate a Entomological Channel
#'
#' This function has been designed to generate the
#' Entomological Channel of Aedes aegypti eggs.
#'
#' @param x is the dataset of historic ovitraps. For more information, see \url{http://kin.insp.mx/aplicaciones/SisMV}.
#' @param z is the dataset of actual year of ovitraps.
#' @param y is the catalogue of localities accord the \href{**INEGI**}{https://www.inegi.org.mx/default.html}.
#' @param mun1 is the name municipality.
#' @param nom_loc is the name of locality.
#' @param x_title is the name of tittle of x.
#' @param sep_ticks is the break in the x.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}.
#'
#' @return A Entomological Channel.
#'
#' @export
#'
#' @examples 1+1
#'
#' @references xxxxx
#'
#' @seealso \link[boldenr]{epidemiological_channel}
#'
#' @details The Entomological Channel is constructed in a similar way to the epidemiological channel, using the quantile function of the package stats. First we obtain the average number of eggs of the vector per year (a minimum time series of three years), week and location, second, the 25th, 50th and 75th percentiles are calculated, and categorized as success, safety, alert, respectively. As a third step, they are displayed with ggplot2 with the geom_area function. The fourth step includes the average number of eggs of the current year vector and is compared with the areas defined by the percentiles. If the temporary behavior of the eggs (average number per week) is below the success values (that is, under the area defined by success), then the control of egg abundance is in an area of success. This explanation is extrapolated to the security and alert areas. If the behavior of the eggs is above the warning zone, then it is in an epidemic zone, that is to say, the behavior of the eggs of the current year is superior to the behavior of the eggs of the time series.
entomological_channel <- function(x, z, y, mun1, nom_loc, x_title, sep_ticks){

    ### 1. changue the variable with chr to num
    x$edo <- as.numeric(x$edo)
    x$mpo <- as.numeric(x$mpo)

    ## 2 add the names of id pf the state and municipalities
    x <- dplyr::left_join(x = x,
                          y = y,
                          by = c("edo" = "CveEntidad",
                                 "mpo" = "CveMunicipio"))

    ## 3.0 calculate the quantil by municipality
    ## 3.1 make the function for calculate the quantil
    fun_quantil <- function(x){
        x %>%
            dplyr::group_by(week) %>%
            dplyr::summarise(Exito = stats::quantile(mean_count, probs = 0.25, na.rm = TRUE),
                             Seguridad = stats::quantile(mean_count, probs = 0.50, na.rm = TRUE),
                             Alerta = stats::quantile(mean_count, probs = 0.75, na.rm = TRUE))
        }

    ##

    ## 3.2 apply the function
    x <- x %>%
        dplyr::filter(NombreMunicipio %in%
                          c("Alvarado","Boca del Rio",
                            "Coatzacoalcos","Cordoba",
                            "Cosamaloapan de Carpio",
                            "Emiliano Zapata","Martinez de la Torre",
                            "Minatitlan","Orizaba",
                            "Panuco","Papantla",
                            "Poza Rica de Hidalgo",
                            "San Andres Tuxtla",
                            "Tuxpan", "Veracruz") &
                          year != 2011) %>%
        dplyr::mutate(mun = ifelse(NombreMunicipio %in%
                                c("Boca del Rio","Veracruz"),
                            "Veracruz", NombreMunicipio))

    x$sec_manz <- paste(x$sector, x$manzana, sep = "")


    ##
    x <- x %>%
        dplyr::group_by(year, week, mun) %>%
        dplyr::summarise(mean_count = round(sum(Huevos, na.rm = TRUE)/data.table::uniqueN(sec_manz[Huevos > 0]),1)) %>%
        dplyr::filter(mun == mun1) %>%
        dplyr::group_by(mun) %>%
        tidyr::nest() %>%
        dplyr::mutate(quantil = purrr::map(data, fun_quantil)) %>%
        tidyr::unnest(quantil, .drop = TRUE)
    ##
    z$Municipio <- stringr::str_trim(z$Municipio , side = "both")
    z$Localidad <- stringr::str_trim(z$Localidad, side = "both")
    z$Jurisdiccion <- stringr::str_trim(z$Jurisdiccion, side = "both")

    ##
    #z$Municipio <- stringr::str_sub(z$Municipio, start = 5)
    #z$Localidad <- stringr::str_sub(z$Localidad, start = 6)

    ###
    z$Localidad <- stringr::str_to_title(z$Localidad)
    z$Municipio <- stringr::str_trim(z$Municipio, side = "both")

    ##  Step 2. modify the values of the localidad variable
    z$Localidad <- ifelse(z$Localidad %in% c("Ampliación Las Bajadas",
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
                          z$Localidad)
    z$Localidad <- ifelse(z$Localidad == "Fraccionamiento Ciudad Olmeca",
                          "Coatzacoalcos", z$Localidad)
    z$Localidad <- ifelse(z$Localidad == "Fraccionamiento Puente Moreno",
                          "Martínez De La Torre", z$Localidad)
    z$Localidad <- ifelse(z$Localidad == "Paso Nacional", "Alvarado", z$Localidad)

    ### mean of ovitraps
    z$sec_manz <- paste(z$Sector, z$Manzana, sep = "")
    z <- z %>%
        dplyr::group_by(Semana.Epidemiologica, Localidad) %>%
        dplyr::summarise(mean_count = round(sum(Huevecillos, na.rm = TRUE)/data.table::uniqueN(sec_manz[Huevecillos > 0]),1))


    ## porcentaje de ovitrampas positivas..
    z$Localidad <- factor(z$Localidad)

    z <- z %>% dplyr::filter(Localidad == nom_loc) %>%
        dplyr::mutate(Semana.Epidemiologica = as.numeric(Semana.Epidemiologica))

    ####
    ggplot(data = x) +
        geom_area(aes(x = week, y = Alerta),
                  col = "gray90",
                  fill = "red", alpha = 0.90) +
        geom_area(aes(y = Seguridad, x = week),
                  col = "white",
                  fill = "orange", alpha = 0.80) +
        geom_area(aes(y = Exito, x = week),
                  col = "white",
                  fill = "lawngreen", alpha = 1) +
        #theme_classic() +
        ylab(x_title) +
        xlab("Semanas epidemiológicas") +
        annotate("text",
                 label = "Epidémico",
                 x = 40,
                 y = max(x$Alerta),
                 size = 6,
                 colour = "grey40") +
        annotate("text",
                 label = "Alerta",
                 x = unlist(x[ifelse(x$Alerta == max(x$Alerta),
                                     !is.na(x$week), NA), "week"]),
                 y = stats::quantile(x$Alerta, probs = 0.75),
                 size = 6,
                 colour = "white") +
        annotate("text",
                 label = "Seguridad",
                 x = unlist(x[ifelse(x$Seguridad == max(x$Seguridad),
                                     !is.na(x$week), NA), "week"]),
                 y = quantile(x$Seguridad, probs = 0.75),
                 size = 6,
                 colour = "white") +
        annotate("text",
                 label = "Éxito",
                 x = unlist(x[ifelse(x$Exito == max(x$Exito),
                                     !is.na(x$week), NA), "week"]),
                 y = stats::median(x$Exito, probs = 0.75),
                 size = 6,
                 colour = "grey40") +
        scale_y_continuous(breaks = seq(from = 0, to = max(x$Alerta),
                                        by = sep_ticks),
                           limits = c(0, max(x$Alerta))) +
        scale_x_continuous(breaks = seq(from = 1, to = 52, by = 2),
                           limits = c(1,length(x$week))) +
        theme_classic() +
        ggtitle(label = nom_loc) +
        theme(axis.text.x =  element_text(size = 12.5,
                                          face = "bold",
                                          color = "grey60")) +
        theme(axis.text.y = element_text(size = 12,
                                         face = "bold",
                                         color = "grey60")) +
        theme(axis.title.x = element_text(size = 14,
                                          face = "bold",
                                          color = "grey40")) +
        theme(axis.title.y = element_text(size = 20,
                                          face = "bold",
                                          color = "grey40")) +
        theme(axis.line = element_line(colour = "grey90",
                                       size = 1,
                                       linetype = "solid")) +
        geom_line(data = z,
                  aes(x = Semana.Epidemiologica,
                      y = mean_count),
                  size = 1.5,
                  alpha = 0.6,
                  col = "black") +
        geom_point(data = z,
                   aes(x = Semana.Epidemiologica,
                       y = mean_count),
                   size = 3,
                   fill = "grey",
                   shape = 21,
                   stroke = 3,
                   alpha = 0.6,
                   col = "darkred")
}
