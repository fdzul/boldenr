#' Graph the epidemiological channel.
#'
#' This function has been designed to generate the
#' epidemiological channel of dengue cases.
#'
#' @param x is the historic epidemiological dataset.
#' @param edo is the name state.
#' @param mun is the name municipality. is NULL for state level.
#' @param state is a logical parameter for graph the state or municipality. If the state is TRUE the graph is the state, else the Jurisdiction, municipality
#' @param scale_case is a numeric parameter for the breaks.
#' @param z is the current epidemiological dataset.
#' @param juris is a logical parameter for graph when the state is FALSE. if juris is FALSE the graph is the municipality
#' @param jurisdiccion is the name of jurisdiction. is null in the level municipality
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a graph of class ggplot.
#'
#' @export
#'
#' @import stats
#'
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#'
#' @examples 1+1
#'
#' @seealso \link[boldenr]{entomological_channel}
#'
#' @details The Entomological Channel is constructed in a similar way to the epidemiological channel, using the quantile function of the package stats. First we obtain the average number of eggs of the vector per year (a minimum time series of three years), week and location, second, the 25th, 50th and 75th percentiles are calculated, and categorized as success, safety, alert, respectively. As a third step, they are displayed with ggplot2 with the geom_area function. The fourth step includes the average number of eggs of the current year vector and is compared with the areas defined by the percentiles. If the temporary behavior of the eggs (average number per week) is below the success values (that is, under the area defined by success), then the control of egg abundance is in an area of success. This explanation is extrapolated to the security and alert areas. If the behavior of the eggs is above the warning zone, then it is in an epidemic zone, that is to say, the behavior of the eggs of the current year is superior to the behavior of the eggs of the time series.
epidemiological_channel <- function(x, edo, mun = NULL, state, scale_case, z,
                                    juris, jurisdiccion = NULL,
                                    year1, year2){

    percentil <- function(x){
        x %>% dplyr::mutate(q25 = stats::quantile(n, probs = .25, type = 8),
                            q50 = stats::quantile(n, probs = .50, type = 8),
                            q75 = stats::quantile(n, probs = .75, type = 8))
        }
    x$DES_EDO.x <- stringr::str_trim(x$DES_EDO.x)
    x$DES_MPO.x <- stringr::str_trim(x$DES_MPO.x)
    x$DES_JUR <- stringr::str_trim(x$DES_JUR)
    if(state == TRUE){
        x <- x %>%
            dplyr::filter(VEC_EST == 2) %>%
            dplyr::filter(DES_EDO.x == edo) %>%
            dplyr::group_by(SEM, ANO) %>%
            dplyr::summarise(n = dplyr::n()) %>%
            dplyr::group_by(SEM) %>%
            tidyr::nest() %>%
            dplyr::mutate(risk = purrr::map(data, percentil)) %>%
            tidyr::unnest(risk, .drop =TRUE)
        x$data <- NULL
        x <- as.data.frame(x)
        x <- dplyr::distinct(x,
                             SEM = x$SEM,
                             q25 = round(x$q25,2),
                             q50 = round(x$q50,2),
                             q75= round(x$q75, 2))
        z1 <- z %>%
            dplyr::filter(ESTATUS_CASO == 2) %>%
            dplyr::filter(DES_EDO_RES == edo) %>%
            dplyr::filter(ANO == year1) %>%
            dplyr::group_by(SEM) %>%
            dplyr::summarise(n = dplyr::n())
        z2 <- z %>%
            dplyr::filter(ESTATUS_CASO == 2) %>%
            dplyr::filter(DES_EDO_RES == edo) %>%
            dplyr::filter(ANO == year2) %>%
            dplyr::group_by(SEM) %>%
            dplyr::summarise(n = dplyr::n())
    } else if (juris == TRUE){
        x <- x %>%
            dplyr::filter(VEC_EST == 2) %>%
            dplyr::filter(DES_JUR == jurisdiccion & DES_EDO.x == edo) %>%
            dplyr::group_by(SEM, ANO) %>%
            dplyr::summarise(n = dplyr::n()) %>%
            dplyr::group_by(SEM) %>%
            tidyr::nest() %>%
            dplyr::mutate(risk = purrr::map(data, percentil)) %>%
            tidyr::unnest(risk, .drop =TRUE)
        x$data <- NULL
        x <- as.data.frame(x)
        x <- dplyr::distinct(x,
                             SEM = x$SEM,
                             q25 = round(x$q25,2),
                             q50 = round(x$q50,2),
                             q75= round(x$q75, 2))
        z$DES_JUR_RES <- stringr::str_trim(z$DES_JUR_RES)
        z1 <- z %>%
            dplyr::filter(ESTATUS_CASO == 2) %>%
            dplyr::filter(DES_JUR_RES == jurisdiccion & DES_EDO_RES == edo) %>%
            dplyr::filter(ANO == year1) %>%
            dplyr::group_by(SEM) %>%
            dplyr::summarise(n = dplyr::n())
        z2 <- z %>%
            dplyr::filter(ESTATUS_CASO == 2) %>%
            dplyr::filter(DES_JUR_RES == jurisdiccion & DES_EDO_RES == edo) %>%
            dplyr::filter(ANO == year2) %>%
            dplyr::group_by(SEM) %>%
            dplyr::summarise(n = dplyr::n())

    } else {
        x <- x %>%
            dplyr::filter(VEC_EST == 2) %>%
            dplyr::filter(DES_MPO.x == mun & DES_EDO.x == edo) %>%
            dplyr::group_by(SEM, ANO) %>%
            dplyr::summarise(n = dplyr::n()) %>%
            dplyr::group_by(SEM) %>%
            tidyr::nest() %>%
            dplyr::mutate(risk = purrr::map(data, percentil)) %>%
            tidyr::unnest(risk, .drop =TRUE)
        x$data <- NULL
        x <- as.data.frame(x)
        x <- dplyr::distinct(x,
                             SEM = x$SEM,
                             q25 = round(x$q25,2),
                             q50 = round(x$q50,2),
                             q75= round(x$q75, 2))
        z1 <- z %>%
            dplyr::filter(ESTATUS_CASO == 2) %>%
            dplyr::filter(DES_MPO_RES == mun & DES_EDO_RES == edo) %>%
            dplyr::filter(ANO == year1) %>%
            dplyr::group_by(SEM) %>%
            dplyr::summarise(n = dplyr::n())
        z2 <- z %>%
            dplyr::filter(ESTATUS_CASO == 2) %>%
            dplyr::filter(DES_MPO_RES == mun & DES_EDO_RES == edo) %>%
            dplyr::filter(ANO == year2) %>%
            dplyr::group_by(SEM) %>%
            dplyr::summarise(n = dplyr::n())
    }
    ##
    ggplot(data = x,
           aes(x = SEM)) +
        geom_area(aes(y = q75),
                  col = "grey90",
                  size = 0.8,
                  fill = "#f94d00", alpha = 0.80) +
        geom_area(aes(y = q50),
                  col = "white",
                  size = 0.8,
                  fill = "#ffcc00", alpha = 0.90) +
        geom_area(aes(y = q25),
                  col = "white",
                  size = 0.8,
                  fill = "#a7fc00",
                  alpha = 0.80) +
        geom_line(data = z1, aes(x = SEM, y = n),
                  alpha = 0.5,
                  size = 1.5,
                  colour =  "grey") +
        geom_point(data = z1, aes(x = SEM, y = n),
                   colour =  "red",
                   shape = 21,
                   alpha = 0.5,
                   fill = "black",
                   stroke = 2) +
        geom_line(data = z2, aes(x = SEM, y = n),
                  #alpha = 0.5,
                  size = 1.5,
                  colour =  "black") +
        geom_point(data = z2, aes(x = SEM, y = n),
                  colour =  "white",
                  shape = 21,
                  alpha = 0.5,
                  fill = "red",
                  stroke = 2) +
        ylab("casos") +
        xlab("Semanas epidemiológicas") +
        annotate("text",
                 label = "Epidémico",
                 x = 40,
                 y = max(x$q75),
                 size = 6,
                 colour = "black") +
        annotate("text",
                 label = "Alerta",
                 x = 32,
                 y = max(x$q50),
                 size = 6,
                 colour = "black") +
        annotate("text",
                 label = "Seguridad",
                 x = 32,
                 y = max(x$q25),
                 size = 6,
                 colour = "black") +
        annotate("text",
                 label = "Éxito",
                 x = 41,
                 y = min(x$q25)+3,
                 size = 6,
                 colour = "black") +
        scale_x_continuous(breaks = seq(from = 1,
                                        to = 52,
                                        by = 2),
                           limits = c(1,52)) +
        theme_classic() +
        if(max(x$q75) > max(z1$n)){
            scale_y_continuous(breaks = seq(from = 0,
                                            to = max(x$q75),
                                            by = scale_case))
        } else if (max(z1$n) > max(x$q75)) {
            scale_y_continuous(breaks = seq(from = 0,
                                            to = max(z1$n),
                                            by = scale_case))
        } else {
            scale_y_continuous(breaks = seq(from = 0,
                                            to = max(z2$n),
                                            by = scale_case))
        }
}
