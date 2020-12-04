#' Plot the probables and confirmed cases of arbovirosis of years 2018 and 2019 in Veracruz State
#'
#' @param x is the dataset of probable and confirmed cases of arbovirosis.
#' @param state is the state target. its length is 1 o more.
#' @param year1 is the previous year of the current year.
#' @param year2 is the current year
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a plot the probables and confirmed cases of arbovirosis (DENV, CHIKV, ZIKV) of years 2018 and 2019 in Veracruz State. The lines and areas represent the cases of 2019 and 2018, respectively.
#'
#' @export
#'
#'
#'
#' @examples 1+1
#'
#' @references xxxxx
#'
#' @seealso \link[boldenr]{epidemiological_channel}
#'
#' @details xxx
plot_arbovirosis <- function(x, state, year1, year2){
    ##
    conf_prob <- function(x, state, status) {
        x %>%
            dplyr::filter(ESTATUS_CASO %in% c(status)  &
                              DES_EDO_RES %in% c(state)) %>%
            dplyr::group_by(ANO, SEM) %>%
            dplyr::summarise(n = dplyr::n())
    }
    ##
    confirmados <- tidyr::spread(data = conf_prob(x = x,
                                                  state = state,
                                                  status = 2),
                                 key = "ANO",
                                 value = "n",
                                 fill = NA)
    names(confirmados)[c(2,3)] <- paste("c",c(year1, year2), sep = "_")
    ##
    probables <- tidyr::spread(data = conf_prob(x = x,
                                                state = state,
                                                status = c(1, 2)),
                               key = "ANO",
                               value = "n")
    names(probables)[c(2,3)] <- paste("p",c(year1, year2), sep = "_")

    c1 <- rlang::sym(rlang::expr(!! paste("c", year1, sep = "_")))
    c2 <- rlang::sym(rlang::expr(!! paste("c", year2, sep = "_")))
    p1 <- rlang::sym(rlang::expr(!! paste("p", year1, sep = "_")))
    p2 <- rlang::sym(rlang::expr(!! paste("p", year2, sep = "_")))
    ##
    ggplot2::ggplot(data = probables,
                    ggplot2::aes(x = SEM)) +
        ggplot2::geom_area(ggplot2::aes(y = !! p1),
                  colour = "white",
                  fill = "lightgreen", alpha = 0.90) +
        ggplot2::theme_classic() +
        ggplot2::ylab("Casos Probables por Arbovirosis (Áreas)") +
        ggplot2::xlab("Semanas Epidemiológicas")+
        ggplot2::theme(title = ggplot2::element_text(size = 12)) +
        ggplot2::geom_area(ggplot2::aes(y = !! p2),
                  col = "white",
                  fill = "lightblue", alpha = 0.80) +
        ggplot2::annotate("text",
                 label = paste0(year1),
                 x = 38,
                 y = 1000,
                 size = 6,
                 colour = "black") +
        ggplot2::annotate("text",
                 label = paste0(year2),
                 x = 5,
                 y = 200,
                 size = 6,
                 colour = "black") +
        ggplot2::geom_line(data = confirmados,
                           ggplot2::aes(y = (!! c1)/(0.25),
                      x = SEM,
                      colour = paste("Confirmado", year1, sep = " ")),
                  size = 1) +
        ggplot2::scale_y_continuous(ggplot2::sec.axis = ggplot2::sec_axis(~.*0.25,
                                               name = "Casos Confirmado por Arbovirosis (Líneas)")) +
        ggplot2::theme(legend.position = c(0.2, 0.9)) +
        ggplot2::geom_point(data = confirmados,
                            ggplot2::aes(y =  (!! c1)/(0.25),
                       x = SEM,
                       colour = paste("Confirmado", year1, sep = " ")),
                   shape = 21,
                   fill = "darkgreen",
                   stroke = 2,
                   col = "darkgreen",
                   alpha = 0.70) +
        ggplot2::geom_line(data = confirmados,
                           ggplot2::aes(y = (!! c2)/(0.25),
                      x = SEM,
                      colour = paste("Confirmado", year2, sep = " ")),
                  size = 1) +
        ggplot2::geom_point(data = confirmados,
                            ggplot2::aes(y = (!! c2)/(0.25),
                       x = SEM,
                       colour = paste("Confirmado", year2, sep = " ")),
                   shape = 21,
                   fill = "lightblue",
                   stroke = 2,
                   col = "blue",
                   alpha = 0.70) +
        ggplot2::scale_colour_manual("", values = c("darkgreen",
                                           "darkblue"))
}

