#' Plot the probables and confirmed cases of arbovirosis of years 2018 and 2019 in Veracruz State
#'
#' @param x is the dataset of probable and confirmed cases of arbovirosis.
#' @param state is the state target. its length is 1 o more.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a plot the probables and confirmed cases of arbovirosis (DENV, CHIKV, ZIKV) of years 2018 and 2019 in Veracruz State. The lines and areas represent the cases of 2019 and 2018, respectively.
#'
#' @export
#'
#'
#' @importFrom magrittr %>%
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

    c1 <- sym(expr(!! paste("c", year1, sep = "_")))
    c2 <- sym(expr(!! paste("c", year2, sep = "_")))
    p1 <- sym(expr(!! paste("p", year1, sep = "_")))
    p2 <- sym(expr(!! paste("p", year2, sep = "_")))
    ##
    ggplot(data = probables,
           aes(x = SEM)) +
        geom_area(aes(y = !! p1),
                  colour = "white",
                  fill = "lightgreen", alpha = 0.90) +
        theme_classic() +
        ylab("Casos Probables por Arbovirosis (Áreas)") +
        xlab("Semanas Epidemiológicas")+
        theme(title = element_text(size = 12)) +
        geom_area(aes(y = !! p2),
                  col = "white",
                  fill = "lightblue", alpha = 0.80) +
        annotate("text",
                 label = paste0(year1),
                 x = 38,
                 y = 1000,
                 size = 6,
                 colour = "black") +
        annotate("text",
                 label = paste0(year2),
                 x = 5,
                 y = 200,
                 size = 6,
                 colour = "black") +
        geom_line(data = confirmados,
                  aes(y = (!! c1)/(0.25),
                      x = SEM,
                      colour = paste("Confirmado", year1, sep = " ")),
                  size = 1) +
        scale_y_continuous(sec.axis = sec_axis(~.*0.25,
                                               name = "Casos Confirmado por Arbovirosis (Líneas)")) +
        theme(legend.position = c(0.2, 0.9)) +
        geom_point(data = confirmados,
                   aes(y =  (!! c1)/(0.25),
                       x = SEM,
                       colour = paste("Confirmado", year1, sep = " ")),
                   shape = 21,
                   fill = "darkgreen",
                   stroke = 2,
                   col = "darkgreen",
                   alpha = 0.70) +
        geom_line(data = confirmados,
                  aes(y = (!! c2)/(0.25),
                      x = SEM,
                      colour = paste("Confirmado", year2, sep = " ")),
                  size = 1) +
        geom_point(data = confirmados,
                   aes(y = (!! c2)/(0.25),
                       x = SEM,
                       colour = paste("Confirmado", year2, sep = " ")),
                   shape = 21,
                   fill = "lightblue",
                   stroke = 2,
                   col = "blue",
                   alpha = 0.70) +
        scale_colour_manual("", values = c("darkgreen",
                                           "darkblue"))
}

