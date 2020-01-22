#' Plot the dengue cases by state and serotype
#'
#' @param dataset is the dataset of sinave http://vectores.sinave.gob.mx/.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a plot of class ggplot.
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @import stats
#' @import grid
#' @import png
#' @import mgrittr
#' @import stringr
#'
#' @importFrom magrittr %>%
#'
#' @examples 1+1
plot_state_serotype <- function(dataset, year){
    y <- dataset
    x_state <- y %>%
        dplyr::filter(ANO == year &
                   DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                         "DENGUE NO GRAVE", 
                                         "DENGUE GRAVE")) %>%
        dplyr::group_by(DES_EDO_RES, DES_DIAG_FINAL) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::mutate(DES_EDO_RES2 = stringr::str_to_title(DES_EDO_RES))

    x_state$DES_DIAG_FINAL <- factor(x_state$DES_DIAG_FINAL,
                                     levels = c("DENGUE CON SIGNOS DE ALARMA",
                                                "DENGUE GRAVE",
                                                "DENGUE NO GRAVE")[c(3,2,1)],
                                     labels = c("DCSA",
                                                "DG",
                                                "DNG")[c(3,2,1)])
    ###
    p <- ggplot2::ggplot(data = as.data.frame(x_state),
                aes(y = n,
                    label = n,
                    x = stats::reorder(DES_EDO_RES2, n,
                                       FUN = sum),
                    fill = DES_DIAG_FINAL)) +
        ggplot2::geom_bar(position = position_stack(reverse = F),
                 stat = "identity",
                 color="white") +
        ggplot2::coord_flip() +
        ggplot2::ylab("Casos de Dengue") +
        ggplot2::xlab("Estados") +
        ggplot2::geom_text(position = position_stack(vjust = 0.5),
                           size = 2) +
        ggplot2::scale_fill_brewer("", palette = "YlOrRd") +
        ggplot2::theme_classic()  +
        ggplot2::theme(legend.position = c(0.9,0.72)) +
        ggplot2::theme(legend.text = element_text(size = 12)) +
        ggplot2::theme(title = element_text(size = 12)) +
        ggplot2::theme(legend.justification = c(1,0))  +
        ggplot2::theme(axis.text.x = element_text(size = 12.5)) +
        ggplot2::theme(axis.text.y = element_text(size = 12)) +
        ggplot2::theme(axis.title.x = element_text(size = 14)) +
        ggplot2::theme(axis.title.y = element_text(size = 20)) +
        ggplot2::theme(strip.text = element_text(size = 20)) +
        ggplot2::theme(legend.background = element_rect(fill="transparent")) +
        ggplot2::theme(legend.key = element_rect(colour = NA, fill = NA))
    p
    ### serotipos
    ser <- x %>%
        dplyr::filter(ESTATUS_CASO == 2 &
                           ANO == year &
                          DENGUE_SER_TRIPLEX %in% c(1:4)) %>%
        dplyr::group_by(DES_EDO_RES, DENGUE_SER_TRIPLEX) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::mutate(DES_EDO_RES2 = stringr::str_to_title(DES_EDO_RES)) %>%
        ggplot2::ggplot(aes(x = stats::reorder(DES_EDO_RES2, n, 
                                               FUN = sum),
                   y = n,
                   label = n,
                   fill = factor(DENGUE_SER_TRIPLEX))) +
        ggplot2::geom_bar(position = "stack", stat = "identity", color="white") +
        ggplot2::coord_flip() +
        ggplot2::ylab("") + ggplot2::xlab("") +
        ggplot2::geom_text(position = position_stack(vjust = 0.5)) +
        ggplot2::scale_fill_brewer("", palette = "YlOrRd") +
        ggplot2::theme_classic()  +
        ggplot2::theme(legend.position = c(0.9,0.45)) +
        ggplot2::theme(legend.text = element_text(size = 12)) +
        ggplot2::theme(legend.justification = c(1,0))  +
        ggplot2::theme(axis.text.x = element_text(size = 12.5)) +
        ggplot2::theme(axis.text.y = element_text(size = 12)) +
        ggplot2::theme(axis.title.x = element_text(size = 14)) +
        ggplot2::theme(axis.title.y = element_text(size = 20)) +
        ggplot2::theme(strip.text = element_text(size = 20)) +
        ggplot2::theme(legend.background = element_rect(fill="transparent")) +
        ggplot2::theme(legend.key = element_rect(colour = NA, fill = NA))

    ser + ggplot2::ggtitle(label = "Serotipos por Estado") +
        ggplot2::theme(plot.title = element_text(color="black",
                                        size= 16,
                                        face="bold.italic"))

    ggplot2::ggsave(filename = "serotipo.png",
           dpi = 200)
    ##
    ##
    serotype <- grid::rasterGrob(png::readPNG("serotipo.png"),
                                 interpolate = T)
    ##
    ## add  to the plot the png
    p
    p + ggplot2::annotation_custom(serotype,
                                   ymin = 5,
                                   ymax = max(tapply(x_state$n, x_state$DES_EDO_RES2, sum)),
                                   xmin = 1,
                                   xmax = 25)
}
