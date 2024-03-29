#' Plot the dengue cases by state and serotype
#'
#' @param dataset is the dataset of \href{**SINAVE**}{http://vectores.sinave.gob.mx/}.
#' @param year is the year of dataset.
#' @param scale_serotype is the scale of the dengue serotype plot.
#' @param x_serotype The x location of the dengue serotype plot. Sea also cowplot::draw_plot?.
#' @param y_serotype The y location of the dengue serotype plot. Sea also cowplot::draw_plot?.
#'
#' @seealso \link[cowplot]{draw_plot} and \link[cowplot]{ggdraw}.
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a plot of class ggplot.
#' @export
#'
#' @import stats
#' @import grid
#'
#' @examples 1+1
plot_state_serotype <- function(dataset, year, scale_serotype, x_serotype,
                                y_serotype){
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
    p <- ggplot2::ggplot(data = as.data.frame(x_state),
                         ggplot2::aes(y = n,
                             label = n,
                             x = stats::reorder(DES_EDO_RES2, n,
                                                FUN = sum),
                             fill = DES_DIAG_FINAL)) +
        ggplot2::geom_bar(position = ggplot2::position_stack(reverse = F),
                          stat = "identity",
                          color="white") +
        ggplot2::coord_flip() +
        ggplot2::ylab("Casos de Dengue") +
        ggplot2::xlab("Estados") +
        ggplot2::geom_text(position = ggplot2::position_stack(vjust = 0.5),
                           size = 2) +
        ggplot2::scale_fill_brewer("", palette = "YlOrRd") +
        ggplot2::theme_classic()  +
        ggplot2::theme(legend.position = c(0.9,0.72)) +
        ggplot2::theme(legend.text = ggplot2::element_text(size = 12)) +
        ggplot2::theme(title = ggplot2::element_text(size = 12)) +
        ggplot2::theme(legend.justification = c(1,0))  +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10)) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8, face = "bold")) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 14)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20)) +
        ggplot2::theme(strip.text = ggplot2::element_text(size = 20)) +
        ggplot2::theme(legend.background = ggplot2::element_rect(fill="transparent")) +
        ggplot2::theme(legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                       legend.text = ggplot2::element_text(face = "bold", size = 12))

    ### serotipos
    ser <- y %>%
        dplyr::filter(ESTATUS_CASO == 2 &
                          ANO == year &
                          DENGUE_SER_TRIPLEX %in% c(1:4)) %>%
        dplyr::group_by(DES_EDO_RES, DENGUE_SER_TRIPLEX) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::mutate(DES_EDO_RES2 = stringr::str_to_title(DES_EDO_RES)) %>%
        ggplot2::ggplot(ggplot2::aes(x = stats::reorder(DES_EDO_RES2, n,
                                               FUN = sum),
                            y = n,
                            label = n,
                            fill = factor(DENGUE_SER_TRIPLEX))) + ####
        ggplot2::geom_bar(position = "stack", stat = "identity", color="white") +
        ggplot2::coord_flip() +
        ggplot2::ylab("") + ggplot2::xlab("") +
        ggplot2::geom_text(position = ggplot2::position_stack(vjust = 0.5), size = 2, alpha = 0.5) + ####
        ggplot2::scale_fill_brewer("", palette = "YlOrRd") +
        ggplot2::theme_classic()  +
        ggplot2::theme(legend.position = c(0.9,0.3)) +  ####
        ggplot2::theme(legend.text = ggplot2::element_text(size = 10, face = "bold")) +
        ggplot2::theme(legend.justification = c(1,0))  +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8)) + ## EDO TEXT
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 6)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 14)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20)) +
        ggplot2::theme(strip.text = ggplot2::element_text(size = 20)) +
        ggplot2::theme(legend.background = ggplot2::element_rect(fill="transparent"),
                       panel.background = ggplot2::element_rect(fill = "transparent"),
                       plot.background = ggplot2::element_rect(fill = "transparent", color = NA)) +
        ggplot2::theme(legend.key = ggplot2::element_rect(colour = NA, fill = NA))

    ser <- ser + ggplot2::ggtitle(label = "Serotipos por Estado") + ####
        ggplot2::theme(plot.title = ggplot2::element_text(color="black",
                                                 size= 12,  ####
                                                 face="bold.italic"))

    #ggplot2::ggsave(filename = "serotipo.png", dpi = 200)
    ##
    ##
    #serotype <- grid::rasterGrob(png::readPNG("serotipo.png"),interpolate = T)
    ##
    ## add  to the plot the
    cowplot::ggdraw() +
        cowplot::draw_plot(p, 0, 0, 1, 1) +
        cowplot::draw_plot(ser,
                           x = x_serotype,
                           y = y_serotype,
                           0.35, 0.35,
                           scale = scale_serotype)

}
