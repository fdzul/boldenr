#' Heatmap of hospitalized dengue cases
#'
#' @param dataset is a dengue dataset.
#' @param state is the target state.
#' @param year is the year target
#' @param size_text is the font size of the text.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return
#' @export
#'
#' @examples
hosp_jur <- function(dataset, state, year, size_text){
    x <- dataset %>%
        dplyr::filter(ANO == year) %>%
        dplyr::filter(DES_EDO_RES %in% c(state)) %>%
        dplyr::filter(!is.na(FEC_INGRESO)) %>%
        dplyr::filter(DES_DIAG_FINAL %in%
                          c("DENGUE CON SIGNOS DE ALARMA", "DENGUE NO GRAVE",
                            "DENGUE GRAVE")) %>%
        dplyr::group_by(SEM, DES_JUR_RES) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        tidyr::pivot_wider(id_cols = c(DES_JUR_RES),
                           names_from = SEM,
                           values_from = n,
                           values_fill = 0) %>%
        tidyr::pivot_longer(cols = -DES_JUR_RES,
                            names_to = "week",
                            values_to = "n") %>%
        dplyr::mutate(DES_JUR_RES = stringr::str_to_title(DES_JUR_RES),
                      week = as.numeric(week))
    z <- dataset %>%
        dplyr::filter(ANO == year) %>%
        dplyr::filter(DES_EDO_RES %in% c(state)) %>%
        dplyr::filter(!is.na(FEC_INGRESO)) %>%
        dplyr::filter(DES_DIAG_FINAL %in%
                          c("DENGUE CON SIGNOS DE ALARMA", "DENGUE NO GRAVE",
                            "DENGUE GRAVE")) %>%
        dplyr::group_by(DES_JUR_RES) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::arrange(dplyr::desc(-n)) %>%
        dplyr::mutate(DES_JUR_RES = stringr::str_to_title(DES_JUR_RES))

    x$DES_JUR_RES <- factor(x$DES_JUR_RES, levels = z$DES_JUR_RES)


    plotly::ggplotly(ggplot2::ggplot(x,
                                     ggplot2::aes(y = DES_JUR_RES,
                                                  x = week,
                                                  fill = n,
                                                  label = n)) +
                         ggplot2::geom_tile(alpha = 1)+
                         ggplot2::scale_fill_viridis_c() +
                         ggplot2::geom_text(col = ifelse(x$n >= quantile(x$n, probs = .75),
                                                         "black","gray"),
                                            size = size_text,
                                            alpha = 0.4) +
                         #theme_minimal()+
                         cowplot::theme_cowplot() +
                         ggplot2::ylab("") +
                         ggplot2::xlab("") +
                         ggplot2::labs(fill = "")+
                         ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold",
                                                                            size = 9),
                                        axis.text.x = ggplot2::element_text(size = 6)) +
                         ggplot2::scale_x_continuous(breaks = c(1:max(unique(x$week)))) +
                         ggplot2::theme(legend.position = "bottom") +
                         ggplot2::theme(legend.key.size = unit(1, "cm"),
                                        legend.key.width = unit(.5,"cm"),
                                        legend.margin = ggplot2::margin(0,0,0,0),
                                        legend.box.margin = ggplot2::margin(-20,0,0,0)))
}
