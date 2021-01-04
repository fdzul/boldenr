#' den_heatmap
#'
#' heatmap of dengue cases by state.
#'
#' @param x is the dengue dataset.
#' @param year is the year dataset.
#' @param breaks is the breaks of legend.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a ggplot object
#' @export
#'
#' @examples
den_heatmap <- function(x, year, breaks){
    y <- x %>%
        dplyr::filter(ANO == year) %>%
        dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                          "OTROS PAISES DE LATINOAMERICA",
                                          "ESTADOS UNIDOS DE NORTEAMERICA")) %>%
        dplyr::filter(DES_DIAG_FINAL %in%
                          c("DENGUE CON SIGNOS DE ALARMA", "DENGUE NO GRAVE",
                            "DENGUE GRAVE")) %>%
        dplyr::group_by(DES_EDO_RES, SEM) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        tidyr::pivot_wider(id_cols = c(DES_EDO_RES),
                           names_from = SEM,
                           values_from = n,
                           values_fill = 0) %>%
        tidyr::pivot_longer(cols = -DES_EDO_RES,
                            names_to = "week",
                            values_to = "n") %>%
        dplyr::mutate(edo = stringr::str_to_title(DES_EDO_RES),
                      week = as.numeric(week))

    z <- x %>%
        dplyr::filter(ANO == year) %>%
        dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                          "OTROS PAISES DE LATINOAMERICA",
                                          "ESTADOS UNIDOS DE NORTEAMERICA")) %>%
        dplyr::filter(DES_DIAG_FINAL %in%
                          c("DENGUE CON SIGNOS DE ALARMA", "DENGUE NO GRAVE",
                            "DENGUE GRAVE")) %>%
        dplyr::group_by(DES_EDO_RES) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(-n)) %>%
        dplyr::mutate(edo = stringr::str_to_title(DES_EDO_RES))

    y$edo <- factor(y$edo, levels = z$edo)


    ggplot2::ggplot(y, ggplot2::aes(y = edo,
                  x = week,
                  fill = n,
                  label = n)) +
        ggplot2::geom_raster(alpha = 1)+
        ggplot2::scale_fill_viridis_c(breaks  = c(0, seq(from = 0, to = max(y$n), by = breaks))) +
        ggplot2::geom_text(col = ifelse(y$n >= 120, "black","gray"),
                  size = 1, alpha = 0.4) +
        #theme_minimal()+
        cowplot::theme_cowplot() +
        ggplot2::ylab("") +
        ggplot2::xlab("") +
        ggplot2::labs(fill = "")+
        ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold", size = 9),
              axis.text.x = ggplot2::element_text(size = 6)) +
        ggplot2::scale_x_continuous(breaks = c(1:max(unique(y$week)))) +
        ggplot2::theme(legend.position = "bottom",
              legend.box.just = "center") +
        ggplot2::theme(legend.key.size = unit(.4, "cm"),
              legend.key.width = unit(2,"cm"),
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(-20,0,0,0))
}
