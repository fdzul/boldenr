#' epi_channel
#'
#' @param x is the database that contains the 25th, 50th and 75th percentiles of dengue cases by state
#' @param y is the current dataset. Load with \link[boldenr]{read_dataset_bol} function.
#' @param edo is the target state. The epidemiological channel is provided by state.
#' @param jur is the target furidiction.
#' @param mpo is the target municipality.
#' @param loc is the locality target.
#' @param year1 is the year one.
#' @param year2 is the year two.
#' @param x_epi is the x position of the epidemic zone legend.
#' @param y_epi is the y position of the epidemic zone legend.
#' @param x_alerta is the x position of the alert zone legend.
#' @param y_alerta is the y position of the alert zone legend.
#' @param x_seg is the x position of the security zone legend.
#' @param y_seg is the y position of the security zone legend.
#' @param x_exito is the x position of the success zone legend.
#' @param y_exito is the y position of the success zone legend.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return
#' @export
#'
#' @examples
epi_channel <- function(x, y,
                        edo, jur, mpo, loc,
                        year1, year2,
                        x_epi, y_epi,
                        x_alerta,y_alerta,
                        x_seg, y_seg,
                        x_exito, y_exito){

    if(is.null(jur)){
        x <- x |>
            dplyr::mutate(DES_EDO_RES = stringr::str_replace_all(string = DES_EDO_RES,
                                                                 pattern = "  ",
                                                                 repl = " ")) |>
            dplyr::filter(DES_EDO_RES %in% c(edo))

        z1 <- y |>
            dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                              "OTROS PAISES DE LATINOAMERICA",
                                              "ESTADOS UNIDOS DE NORTEAMERICA")) |>
            dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                                "DENGUE GRAVE",
                                                "DENGUE NO GRAVE",
                                                "FIEBRE HEMORRAGICA POR DENGUE",
                                                "FIEBRE POR DENGUE")) |>
            dplyr::mutate(DES_EDO_RES = stringr::str_replace_all(string = DES_EDO_RES,
                                                                 pattern = "  ",
                                                                 repl = " ")) |>
            dplyr::filter(DES_EDO_RES == edo) |>
            dplyr::filter(ANO == year1) |>
            dplyr::group_by(SEM) |>
            dplyr::summarise(n = dplyr::n(), .groups = "drop")
        z2 <- y |>
            dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                              "OTROS PAISES DE LATINOAMERICA",
                                              "ESTADOS UNIDOS DE NORTEAMERICA")) |>
            dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                                "DENGUE GRAVE",
                                                "DENGUE NO GRAVE",
                                                "FIEBRE HEMORRAGICA POR DENGUE",
                                                "FIEBRE POR DENGUE")) |>
            dplyr::mutate(DES_EDO_RES = stringr::str_replace_all(string = DES_EDO_RES,
                                                                 pattern = "  ",
                                                                 repl = " ")) |>
            dplyr::filter(DES_EDO_RES == edo) |>
            dplyr::filter(ANO == year2) |>
            dplyr::group_by(SEM) |>
            dplyr::summarise(n = dplyr::n(), .groups = "drop")

        ggplot2::ggplot(data = x,
                        ggplot2::aes(x = SEM)) +
            ggplot2::geom_area(ggplot2::aes(y = q75),
                               col = "black",
                               size = 0.8,
                               fill = "#F44B1FFF",
                               alpha = 1) +
            ggplot2::theme_classic() +
            ggplot2::geom_area(ggplot2::aes(y = q50),
                               col = "white",
                               size = 0.8,
                               fill = "#FF9000FF",
                               alpha = 1) +
            ggplot2::geom_area(ggplot2::aes(y = q25),
                               col = "white",
                               size = .8,
                               fill = "#00F293FF",
                               alpha = .8) +
            ggplot2::ylab("Casos") +
            ggplot2::xlab("Semanas epidemiológicas") +
            ggplot2::annotate("text",
                              label = "Epidémico",
                              x = x_epi,
                              y = y_epi,
                              size = 6,
                              colour = "black") +
            ggplot2::annotate("text",
                              label = "Alerta",
                              x = x_alerta,
                              y = y_alerta,
                              size = 6,
                              colour = "black") +
            ggplot2::annotate("text",
                              label = "Seguridad",
                              x = x_seg,
                              y = y_seg,
                              size = 6,
                              colour = "black") +
            ggplot2::annotate("text",
                              label = "Éxito",
                              x = x_exito,
                              y = y_exito,
                              size = 6,
                              colour = "black") +
            ggplot2::scale_x_continuous(breaks = seq(from = 1,
                                                     to = 52,
                                                     by = 5),
                                        limits = c(1,52)) +
            ggplot2::geom_line(data = z1,
                               ggplot2::aes(x = SEM, y = n),
                               alpha = 0.5,
                               linewidth = 1.5,
                               colour =  "grey") +
            ggplot2::geom_point(data = z1,
                                ggplot2::aes(x = SEM, y = n),
                                colour =  "red",
                                shape = 21,
                                alpha = 0.5,
                                fill = "black",
                                stroke = 2) +
            ggplot2::geom_line(data = z2,
                               ggplot2::aes(x = SEM, y = n),
                               #alpha = 0.5,s
                               linewidth = 1.5,
                               colour =  "black") +
            ggplot2::geom_point(data = z2,
                                ggplot2::aes(x = SEM, y = n),
                                colour =  "white",
                                shape = 21,
                                alpha = 0.5,
                                fill = "red",
                                stroke = 2)
    } else{
        if(is.null(mpo)){
            x <- x |>
                dplyr::mutate(DES_EDO_RES = stringr::str_replace_all(string = DES_EDO_RES,
                                                                     pattern = "  ",
                                                                     repl = " "),
                              DES_JUR_RES = stringr::str_replace_all(string = DES_JUR_RES,
                                                                     pattern = "  ",
                                                                     repl = " ")) |>

                dplyr::filter(DES_EDO_RES %in% c(edo) &
                                  DES_JUR_RES %in% c(jur))

            z1 <- y |>
                dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                                  "OTROS PAISES DE LATINOAMERICA",
                                                  "ESTADOS UNIDOS DE NORTEAMERICA")) |>
                dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                                    "DENGUE GRAVE",
                                                    "DENGUE NO GRAVE",
                                                    "FIEBRE HEMORRAGICA POR DENGUE",
                                                    "FIEBRE POR DENGUE")) |>
                dplyr::mutate(DES_EDO_RES = stringr::str_replace_all(string = DES_EDO_RES,
                                                                     pattern = "  ",
                                                                     repl = " "),
                              DES_JUR_RES = stringr::str_replace_all(string = DES_JUR_RES,
                                                                     pattern = "  ",
                                                                     repl = " ")) |>
                dplyr::filter(DES_EDO_RES %in% edo &
                                  DES_JUR_RES %in% c(jur)) |>
                dplyr::filter(ANO == year1) |>
                dplyr::group_by(SEM) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop")
            z2 <- y |>
                dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                                  "OTROS PAISES DE LATINOAMERICA",
                                                  "ESTADOS UNIDOS DE NORTEAMERICA")) |>
                dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                                    "DENGUE GRAVE",
                                                    "DENGUE NO GRAVE",
                                                    "FIEBRE HEMORRAGICA POR DENGUE",
                                                    "FIEBRE POR DENGUE")) |>
                dplyr::mutate(DES_EDO_RES = stringr::str_replace_all(string = DES_EDO_RES,
                                                                     pattern = "  ",
                                                                     repl = " "),
                              DES_JUR_RES = stringr::str_replace_all(string = DES_JUR_RES,
                                                                     pattern = "  ",
                                                                     repl = " ")) |>
                dplyr::filter(DES_EDO_RES %in% edo &
                                  DES_JUR_RES %in% c(jur)) |>
                dplyr::filter(ANO == year2) |>
                dplyr::group_by(SEM) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop")

        } else {
            if(is.null(loc)){
                x <- x |>
                    dplyr::mutate(DES_EDO_RES = stringr::str_replace_all(string = DES_EDO_RES,
                                                                         pattern = "  ",
                                                                         repl = " "),
                                  DES_MPO_RES = stringr::str_replace_all(string = DES_MPO_RES,
                                                                         pattern = "  ",
                                                                         repl = " ")) |>

                    dplyr::filter(DES_EDO_RES %in% c(edo) &
                                      DES_MPO_RES %in% c(mpo))
                z1 <- y |>
                    dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                                      "OTROS PAISES DE LATINOAMERICA",
                                                      "ESTADOS UNIDOS DE NORTEAMERICA")) |>
                    dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                                        "DENGUE GRAVE",
                                                        "DENGUE NO GRAVE",
                                                        "FIEBRE HEMORRAGICA POR DENGUE",
                                                        "FIEBRE POR DENGUE")) |>
                    dplyr::mutate(DES_EDO_RES = stringr::str_replace_all(string = DES_EDO_RES,
                                                                         pattern = "  ",
                                                                         repl = " "),
                                  DES_MPO_RES = stringr::str_replace_all(string = DES_MPO_RES,
                                                                         pattern = "  ",
                                                                         repl = " ")) |>
                    dplyr::filter(DES_EDO_RES %in% edo &
                                      DES_MPO_RES %in% c(mpo)) |>
                    dplyr::filter(ANO == year1) |>
                    dplyr::group_by(SEM) |>
                    dplyr::summarise(n = dplyr::n(), .groups = "drop")
                z2 <- y |>
                    dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                                      "OTROS PAISES DE LATINOAMERICA",
                                                      "ESTADOS UNIDOS DE NORTEAMERICA")) |>
                    dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                                        "DENGUE GRAVE",
                                                        "DENGUE NO GRAVE",
                                                        "FIEBRE HEMORRAGICA POR DENGUE",
                                                        "FIEBRE POR DENGUE")) |>
                    dplyr::mutate(DES_EDO_RES = stringr::str_replace_all(string = DES_EDO_RES,
                                                                         pattern = "  ",
                                                                         repl = " "),
                                  DES_MPO_RES = stringr::str_replace_all(string = DES_MPO_RES,
                                                                         pattern = "  ",
                                                                         repl = " ")) |>
                    dplyr::filter(DES_EDO_RES %in% edo &
                                      DES_MPO_RES %in% c(mpo)) |>
                    dplyr::filter(ANO == year2) |>
                    dplyr::group_by(SEM) |>
                    dplyr::summarise(n = dplyr::n(), .groups = "drop")

            } else {
                x <- x |>
                    dplyr::mutate(DES_EDO_RES = stringr::str_replace_all(string = DES_EDO_RES,
                                                                         pattern = "  ",
                                                                         repl = " "),
                                  DES_LOC_RES = stringr::str_replace_all(string = DES_LOC_RES,
                                                                         pattern = "  ",
                                                                         repl = " ")) |>

                    dplyr::filter(DES_EDO_RES %in% c(edo) &
                                      DES_LOC_RES %in% c(loc))
                z1 <- y |>
                    dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                                      "OTROS PAISES DE LATINOAMERICA",
                                                      "ESTADOS UNIDOS DE NORTEAMERICA")) |>
                    dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                                        "DENGUE GRAVE",
                                                        "DENGUE NO GRAVE",
                                                        "FIEBRE HEMORRAGICA POR DENGUE",
                                                        "FIEBRE POR DENGUE")) |>
                    dplyr::mutate(DES_EDO_RES = stringr::str_replace_all(string = DES_EDO_RES,
                                                                         pattern = "  ",
                                                                         repl = " "),
                                  DES_LOC_RES = stringr::str_replace_all(string = DES_LOC_RES,
                                                                         pattern = "  ",
                                                                         repl = " ")) |>
                    dplyr::filter(DES_EDO_RES %in% edo &
                                      DES_LOC_RES %in% c(loc)) |>
                    dplyr::filter(ANO == year1) |>
                    dplyr::group_by(SEM) |>
                    dplyr::summarise(n = dplyr::n(), .groups = "drop")

                z2 <- y |>
                    dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                                      "OTROS PAISES DE LATINOAMERICA",
                                                      "ESTADOS UNIDOS DE NORTEAMERICA")) |>
                    dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                                        "DENGUE GRAVE",
                                                        "DENGUE NO GRAVE",
                                                        "FIEBRE HEMORRAGICA POR DENGUE",
                                                        "FIEBRE POR DENGUE")) |>
                    dplyr::mutate(DES_EDO_RES = stringr::str_replace_all(string = DES_EDO_RES,
                                                                         pattern = "  ",
                                                                         repl = " "),
                                  DES_LOC_RES = stringr::str_replace_all(string = DES_LOC_RES,
                                                                         pattern = "  ",
                                                                         repl = " ")) |>
                    dplyr::filter(DES_EDO_RES %in% edo &
                                      DES_LOC_RES %in% c(loc)) |>
                    dplyr::filter(ANO == year2) |>
                    dplyr::group_by(SEM) |>
                    dplyr::summarise(n = dplyr::n(), .groups = "drop")
            }
        }
    }
    ggplot2::ggplot(data = x,
                    ggplot2::aes(x = SEM)) +
        ggplot2::geom_area(ggplot2::aes(y = q75),
                           col = "black",
                           size = 0.8,
                           fill = "#F44B1FFF",
                           alpha = 1) +
        ggplot2::theme_classic() +
        ggplot2::geom_area(ggplot2::aes(y = q50),
                           col = "white",
                           size = 0.8,
                           fill = "#FF9000FF",
                           alpha = 1) +
        ggplot2::geom_area(ggplot2::aes(y = q25),
                           col = "white",
                           size = .8,
                           fill = "#00F293FF",
                           alpha = .8) +
        ggplot2::ylab("Casos") +
        ggplot2::xlab("Semanas epidemiológicas") +
        ggplot2::annotate("text",
                          label = "Epidémico",
                          x = x_epi,
                          y = y_epi,
                          size = 6,
                          colour = "black") +
        ggplot2::annotate("text",
                          label = "Alerta",
                          x = x_alerta,
                          y = y_alerta,
                          size = 6,
                          colour = "black") +
        ggplot2::annotate("text",
                          label = "Seguridad",
                          x = x_seg,
                          y = y_seg,
                          size = 6,
                          colour = "black") +
        ggplot2::annotate("text",
                          label = "Éxito",
                          x = x_exito,
                          y = y_exito,
                          size = 6,
                          colour = "black") +
        ggplot2::scale_x_continuous(breaks = seq(from = 1,
                                                 to = 52,
                                                 by = 5),
                                    limits = c(1,52)) +
        ggplot2::geom_line(data = z1,
                           ggplot2::aes(x = SEM, y = n),
                           alpha = 0.5,
                           linewidth = 1.5,
                           colour =  "grey") +
        ggplot2::geom_point(data = z1,
                            ggplot2::aes(x = SEM, y = n),
                            colour =  "red",
                            shape = 21,
                            alpha = 0.5,
                            fill = "black",
                            stroke = 2) +
        ggplot2::geom_line(data = z2,
                           ggplot2::aes(x = SEM, y = n),
                           #alpha = 0.5,s
                           linewidth = 1.5,
                           colour =  "black") +
        ggplot2::geom_point(data = z2,
                            ggplot2::aes(x = SEM, y = n),
                            colour =  "white",
                            shape = 21,
                            alpha = 0.5,
                            fill = "red",
                            stroke = 2)

}
