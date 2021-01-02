#' Generate a larval control table
#'
#' @param x is the dataset of control larvario.
#' @param jur is the Jurisdiccion.
#' @param mun is the municipio.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a table.
#'
#' @export
#'
#' @import formattable
#' @examples 1+1
#'
#' @references xxxxx
#'
#' @seealso \link[formattable]{formattable}
#'
#' @details xxx
cl_table <- function(x, jur = NULL, mun){
    if(TRUE == mun){
        xa <- x %>%
            dplyr::filter(Semana.Epidemiologica %in% c(1:(lubridate::week(Sys.time())-1))) %>%
            dplyr::filter(Jurisdiccion == jur) %>%
            dplyr::group_by(Municipio) %>%
            dplyr::summarise("Casas Visitadas hasta la Semana" = sum(Casas.Revisadas, na.rm = TRUE),
                             "Casas Trabajadas hasta la Semana" = sum(Casas.Trabajadas,na.rm = TRUE))
        xb <- x %>%
            dplyr::filter(Jurisdiccion == jur) %>%
            dplyr::filter(Semana.Epidemiologica %in% c((lubridate::week(Sys.time())-9):(lubridate::week(Sys.time())-1))) %>%
            dplyr::group_by(Municipio) %>%
            dplyr::summarise("Casas Visitadas en los dos últimos meses" = sum(Casas.Revisadas, na.rm = TRUE),
                             "Casas Trabajadas en los dos últimos meses" = sum(Casas.Trabajadas,na.rm = TRUE))
        xc <- x %>%
            dplyr::filter(Jurisdiccion == jur) %>%
            dplyr::filter(Semana.Epidemiologica == lubridate::week(Sys.time())-1) %>%
            dplyr::group_by(Municipio) %>%
            dplyr::summarise("Casas Visitadas en la Semana" = sum(Casas.Revisadas, na.rm = TRUE),
                             "Casas Trabajadas en la Semana" = sum(Casas.Trabajadas,na.rm = TRUE))
        #xab <- dplyr::left_join(x = xa, y = xb, by = "Municipio")
        xabc <- dplyr::left_join(x = dplyr::left_join(x = xa, y = xb, by = "Municipio"),
                                 y = xc,
                                 by = "Municipio") %>%
            tidyr::replace_na(list(`Casas Visitadas hasta la Semana` = 0,
                                   `Casas Trabajadas hasta la Semana` = 0,
                                   `Casas Visitadas en los dos últimos meses` = 0,
                                   `Casas Trabajadas en los dos últimos meses` = 0,
                                   `Casas Visitadas en la Semana` = 0,
                                   `Casas Trabajadas en la Semana` = 0)) %>%
            dplyr::arrange(desc(`Casas Trabajadas hasta la Semana`))
        formattable::formattable(xabc,
                    align = c("c","c","c", "c", "c", "c", "c"),
                    list(
                        Municipio = formattable::formatter("span", style = x ~ formattable::style(color =  "black",
                                                                        font.weight = "bold")),
                        "Casas Visitadas hasta la Semana" = formattable::color_tile("#DeF7E9", "#71CA97"),
                        "Casas Trabajadas hasta la Semana" = formattable::color_tile("white", "orange"),
                        "Casas Visitadas en los dos últimos meses" = formattable::color_tile("#DeF7E9", "#71CA97"),
                        "Casas Trabajadas en los dos últimos meses" = formattable::color_tile("white", "orange"),
                        "Casas Visitadas en la Semana" = formattable::color_tile("#DeF7E9", "#71CA97"),
                        "Casas Trabajadas en la Semana" = formattable::color_tile("white", "orange")))
    } else {
        xa <- x %>%
            dplyr::filter(Semana.Epidemiologica %in% c(1:(lubridate::week(Sys.time())-1))) %>%
            dplyr::group_by(Jurisdiccion) %>%
            dplyr::summarise("Casas Visitadas hasta la Semana" = sum(Casas.Revisadas, na.rm = TRUE),
                             "Casas Trabajadas hasta la Semana" = sum(Casas.Trabajadas,na.rm = TRUE))
        xb <- x %>%
            dplyr::filter(Semana.Epidemiologica %in% c((lubridate::week(Sys.time())-9):(lubridate::week(Sys.time())-1))) %>%
            dplyr::group_by(Jurisdiccion) %>%
            dplyr::summarise("Casas Visitadas en los dos últimos meses" = sum(Casas.Revisadas, na.rm = TRUE),
                             "Casas Trabajadas en los dos últimos meses" = sum(Casas.Trabajadas,na.rm = TRUE))
        xc <- x %>%
            dplyr::filter(Semana.Epidemiologica == lubridate::week(Sys.time())-1) %>%
            dplyr::group_by(Jurisdiccion) %>%
            dplyr::summarise("Casas Visitadas en la Semana" = sum(Casas.Revisadas, na.rm = TRUE),
                             "Casas Trabajadas en la Semana" = sum(Casas.Trabajadas,na.rm = TRUE))
        xabc <- dplyr::left_join(x = dplyr::left_join(x = xa,
                                                      y = xb,
                                                      by = "Jurisdiccion"),
                                 y = xc,
                                 by = "Jurisdiccion")
        formattable::formattable(xabc,
                    align = c("c","c","c", "c", "c", "c", "c"),
                    list(
                        Jurisdiccion = formattable::formatter("span", style = x ~ formattable::style(color =  "black",
                                                                           font.weight = "bold")),
                        "Casas Visitadas hasta la Semana" = formattable::color_tile("#DeF7E9", "#71CA97"),
                        "Casas Trabajadas hasta la Semana" = formattable::color_tile("white", "orange"),
                        "Casas Visitadas en los dos últimos meses" = formattable::color_tile("#DeF7E9", "#71CA97"),
                        "Casas Trabajadas en los dos últimos meses" = formattable::color_tile("white", "orange"),
                        "Casas Visitadas en la Semana" = formattable::color_tile("#DeF7E9", "#71CA97"),
                        "Casas Trabajadas en la Semana" = formattable::color_tile("white", "orange")))

    }
}
