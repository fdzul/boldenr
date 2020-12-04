#' Ultra Low Volumen (ULV) table.
#'
#' This function generate of thermal o cold ULV table.
#'
#' @param x is the ULV datasets.
#' @param jur is the name of jurisdiction.
#' @param mun es valor logico TRUE o FALSE para indicar si la tabla es por jurisdiccion y municipia o por por estado y jurisdiccion, respectivamente.
#' @param coldfog is logical value TRUE or FALSE to indicate if the table is by jurisdiction and municipality or by state and jurisdiction, respectively
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a table.
#'
#' @export
#'
#'
#' @examples 1+1
tb_ulv <- function(x, jur = NULL, mun, coldfog){
    if(coldfog == TRUE){
        x <- x %>% dplyr::filter(Tipo.de.Nebulizacion == "Nebulización UBV")
    } else {
        x <- x %>% dplyr::filter(Tipo.de.Nebulizacion == "Termonebulización")
    }

    if(TRUE == mun){
        xa <- x %>%
            #dplyr::filter(Tipo.de.Nebulización == "Nebulización UBV") %>%
            dplyr::filter(Semana_Epidemiologica %in% c(1:(lubridate::week(Sys.time())-1))) %>%
            dplyr::filter(Jurisdiccion == jur) %>%
            dplyr::group_by(Municipio) %>%
            dplyr::summarise("Hectáreas Trabajadas hasta la Semana" = sum(Hectareas.trabajadas, na.rm = TRUE),
                             "Insecticida Consumido hasta la Semana" = sum(Insecticida.consumido,na.rm = TRUE))
        xb <- x %>%
            #dplyr::filter(Tipo.de.Nebulización == "Nebulización UBV") %>%
            dplyr::filter(Jurisdiccion == jur) %>%
            dplyr::filter(Semana_Epidemiologica %in% c((lubridate::week(Sys.time())-9):(lubridate::week(Sys.time())-1))) %>%
            dplyr::group_by(Municipio) %>%
            dplyr::summarise("Hectáreas Trabajadas en los dos últimos meses" = sum(Hectareas.trabajadas, na.rm = TRUE),
                             "Insecticida Consumido en los dos últimos meses" = sum(Insecticida.consumido,na.rm = TRUE))
        xc <- x %>%
            #dplyr::filter(Tipo.de.Nebulización == "Nebulización UBV") %>%
            dplyr::filter(Jurisdiccion == jur) %>%
            dplyr::filter(Semana_Epidemiologica == lubridate::week(Sys.time())-1) %>%
            dplyr::group_by(Municipio) %>%
            dplyr::summarise("Hectáreas Trabajadas en la semana" = sum(Hectareas.trabajadas, na.rm = TRUE),
                             "Insecticida Consumido en la semana" = sum(Insecticida.consumido,na.rm = TRUE))
        #xab <- dplyr::left_join(x = xa, y = xb, by = "Municipio")
        xabc <- dplyr::left_join(x = dplyr::left_join(x = xa, y = xb, by = "Municipio"),
                                 y = xc,
                                 by = "Municipio") %>%
            tidyr::replace_na(list(`Hectáreas Trabajadas hasta la Semana` = 0,
                                   `Insecticida Consumido hasta la Semana` = 0,
                                   `Hectáreas Trabajadas en los dos últimos meses` = 0,
                                   `Insecticida Consumido en los dos últimos meses` = 0,
                                   `Hectáreas Trabajadas en la semana` = 0,
                                   `Insecticida Consumido en la semana` = 0)) %>%
            dplyr::arrange(desc(`Hectáreas Trabajadas hasta la Semana`))

        formattable::formattable(xabc,
                    align = c("c","c","c", "c", "c", "c", "c"),
                    list(
                        Municipio = formattable::formatter("span", style = x ~ formattable::style(color =  "black",
                                                                        font.weight = "bold")),
                        "Hectáreas Trabajadas hasta la Semana" = formattable::color_tile("#DeF7E9", "#71CA97"),
                        "Insecticida Consumido hasta la Semana" = formattable::color_tile("white", "orange"),
                        "Hectáreas Trabajadas en los dos últimos meses" = formattable::color_tile("#DeF7E9", "#71CA97"),
                        "Insecticida Consumido en los dos últimos meses" = formattable::color_tile("white", "orange"),
                        "Hectáreas Trabajadas en la semana" = formattable::color_tile("#DeF7E9", "#71CA97"),
                        "Insecticida Consumido en la semana" = formattable::color_tile("white", "orange")))
    } else {
        xa <- x %>%
            #dplyr::filter(Tipo.de.Nebulización == "Nebulización UBV") %>%
            dplyr::filter(Semana_Epidemiologica %in% c(1:(lubridate::week(Sys.time())-1))) %>%
            dplyr::group_by(Jurisdiccion) %>%
            dplyr::summarise("Hectáreas Trabajadas hasta la Semana" = sum(Hectareas.trabajadas, na.rm = TRUE),
                             "Insecticida Consumido hasta la Semana" = sum(Insecticida.consumido,na.rm = TRUE))
        xb <- x %>%
            #dplyr::filter(Tipo.de.Nebulización == "Nebulización UBV") %>%
            dplyr::filter(Semana_Epidemiologica %in% c((lubridate::week(Sys.time())-9):(lubridate::week(Sys.time())-1))) %>%
            dplyr::group_by(Jurisdiccion) %>%
            dplyr::summarise("Hectáreas Trabajadas en los dos últimos meses" = sum(Hectareas.trabajadas, na.rm = TRUE),
                             "Insecticida Consumido en los dos últimos meses" = sum(Insecticida.consumido,na.rm = TRUE))
        xc <- x %>%
            #dplyr::filter(Tipo.de.Nebulización == "Nebulización UBV") %>%
            dplyr::filter(Semana_Epidemiologica == lubridate::week(Sys.time())-1) %>%
            dplyr::group_by(Jurisdiccion) %>%
            dplyr::summarise("Hectáreas Trabajadas en la semana" = sum(Hectareas.trabajadas, na.rm = TRUE),
                             "Insecticida Consumido en la semana" = sum(Insecticida.consumido,na.rm = TRUE))
        xabc <- dplyr::left_join(x = dplyr::left_join(x = xa,
                                                      y = xb,
                                                      by = "Jurisdiccion"),
                                 y = xc,
                                 by = "Jurisdiccion") %>%
            tidyr::replace_na(list(`Hectáreas Trabajadas hasta la Semana` = 0,
                                   `Insecticida Consumido hasta la Semana` = 0,
                                   `Hectáreas Trabajadas en los dos últimos meses` = 0,
                                   `Insecticida Consumido en los dos últimos meses` = 0,
                                   `Hectáreas Trabajadas en la semana` = 0,
                                   `Insecticida Consumido en la semana` = 0)) %>%
            dplyr::arrange(desc(`Hectáreas Trabajadas hasta la Semana`))
        library(formattable)
        formattable::formattable(xabc,
                    align = c("c","c","c", "c", "c", "c", "c"),
                    list(
                        Jurisdiccion = formattable::formatter("span", style = x ~ formattable::style(color =  "black",
                                                                           font.weight = "bold")),
                        "Hectáreas Trabajadas hasta la Semana" = formattable::color_tile("#DeF7E9", "#71CA97"),
                        "Insecticida Consumido hasta la Semana" = formattable::color_tile("white", "orange"),
                        "Hectáreas Trabajadas en los dos últimos meses" = formattable::color_tile("#DeF7E9", "#71CA97"),
                        "Insecticida Consumido en los dos últimos meses" = formattable::color_tile("white", "orange"),
                        "Hectáreas Trabajadas en la semana" = formattable::color_tile("#DeF7E9", "#71CA97"),
                        "Insecticida Consumido en la semana" = formattable::color_tile("white", "orange")))

    }
}
