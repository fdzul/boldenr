#' Action Map
#'
#'This function generate the action map of vector control.
#' @param data it is the database of vector actions.
#' @param mun  is the name of municipality.
#' @param cve_mpo is the id of municipality according the \href{**INEGI**}{https://www.inegi.org.mx/default.html}.
#' @param loc is the name of locality.
#' @param week is TRUE or FALSE. TRUE indicate the vector control actions in the week, else FALSE indicate the cumulative vector control.
#' @param num_loc is the id of locality according the \href{**INEGI**}{https://www.inegi.org.mx/default.html}.
#' @param blocks is the sf object of blocks.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a tmap object.
#' @export
#'
#'
#' @examples 1+1
action_map <- function(data, mun, cve_mpo, loc, week, num_loc, blocks){
    if(loc == "Rinconada") {
        z <- blocks |> dplyr::filter(MUNICIPIO %in%
                                              cve_mpo & LOCALIDAD == 5 |
                                              MUNICIPIO == 134 &
                                              LOCALIDAD == 3) |>
            dplyr::mutate(sec_manz = paste(SECCION, MANZANA, sep ="")) |>
            dplyr::mutate(Municipio = rep(x = loc, times = dplyr::n()))
    } else if(loc == "Veracruz"){
        z <- blocks |>
            dplyr::filter(MUNICIPIO %in% cve_mpo, LOCALIDAD == 1) |>
            dplyr::filter(!SECCION == 4228 &
                              !MANZANA %in% c(257, 275, 165,
                                              164, 252, 156,
                                              251, 164, 163,
                                              162, 158, 152,
                                              278, 151, 153,
                                              155, 159, 161,
                                              154)) |>
            dplyr::filter(!SECCION %in% c(585, 267, 266)) |>
            dplyr::mutate(sec_manz = paste(SECCION, MANZANA, sep ="")) |>
            dplyr::mutate(Municipio = ifelse(MUNICIPIO == 29,
                                             "Boca del Río",
                                             "Veracruz"))

    } else {
        z <- blocks |> dplyr::filter(MUNICIPIO %in% c(cve_mpo),
                                          LOCALIDAD == num_loc) |>
            dplyr::mutate(sec_manz = paste(SECCION, MANZANA, sep ="")) |>
            dplyr::mutate(Municipio = rep(loc, times = dplyr::n()))
    }

    ###
    x <- data |>
        dplyr::filter(Municipio %in% c(mun)) |>
        dplyr::filter(Semana.Epidemiologica %in% c(1:(lubridate::week(Sys.time())-1))) |>
        dplyr::mutate(man = as.numeric(Manzana),
                      sec = as.numeric(sector)) |>
        dplyr::mutate(sec_manz = paste(sec, man, sep =""))
    y <- data |>
        dplyr::filter(Municipio %in% c(mun)) |>
        dplyr::filter(Semana.Epidemiologica == lubridate::week(Sys.time())-1) |>
        dplyr::mutate(man = as.numeric(Manzana),
                      sec = as.numeric(sector)) |>
        dplyr::mutate(sec_manz = paste(sec, man, sep =""))
    ###
    z_x <-  dplyr::left_join(x = z[, c(-9,-10, -11,-12)],
                             y = x,
                             by = c("sec_manz", "Municipio")) |>
        dplyr::mutate(avance = rep("Acumulado", times = dplyr::n())) |>
        dplyr::filter(!is.na(sector))
    z_y <-  dplyr::left_join(x = z[, c(-9,-10, -11,-12)],
                             y = y,
                             by = c("sec_manz", "Municipio")) |>
        dplyr::mutate(avance = rep("En la semana", times = dplyr::n())) |>
        dplyr::filter(!is.na(sector))

    if(week == TRUE){
        zx <- z_y |>
            dplyr::mutate("Criterio_Operativo" = ifelse(Cobertura.en.Manzana <= 50, "Deficiente",
                                                        ifelse(Cobertura.en.Manzana >= 51 &
                                                                   Cobertura.en.Manzana <= 69,"Regular",
                                                               ifelse(Cobertura.en.Manzana >= 70 &
                                                                          Cobertura.en.Manzana <= 84,"Bueno",
                                                                      ifelse(Cobertura.en.Manzana >= 85, "Óptimo", NA)))))
        zx$Criterio_Operativo <- factor(zx$Criterio_Operativo,
                                        levels = c("Bueno", "Deficiente", "Óptimo","Regular")[c(3, 1,4, 2)])
        zx

    } else {
        zx <- z_x |>
            dplyr::mutate("Criterio_Operativo" = ifelse(Cobertura.en.Manzana <= 50, "Deficiente",
                                                        ifelse(Cobertura.en.Manzana >= 51 &
                                                                   Cobertura.en.Manzana <= 69,"Regular",
                                                               ifelse(Cobertura.en.Manzana >= 70 &
                                                                          Cobertura.en.Manzana <= 84,"Bueno",
                                                                      ifelse(Cobertura.en.Manzana >= 85, "Óptimo", NA)))))
        zx$Criterio_Operativo <- factor(zx$Criterio_Operativo,
                                          levels = c("Bueno", "Deficiente", "Óptimo","Regular")[c(3, 1,4, 2)])
        zx

    }
    tmap::tm_shape(z) +
        tmap::tm_fill(col = "gray85",
                lwd = 0.1,
                border.col = "white") +
        tmap::tm_layout(frame = F) +
        tmap::tm_shape(zx) +
        tmap::tm_polygons(col = "Criterio_Operativo",
                    title = "",   ## cambia la etiqueta de la leyenda
                    border.col =  "white",
                    lwd = 0.1,
                    style= "quantile", #"quantile", #"cont",# "cat", "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "cont"
                    #breaks = c(0, 100, 200, 300,400,500,600,700,800,900),
                    #n = 9,
                    max.categorias = 4,
                    #interval.closure = "left",
                    interval.closure = "right",
                    colorNA = "black",
                    alpha = 1,
                    textNA = "sin registro",
                    #auto.palette.mapping=FALSE,
                    #palette = c("#00FF00", "red", "#0000FF", "orange"),
                    palette = c("#0000FF","#00FF00", "#FF9900", "#FF0033"),#"Reds",#colorRamps::matlab.like2(4), #"GnBu", #rev(viridisLite::viridis(10)), #colorRamps::matlab.like(10),#"Reds",#viridisLite::viridis(9), #"Reds", ##
                    legend.show = T)




}
