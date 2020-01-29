#' Generate a map of hot blocks by city.
#'
#' @param loc is the locality.
#' @param x is the ovitraps dataset.
#' @param blocks is the the sf object of blocks the state.
#' @param cve_mpo is the id of municipality accord of INEGI.
#' @param sem1 is the current week.
#' @param risk is the argument for map. If the risk is true the map is elaborate with the percentil and risk, else map the eggs by blocks.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a map of risk or eggs map based in the percentil.
#'
#' @examples 1+1
#'
#' @references xxxxx
#'
#' @seealso \link[boldenr]{epidemiological_channel}
#'
#' @details xxx
hot_blocks <- function(loc, x, blocks, cve_mpo, sem1, risk){
    ## Step 1.2 apply the function for read
    x$Municipio <- stringr::str_trim(x$Municipio, side = "both")

    ##  Step 1.3 modify the values of the localidad variable
    x$Localidad <- ifelse(x$Localidad %in% c("Ampliación Las Bajadas",
                                             "Bajos Del Jobo (Puente Moreno)",
                                             "Campestre Las Bajadas",
                                             "Colinas De Santa Fe",
                                             "Colonia Chalchihuecan",
                                             "Colonia El Renacimiento",
                                             "Dos Lomas",
                                             "Las Amapolas",
                                             "Las Bajadas",
                                             "Mata De Pita",
                                             "Río Medio [Granja]",
                                             "Santa Teresa",
                                             "Valente Díaz",
                                             "Boca Del Río"),
                          "Veracruz",
                          x$Localidad)
    x$Localidad <- ifelse(x$Localidad == "Fraccionamiento Ciudad Olmeca", "Coatzacoalcos", x$Localidad)
    x$Localidad <- ifelse(x$Localidad == "Fraccionamiento Puente Moreno", "Martínez De La Torre", x$Localidad)
    x$Localidad <- ifelse(x$Localidad == "Paso Nacional", "Alvarado", x$Localidad)

    ## Step 2.
    ### mean of ovitraps and spread the dataset

    ## Step 2.1 Create the function
    fun_spread <- function(x){
        tidyr::spread(x,
                      key = "Semana.Epidemiologica",
                      value = "mean")
    }
    ### Step 2.2 apply the fuctions
    y <- x %>%
        dplyr::filter(Semana.Epidemiologica == sem1) %>%
        dplyr::group_by(Semana.Epidemiologica, Localidad, Sector, Manzana) %>%
        dplyr::summarise(mean = mean(Huevecillos, na.rm = TRUE)) %>%
        dplyr::group_by(Localidad) %>%
        tidyr::nest() %>%
        dplyr::mutate(tab_sem = purrr::map(data, fun_spread))

    ## Step 2.3
    w <- y[y$Localidad == loc, ] %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = c(tab_sem)) %>%
        dplyr::mutate(sec_manz = paste(Sector, Manzana, sep =""))
    w$Sector <- NULL;w$Manzana <- NULL; w$Localidad <- NULL

    ##
    if(loc == "Rinconada") {
        z <- blocks %>% dplyr::filter(MUNICIPIO %in% cve_mpo &
                                           LOCALIDAD == 5 |
                                           MUNICIPIO == 134 &
                                           LOCALIDAD == 3)
        } else if(loc == "Veracruz"){
            z <- blocks %>%
                dplyr::filter(MUNICIPIO %in% cve_mpo,
                               LOCALIDAD == 1) %>%
                dplyr::filter(!SECCION == 4228 &
                              !MANZANA %in% c(257, 275, 165,
                                              164, 252, 156,
                                              251, 164, 163,
                                              162, 158, 152,
                                              278, 151, 153,
                                              155, 159, 161,
                                              154)) %>%
                dplyr::filter(!SECCION %in% c(585, 267, 266))

            } else {
        z <- blocks %>% dplyr::filter(MUNICIPIO %in% c(cve_mpo),
                                          LOCALIDAD == 1)
        }
    ###
    z$sec_manz <- paste(z$SECCION, z$MANZANA, sep = "")
    z_hot <-  dplyr::left_join(x = z[, c(-9,-10, -11,-12)],
                                  y = w,
                                  by = c("sec_manz"))
    colnames(z_hot)[10] <- "huevos"
    z_hot <- z_hot %>% dplyr::filter(!is.na(huevos))
    #z_hot <- risk_percentil(x = z_hot, var = "huevos", en = FALSE)
    cuts <- stats::quantile(z_hot$huevos,
                     probs = c(seq(from = .1, to = 1, by = 0.1)),
                     na.rm = TRUE,
                     names = FALSE)
    z_hot$percentil <- findInterval(z_hot$huevos, vec = sort(cuts))
    z_hot$risk <- ifelse(z_hot$percentil <= 4, "Riesgo Bajo",
                     ifelse(z_hot$percentil > 4 & z_hot$percentil < 8, "Riesgo Moderado",
                          ifelse(z_hot$percentil >= 8 & z_hot$percentil < 9, "Riesgo Alto",
                                   ifelse(z_hot$percentil >= 9, "Riesgo Muy Alto", NA))))
    z_hot$risk <- factor(z_hot$risk,
                     levels = c("Riesgo Alto", "Riesgo Bajo",
                                "Riesgo Moderado", "Riesgo Muy Alto")[c(4, 1, 3,2)])
    if(risk == TRUE){
        tmap::tm_shape(z) +
            tmap::tm_fill(col = "gray85",
                    lwd = 0.1,
                    border.col = "white") +
            tmap::tm_layout(frame = F) +
            tmap::tm_shape(z_hot) +
            tmap::tm_polygons(col = "risk",
                        title = "",   ## cambia la etiqueta de la leyenda
                        border.col =  "white",
                        lwd = 0.1,
                        style = "cat", #"quantile", #"quantile", #"cont",# "cat", "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "cont"
                        #breaks = c(0, 100, 200, 300,400,500,600,700,800,900),
                        #n = 9,
                        max.categorias = 4,
                        #interval.closure = "left",
                        interval.closure = "right",
                        colorNA = "black",
                        #alpha = 0.7,
                        textNA = "sin registro",
                        #auto.palette.mapping=FALSE,
                        palette = rev(c("blue", "green", "orange", "red")),#"Reds",#colorRamps::matlab.like2(4), #"GnBu", #rev(viridisLite::viridis(10)), #colorRamps::matlab.like(10),#"Reds",#viridisLite::viridis(9), #"Reds", ##
                        legend.show = T)
    } else if(risk == FALSE){
        tmap::tm_shape(z) +
            tmap::tm_fill(col = "gray85",
                    lwd = 0.1,
                    border.col = "white") +
            tmap::tm_layout(frame = F) +
            tmap::tm_shape(z_hot) +
            tmap::tm_polygons(col = "huevos",
                        title = "",   ## cambia la etiqueta de la leyenda
                        border.col =  "white",
                        lwd = 0.1,
                        style= "quantile", #"quantile", #"cont",# "cat", "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "cont"
                        #breaks = c(0, 100, 200, 300,400,500,600,700,800,900),
                        #n = 9,
                        max.categorias = 3,
                        #interval.closure = "left",
                        interval.closure = "right",
                        colorNA = "black",
                        #alpha = 0.7,
                        textNA = "sin registro",
                        stretch.palette = TRUE,
                        #auto.palette.mapping = FALSE,
                        palette = c("blue", "green", "orange", "red"),#"Reds",#colorRamps::matlab.like2(4), #"GnBu", #rev(viridisLite::viridis(10)), #colorRamps::matlab.like(10),#"Reds",#viridisLite::viridis(9), #"Reds", ##
                        legend.show = T)
    } else {
        tmap::tm_shape(z) +
            tmap::tm_fill(col = "gray85",
                    lwd = 0.1,
                    border.col = "white") +
            tmap::tm_layout(frame = F) +
            tmap::tm_shape(z_hot) +
            tmap::tm_polygons(col = c("huevos", "risk"),
                        title = "",   ## cambia la etiqueta de la leyenda
                        border.col =  "white",
                        lwd = 0.1,
                        #style= "cat", #"quantile", #"cont",# "cat", "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "cont"
                        #breaks = c(0, 100, 200, 300,400,500,600,700,800,900),
                        #n = 9,
                        #max.categorias = 4,
                        #interval.closure = "left",
                        interval.closure = "right",
                        colorNA = "black",
                        #alpha = 0.7,
                        textNA = "sin registro",
                        #auto.palette.mapping=FALSE,
                        #palette = c("blue", "green", "orange", "red"),#"Reds",#colorRamps::matlab.like2(4), #"GnBu", #rev(viridisLite::viridis(10)), #colorRamps::matlab.like(10),#"Reds",#viridisLite::viridis(9), #"Reds", ##
                        legend.show = T) +
            tmap::tm_facets(sync = TRUE, ncol = 2)
        }
    }
