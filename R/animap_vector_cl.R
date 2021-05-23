#' animate maps of larvae control
#'
#' this function created the animated map for larvae control.
#'
#' @param path is the directory of the larvae control file.
#' @param locality is the locality target.
#' @param path_loc is the directory of shepefile dataset for limit locality.
#' @param vel is the delay time between images. See also \link[tmap]{tmap_animation}.
#' @param dir is the directory where the animation will be saved.
#' @param name is the name of the gif file.
#' @param x_leg is the x position of legend.
#' @param y_leg is the y position of legend.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @return a gif file of animation.
#' @export
#'
#' @seealso \link[tmap]{tmap_animation}
#'
#' @examples 1+1
#' @details \link[tmap]{tmap_animation}.
animap_vector_cl <- function(path, locality, path_loc, vel, dir, name, x_leg, y_leg){


    # Step 1. load the coontrol larvario dataset #####
    x  <- boldenr::read_dataset_bol(path = path,
                                    dataset = "vectores",
                                    inf = "Control") %>%
        dplyr::filter(Localidad %in% c(rgeomex::find_most_similar_string(locality, unique(Localidad)))) %>%
        dplyr::filter(!Semana.Epidemiologica %in% c(lubridate::epiweek(Sys.Date()))) %>%
        dplyr::filter(Tipo.de.Operativo %in% c("Barrido", "Focalizado")) %>%
        dplyr::mutate(sec_manz = paste(as.numeric(sector), as.numeric(Manzana), sep = "")) %>%
        dplyr::select(Localidad, sec_manz,Semana.Epidemiologica, Cobertura.en.Manzana) %>%
        tidyr::pivot_wider(id_cols = c(Localidad, sec_manz),
                           names_from = Semana.Epidemiologica,
                           values_from = Cobertura.en.Manzana,
                           values_fn = mean)

    # Step 2.1 load the locality dataset of inegi ####
    z <- sf::st_read(path_loc, quiet = TRUE)
    Encoding(z$NOMGEO) <- "latin1"
    z <- z %>%
        dplyr::filter(NOMGEO %in% c(rgeomex::find_most_similar_string(locality, unique(NOMGEO))) &
                          AMBITO %in% c("Urbana")) %>%
        sf::st_transform(crs = 4326) %>%
        sf::st_union()

    # Step 2.2 load the manzana dataset of ine ####
    y <- sf::st_read(path_ine,
                     quiet = TRUE) %>%
        sf::st_transform(crs = 4326) %>%
        dplyr::mutate(sec_manz =  paste(SECCION, MANZANA, sep = ""))

    # Step 2.3 exctract the block of the locality ####
    y <- y[z, ]

    # Step 3.1 joint the dataset ####
    y$sec_manz <- as.numeric(y$sec_manz)
    x$sec_manz <- as.numeric(x$sec_manz)
    xy <- dplyr::left_join(x = y,
                           y = x,
                           by = c("sec_manz")) %>%
        dplyr::filter(!is.na(Localidad)) %>%
        dplyr::select(-c(1:13)) %>%
        tidyr::pivot_longer(cols = c(-Localidad,  -geometry),
                            names_to = "week",
                            values_to = "n") %>%
        as.data.frame() %>%
        dplyr::mutate(week = as.numeric(week),
                      n = round(n, digits = 1)) %>%
        dplyr::mutate(week2 = forcats::fct_reorder(paste("Semana Epidemiológica",
                                                         week, sep = " "),
                                                   week)) %>%
        dplyr::filter(!is.na(n)) %>%
        sf::st_set_geometry(value = "geometry") %>%
        dplyr::mutate(cobertura = ifelse(n <= 50, "Deficiente",
                                         ifelse(n > 50 & n < 70, "Regular",
                                                ifelse(n >=70 & n < 85, "Bueno",
                                                       ifelse(n >=85, "Óptimo", NA)))))
    # Step 3.1 order the level of cobertura factor ####
    Encoding(xy$cobertura) <- "latin1"
    xy$cobertura <- factor(xy$cobertura,
                           levels = c("Bueno","Deficiente", "Óptimo", "Regular")[c(2,4,1,3)],
                           labels = c("Bueno","Deficiente", "Óptimo", "Regular")[c(2,4,1,3)])

    # Step 4. genera the control lavario animated map ####
    animated_map <- tmap::tm_shape(shp = z) +
        tmap::tm_polygons(col = "gray85",
                          border.col = "white",
                          lwd = 0.01) +
        tmap::tm_shape(shp = xy) +
        tmap::tm_fill(col = "cobertura",
                      style = "cat",
                      title = "",
                      palette = c("#F44B1FFF",
                                  "#FF9000FF",
                                  "#00F293FF",
                                  "#35BFFFFF")) +
        tmap::tm_facets(along = "week2", free.coords = FALSE) +
        tmap::tm_layout(legend.position = c(x_leg, y_leg))


    # Step 4. savecontrol lavario animated map ####

    tmap::tmap_animation(animated_map,
                         dpi = 300,
                         delay = vel,
                         filename = paste(dir, paste(name, "animated_map.gif",
                                                     sep = "_"), sep = ""),
                         width = 1400,
                         height = 1400)


}
