#' animap_areal
#'
#' Animated map areal of dengue cases by municipality
#'
#' @param data is dengue dataset.
#' @param name is the name of the gif file.
#' @param dir is the directory where the animation will be saved.
#' @param country is logical value for indicating if the map is for all country o by state.
#' @param breaks is the numeric for defining the breaks of legend.
#' @param vel vel is the delay time between images. See also \link[tmap]{tmap_animation}.
#' @param save is the logical value for indicating if the gif is saved or not.
#' @param cve_edo is the id of state according the \href{**INEGI**}{https://www.inegi.org.mx/default.html}.
#' @param dot is the logical value, if dot TRUE the tmap is dot, else is areal.
#' @param pal is the palette.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @return a gif file of animation.
#' @export
#'
#' @seealso \link[tmap]{tmap_animation}
#'
#' @examples 1+1
#' @details \link[tmap]{tmap_animation}.
#'
#' @examples
animap_areal <- function(data, name, dir, country, breaks = NULL,
                         vel,save,
                         cve_edo = NULL, dot = NULL, pal = NULL){

    # Step 1. aggregate the cases by municipality and week ###
    x <- data %>%
        dplyr::filter(ANO == 2020) %>%
        dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                          "OTROS PAISES DE LATINOAMERICA",
                                          "ESTADOS UNIDOS DE NORTEAMERICA")) %>%
        dplyr::filter(DES_DIAG_FINAL %in%
                          c("DENGUE CON SIGNOS DE ALARMA", "DENGUE NO GRAVE",
                            "DENGUE GRAVE")) %>%
        dplyr::group_by(CVE_EDO_RES, CVE_MPO_RES, SEM) %>%
        dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>%
        tidyr::pivot_wider(id_cols = c(CVE_EDO_RES,CVE_MPO_RES),
                           names_from = SEM,
                           values_from = n,
                           values_fill = 0,
                           names_sort = TRUE)

    # Step 2. load the municipalities and states ####

    mex_sf <- rgeomex::AGEM_inegi19_mx %>%
        dplyr::mutate(CVE_ENT = as.numeric(CVE_ENT),
                      CVE_MUN = as.numeric(CVE_MUN))

    edo_sf <- rgeomex::AGEE_inegi19_mx

    # Step 3. joint ####
    y <- dplyr::left_join(x = mex_sf,
                          y = x,
                          by = c("CVE_ENT" = "CVE_EDO_RES",
                                 "CVE_MUN" = "CVE_MPO_RES"))

    # Step 4. sustitute the values na by 0 ####
    y[is.na(y)] <- 0

    # Step 5. extract the estate or not
    if(country == FALSE){
        y <- y %>% dplyr::filter(CVE_ENT %in% c(cve_edo))
        edo_sf <- edo_sf %>% dplyr::filter(CVE_ENT %in% c(cve_edo))
        mex_sf <- mex_sf %>% dplyr::filter(CVE_ENT %in% c(as.numeric(cve_edo)))

    } else{
        y
    }

    # Step 6. Make the dataset tidydaset (pivot_longer) ####
    y1 <- y %>%
        tidyr::pivot_longer(cols = !c(CVEGEO,CVE_ENT,CVE_MUN,NOMGEO, geometry),
                            names_to = "week",
                            values_to = "n") %>%
        as.data.frame()%>%
        sf::st_set_geometry(value = "geometry") %>%
        dplyr::mutate(week = as.numeric(week)) %>%
        dplyr::mutate(week = forcats::fct_reorder(paste("Semana Epidemiológica",
                                                        week, sep = " "),
                                                  week))

    # Step 7. make the map ####
    if(dot == TRUE){
        animated_map <- tmap::tm_shape(shp = mex_sf) +
            tmap::tm_polygons(col = "gray85",
                              border.col = "white",
                              lwd = 0.01) +
            tmap::tm_shape(shp = edo_sf) +
            tmap::tm_borders(col = "black",
                             lwd = 1) +
            tmap::tm_shape(shp = y1 %>% dplyr::filter(n >= 1))+
            tmap::tm_dots(size = "n",
                          title.size = "Número de Casos",
                          alpha = 0.5,
                          col = "darkred",
                          shape = 21,
                          scale = 1.5,
                          palette = pal(n = max(y1$n), direction = -1),
                          breaks = seq(from = 1, to = max(y1$n), by = breaks),
                          #n = 6,
                          border.col = "white") +
            tmap::tm_text(text = "n", size = .3, col = "white") +
            tmap::tm_facets(along = "week", free.coords = FALSE) +
            tmap::tm_layout(legend.show = FALSE) +
            tmap::tm_layout(main.title.size = 1,
                            legend.title.size  = .5,
                            legend.text.size =  .7)
        if(save == TRUE){
            tmap::tmap_animation(animated_map,
                                 dpi = 300,
                                 delay = vel,
                                 filename = paste(dir, paste(name, "animap.gif",
                                                             sep = "_"), sep = ""),
                                 width = 1400,
                                 height = 1400)
        } else{
            tmap::tmap_animation(animated_map,
                                 dpi = 300,
                                 delay = vel,
                                 width = 1400,
                                 height = 1400)
        }
    } else{
        animated_map <- tmap::tm_shape(shp = mex_sf) +
            tmap::tm_polygons(col = "gray85",
                              border.col = "white",
                              lwd = 0.01) +
            tmap::tm_shape(shp = edo_sf) +
            tmap::tm_borders(col = "black",
                             lwd = 1) +
            tmap::tm_shape(shp = y1 %>% dplyr::filter(n > 0)) +
            tmap::tm_fill(col = "n",
                          style = "cont",
                          breaks =  seq(from = 1, to = max(y1$n), by = breaks),
                          title = "",
                          #legend.is.portrait = FALSE,
                          palette = pal(n = max(y1$n),
                                        direction = -1)) +
            tmap::tm_text(text = "n", size = .3) +
            tmap::tm_borders(col = "white", lwd = .5) +
            tmap::tm_facets(along = "week", free.coords = FALSE) +
            tmap::tm_layout(main.title.size = 1,
                            legend.title.size  = .5,
                            legend.text.size =  .7)
        if(save == TRUE){
            tmap::tmap_animation(animated_map,
                                 dpi = 300,
                                 delay = vel,
                                 filename = paste(dir, paste(name, "animap.gif",
                                                             sep = "_"), sep = ""),
                                 width = 1400,
                                 height = 1400)
        } else{
            tmap::tmap_animation(animated_map,
                                 dpi = 300,
                                 delay = vel,
                                 width = 1400,
                                 height = 1400)
        }
    }


}
