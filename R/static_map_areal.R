#' static_map_areal
#'
#' static map of aggregated dengue cases by municipality
#'
#' @param x is the dengue dataset.
#' @param breaks is the breaks of legend.
#' @param week_start is the week start for subset data.
#' @param week_end is the week end for subset data.
#' @param country is a logical parameter (TRUE or FALSE), if TRUE is all municipalities of Mexico else a state specific.
#' @param cve_edo is default NULL, but is id numeric for each state if country FALSE.
#' @param year is the year dataset.
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a ggplot object
#' @export
#'
#' @examples
static_map_areal <- function(x, breaks, week_start,week_end, country, cve_edo = NULL, year){

    # step 1. load the municipalities sf object
    w <- rgeomex::AGEM_inegi19_mx %>%
        dplyr::mutate(CVE_ENT = as.numeric(CVE_ENT),
                      CVE_MUN = as.numeric(CVE_MUN))

    # step 2. load the state sf object
    z <- rgeomex::AGEE_inegi19_mx


    # Step 3. aggrageted the dengue cases by minicipality ####
    x <- x %>%
        dplyr::filter(ANO %in% c(year)) %>%
        dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                          "OTROS PAISES DE LATINOAMERICA",
                                          "ESTADOS UNIDOS DE NORTEAMERICA")) %>%
        dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                            "DENGUE NO GRAVE",
                                            "DENGUE GRAVE")) %>%
        #dplyr::filter(SEM %in% c(week_ini:(lubridate::epiweek(Sys.Date())))) %>%
        dplyr::filter(SEM %in% c(week_start:week_end)) %>%
        dplyr::group_by(CVE_EDO_RES, CVE_MPO_RES) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop")

    # Step 4. joint ####
    y <- dplyr::left_join(x = w,
                          y = x,
                          by = c("CVE_ENT" = "CVE_EDO_RES",
                                 "CVE_MUN" = "CVE_MPO_RES"))
    # Step 5. sustitute the values na by 0 ####
    y[is.na(y)] <- 0

    if(country == FALSE){
        y <- y %>% dplyr::filter(CVE_ENT %in% c(cve_edo))
        z <- z %>% dplyr::filter(CVE_ENT %in% c(cve_edo))
    } else{
        y
    }


    # Step 6. Plot the map ####
    ggplot2::ggplot(data = y) +
        ggplot2::geom_sf(color = "white",
                         size = 0.001) +
        ggplot2::geom_sf(data = y %>% dplyr::filter(n >= 1),
                         ggplot2::aes(fill = n),
                         color = "white",
                         size = 0.001) +
        ggplot2::scale_fill_viridis_c("Casos",
                                      direction = -1,
                                      option = "B",
                                      breaks  = c(seq(from = 1, to = max(y$n), by = breaks))) +
        ggplot2::geom_sf(data = z,
                         fill = NA,
                         color = "black",
                         size = 0.5) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::theme(legend.key.size = ggplot2::unit(.4, "cm"),
                       legend.key.width = ggplot2::unit(1.5,"cm"),
                       legend.margin= ggplot2::margin(0,0,0,0),
                       legend.box.margin= ggplot2::margin(-20,0,0,0))
}
