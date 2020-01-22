#' Generates a map of confirmed cases by municipalities
#'
#' @param x is the dataset of sinave.
#' @param y is sf object of all municipalities of  Mexico.
#' @param cve_state is the id of state, accord of INEGI.
#' @param state is the State.
#' @param years is the current year.
#' @param risk is the type map. If risk is true, the map is the risk, else the map is the cases.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a map of risk or confirmed cases.
#' @export
#'
#'
#' @examples 1+1
#'
#' @references xxxxx
#'
#' @seealso \link[boldenr]{epidemiological_channel}
#'
#' @details xxxx
map_conf_state <- function(x, y, cve_state, state, years, risk){
    ## Step 1.2 Select veracruz state
    y <- y %>% dplyr::filter(CVE_ENT == cve_state)
    ## Step 2.2 Count the cases by municipality
    x <- x %>%
        dplyr::filter(ESTATUS_CASO == 2 &
                          ANO == years &
                          DES_EDO_RES == state) %>%
        dplyr::group_by(CVE_EDO_RES, DES_MPO_RES,CVE_MPO_RES) %>%
        dplyr::summarise(n = dplyr::n())
    ## Step 3. Joint
    ## Step 3.1 convert the factor to numeric for the joint
    y$CVE_ENT <- as.numeric(y$CVE_ENT)
    y$CVE_MUN <- as.numeric(y$CVE_MUN)
    ## Step 3.2 Joint
    yx <- dplyr::left_join(x = y,
                           y = x,
                           by = c("CVE_ENT" = "CVE_EDO_RES",
                                  "CVE_MUN" = "CVE_MPO_RES"))
    ## Step 3.3. the na values is change by 0
    yx$n <- ifelse(is.na(yx$n), 0, yx$n)
    ## Step 4. estimate the risk
    yx_df <- sf::st_set_geometry(yx, NULL)
    yx_df <- risk_percentil(yx_df, var = "n", en = FALSE)

    ## Step 5. bind the df with sf
    yx <- sf:::cbind.sf(yx, yx_df)
    yx_positives <- dplyr::filter(yx, n > 0)
    ## Step 7. plot the results
    ##
    if(risk == TRUE){
        tmap::tm_shape(yx) +
            tmap::tm_compass(size = 4,
                       fontsize = 1,
                       type = "8star",
                       color.dark = "grey",
                       position = c(0.02, 0.3)) +
            tmap::tm_scale_bar(lwd = 2,
                         breaks = c(seq(from = 0, to = 300, by = 50)),
                         size = 0.8,
                         text.color = "blue",
                         color.dark = "grey",
                         position = c(0.04, .01)) +
            tmap::tm_fill(col = "risk",
                    title = "Riesgo",
                    #convert2density=TRUE,
                    border.col = "black",
                    lwd = 0.1,
                    style = "cat", #fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "cont"
                    #breaks = c(1, 2, 3,4,5,6,7),
                    n = 4,
                    #max.categorias = 9,
                    palette = c("red", "orange", "blue", "green"), #viridisLite::viridis(n = length(unique(yx_positives$n)), option = "D"),#"YlOrRd",#rev(colorRamps::blue2yellow(n= 7)),#rev(grDevices::heat.colors(n= 7, alpha = 0.8)), # rev(colorspace::heat_hcl(n = 7)), #colorRamps::green2red(n= 7),# "#Reds",#viridisLite::inferno(9),#"GnBu",#viridisLite::viridis(10), #"GnBu", #rev(viridisLite::viridis(10)), #colorRamps::matlab.like(10),#"Reds",#viridisLite::viridis(9), #"Reds", ##
                    legend.show = T) +
            tmap::tm_layout(
                main.title.size = 1.2,
                frame = FALSE,
                title.position = c(0.2, 0.95),
                #legend.just = "center",
                legend.text.color = "grey",
                legend.stack = "vertical",
                legend.text.size = 0.8,
                legend.title.fontface = "bold",
                legend.position = c(.8, 0.35))
    } else {
        tmap::tm_shape(yx) +
            tmap::tm_polygons(col = "gray92",
                        border.col = "white",
                        lwd = 0.3) +
            tmap::tm_compass(size = 4,
                       fontsize = 1,
                       type = "8star",
                       color.dark = "grey",
                       position = c(0.02, 0.3)) +
            tmap::tm_scale_bar(lwd = 2,
                         breaks = c(seq(from = 0, to = 300, by = 50)),
                         size = 0.8,
                         text.color = "blue",
                         color.dark = "grey",
                         position = c(0.04, .01))+
            tmap::tm_shape(yx_positives) +
            tmap::tm_fill(col = "n",
                    title = "Número de Casos",
                    #convert2density=TRUE,
                    #border.col = "black",
                    lwd = 0.1,
                    style = "pretty", #"cat", #fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "cont"
                    #breaks = c(1, 2, 3,4,5,6,7),
                    #n = 9,
                    #max.categorias = 9,
                    palette = viridisLite::viridis(n = length(unique(yx_positives$n)), option = "D"),#"YlOrRd",#rev(colorRamps::blue2yellow(n= 7)),#rev(grDevices::heat.colors(n= 7, alpha = 0.8)), # rev(colorspace::heat_hcl(n = 7)), #colorRamps::green2red(n= 7),# "#Reds",#viridisLite::inferno(9),#"GnBu",#viridisLite::viridis(10), #"GnBu", #rev(viridisLite::viridis(10)), #colorRamps::matlab.like(10),#"Reds",#viridisLite::viridis(9), #"Reds", ##
                    legend.show = T) +
            tmap::tm_text("n", size = 0.5, col = "white") +
            tmap::tm_borders(col = "white",
                       alpha = 0.2,
                       lwd = 0.3) +
            tmap::tm_layout(
                main.title.size = 1.2,
                frame = FALSE,
                title.position = c(0.2, 0.95),
                #legend.just = "center",
                legend.text.color = "grey",
                legend.stack = "vertical",
                legend.text.size = 0.8,
                legend.title.fontface = "bold",
                legend.position = c(.8, 0.35))
    }
}
