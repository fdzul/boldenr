#' Generate a heatmap of confirmed and probable cases by health jurisdiction
#'
#' @param x is the dataset of sinave. For more information, see \url{http://vectores.sinave.gob.mx/}
#' @param state is the state.
#' @param years is the current year.
#' @param clus is parameter for define the numbers of clusters.
#' @param rtitlesize is the row title size.
#' @param mtextsize is the matrix text size.
#' @param rlabelsize is the row label size.
#' @param clabelsize is the column label size.
#' @param rlabeltextsize is the row text label size.
#' @param clabeltextsize is the column text label size.
#' @param heatmap_color is character specifying the heatmap colour scheme.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a heatmap of confirmed and probables cases by health jurisdiction.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples 1+1
#' @references xxxxx
#'
#' @seealso \link[boldenr]{epidemiological_channel}
#'
#' @details xxxxx
heatmap_jur_prob_conf <- function(x, state, years, clus, prob,
                                 rtitlesize,
                                 mtextsize,
                                 rlabelsize,
                                 clabelsize,
                                 rlabeltextsize,
                                 clabeltextsize,
                                 heatmap_color){

    x <- x %>%
        dplyr::filter(ANO %in% c(years) &
                          ESTATUS_CASO %in% c(prob) &
                          DES_EDO_RES %in% c(state) &
                          DES_DIAG_PROBABLE %in% c("DENGUE CON SIGNOS DE ALARMA",
                                                   "DENGUE NO GRAVE",
                                                   "DENGUE GRAVE")) %>%
        dplyr::group_by(SEM, DES_JUR_RES) %>%
        dplyr::summarise(count = dplyr::n())

    ##
    x$DES_JUR_RES <- stringr::str_to_title(x$DES_JUR_RES)
    y <- tidyr::spread(data = x,
                       key = "SEM",
                       fill = 0,
                       value = "count")
    ## convert to matrix
    z <- as.matrix(y[,-1])
    row.names(z) <- unlist(y[,1])
    row.names(z)

    superheat::superheat(z,
                         #title = "Casos de Dengue",
                         title.size = 7,
                         # row title
                         row.title = "",
                         #row.title.size = 6,
                         row.title.size = rtitlesize,
                         # col title
                         column.title = "",
                         column.title.size = 3,
                         ##
                         heat.pal = heatmap_color(n = max(x$count)),

                         # add as a scatterplot next to the columns
                         yr = rowSums(z),
                         order.rows = order(rowSums(z)),
                         yr.axis.name = "",
                         yr.axis.size = 8,
                         yr.point.size = 4,
                         yr.plot.type = "bar",
                         # change the color of the bar
                         yr.obs.col = heatmap_color(n = nrow(z)),
                         #yr.obs.col = ifelse(stringr::str_detect(row.names(z), stringr::coll("*")) ==  TRUE, "#26828EFF", "#440154FF"),

                         # add mpg as a scatter line plot next to the rows
                         yt = colSums(z),
                         yt.axis.name = "",
                         yt.axis.size = 8,
                         yt.point.size = 4,
                         yt.plot.type = "bar", #scatterline",
                         yt.obs.col = heatmap_color(n = ncol(z)),

                         # add text matrix
                         X.text = round(as.matrix(z), 1),
                         #X.text.size = 4,
                         X.text.size = mtextsize,
                         X.text.col = "white",
                         # change the size of the labels
                         left.label.size = rlabelsize,
                         bottom.label.size = clabelsize,
                         #
                         grid.hline.col = "white",
                         grid.vline.col = "white",
                         # change the size of the label text
                         #left.label.text.size = 4,
                         #bottom.label.text.size = 4,
                         # change the size of the label text
                         left.label.text.size = rlabeltextsize,
                         bottom.label.text.size = clabeltextsize,
                         #left.label.text.col = c("darkblue", "black")
                         # generate column clusters
                         left.label.text.alignment = "left",
                         n.clusters.rows = clus,
                         left.label = 'variable')
}
