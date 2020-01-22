#' Heatmap of hospitalized cases
#'
#' Generates a Heatmap of hospitalized cases by health jurisdiction.
#'
#' @param x is the sinave datasets. For more information, see \url{http://vectores.sinave.gob.mx/}.
#' @param state is the state.
#' @param year is the year.
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
#' @return A heatmap of hospitalized cases by health jurisdiction.
#' @export
#'
#'
#' @examples 1+1
#'
#' @references xxxxx
#'
#' @seealso \link[boldenr]{epidemiological_channel}
#'
#' @details xxx
heatmap_jur_hosp <- function(x, state, year,
                             rtitlesize,
                             mtextsize,
                             rlabelsize,
                             clabelsize,
                             rlabeltextsize,
                             clabeltextsize,
                             heatmap_color,
                             r_ticks){
    x$FEC_INGRESO <- lubridate::ymd(x$FEC_INGRESO)
    x <- x %>%
        dplyr::filter(ESTATUS_CASO == 2 &
                          DES_EDO_RES %in% c(state) &
                          ANO %in% c(year) & !is.na(FEC_INGRESO)) %>%
        dplyr::group_by(SEM, DES_JUR_RES) %>%
        dplyr::summarise(n = dplyr::n())

    y <- tidyr::spread(data = x, key = "SEM", value = "n", fill = 0)
    y$DES_JUR_RES <- stringr::str_to_title(y$DES_JUR_RES)
    z <- as.matrix(y[,-1])
    row.names(z) <- unlist(y[,1])
    superheat::superheat(z,
                         title = "Casos Hospitalizados por Dengue",
                         title.size = 4,
                         ##
                         heat.pal = heatmap_color(n = max(x$n)),
                         # row title
                         row.title = "Jurisdicciones Sanitarias",
                         row.title.size = rtitlesize,
                         left.label.text.alignment = "left",
                         # add  as a scatterplot next to the columns
                         yr = rowSums(z),
                         order.rows = order(rowSums(z)),
                         yr.axis.name = "Casos Acumulados",
                         yr.point.size = 4,
                         yr.plot.type = "bar",
                         # change the color of the points
                         yr.obs.col = heatmap_color(n = nrow(z)),
                         # add mpg as a scatter line plot next to the rows
                         yt = colSums(z),
                         yt.axis.name = "",
                         #yt.axis.size = 14,
                         yt.point.size = 4,
                         yt.plot.type = "bar", #scatterline",
                         yt.obs.col = heatmap_color(n = ncol(z)), #point.col2,
                         # add text matrix
                         X.text = round(as.matrix(z), 1),
                         X.text.size = mtextsize,
                         X.text.col = "white",
                         # change the size of the labels
                         left.label.size = rlabelsize,
                         bottom.label.size = clabelsize,
                         # change the size of the label text
                         left.label.text.size = rlabeltextsize,
                         bottom.label.text.size = clabeltextsize,
                         # increase the space between the heatmap and legend
                         #legend.vspace = 1,
                         ###
                         grid.hline.col = "white",
                         grid.vline.col = "white")
}
