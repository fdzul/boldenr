#' Generate of heatmap of confirmed or probables cases by localities.
#'
#' @param x is the dataset of sinave. For more information, see \url{http://vectores.sinave.gob.mx/}.
#' @param probable is the arguments for define the confirmed or probables cases. If probable is true the heatmap is the probables, else the heatmap is the confirmed cases.
#' @param year is the current year. Is a numeric.
#' @param state is the state.
#' @param rtitlesize is the row title size.
#' @param mtextsize is the matrix text size.
#' @param rlabelsize is the row label size.
#' @param clabelsize is the column label size.
#' @param rlabeltextsize is the row text label size.
#' @param clabeltextsize is the column text label size.
#' @param heatmap_color is character specifying the heatmap colour scheme.
#' @param n_loc is the number of localities.
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a heatmap confirmed or probables cases by localities.
#' @export
#'
#'
#'
#'
#' @references xxxxx
#'
#' @seealso \link[boldenr]{epidemiological_channel}
#' @seealso \link[superheat]{superheat}
#'
#' @details the heatmap is based in the package superheat and the function \link[superheat]{superheat}
heatmap_loc_prob_conf <- function(x, probable,
                                  year, state,
                                  rtitlesize,
                                  mtextsize,
                                  rlabelsize,
                                  clabelsize,
                                  rlabeltextsize,
                                  clabeltextsize,
                                  heatmap_color,
                                  n_loc){
    ## replace the �  in the wordl by the correct  sign
    x$DES_LOC_RES <- ifelse(x$DES_LOC_RES %in% c("CÓRDOBA", "CORDOBA UNO",
                                                 "CORDOBA DOS"),
                            "CORDOBA",
                            x$DES_LOC_RES)
    fun_replace <- function(x, replace, pat){
        stringr::str_replace(x$DES_LOC_RES, pattern = pat,replacement = replace)}

    ## subset the probables and confirmates
    subset_prob_conf <- function(x, status){
       y <- x %>%
            dplyr::filter(ANO %in% c(year) &
                              ESTATUS_CASO %in% c(status) &
                              DES_EDO_RES == state &
                              DES_DIAG_PROBABLE %in% c("DENGUE CON SIGNOS DE ALARMA",
                                                       "DENGUE NO GRAVE",
                                                       "DENGUE GRAVE")) %>%
            dplyr::group_by(SEM, DES_LOC_RES) %>%
            dplyr::summarise(count = dplyr::n())
        y$DES_LOC_RES <- stringr::str_to_title(stringr::str_to_title(y$DES_LOC_RES))
        y
    }
    y_p <- subset_prob_conf(x = x, status = c(1,2,3))
    y_c <- subset_prob_conf(x = x, status = c(2))

    ## spread the sem
    y_c <- tidyr::spread_(data = y_c, key = "SEM",fill = 0, value = "count")
    y_p <- tidyr::spread_(data = y_p, key = "SEM",fill = 0, value = "count")


    ## make the function to convert the df to matrix
    fun_matriz <- function(x){
        y <- as.matrix(x[,-1])
        row.names(y) <- unlist(x[,1])
        y
    }
    ## apply the function
    y_c <- fun_matriz(x = y_c)
    y_p <- fun_matriz(x = y_p)

    ## order the matrix in decreasing order
    y_c <- y_c[order(rowSums(y_c), decreasing=TRUE),]
    y_p <- y_p[order(rowSums(y_p), decreasing=TRUE),]


    ##
    fun_superheat_loc <- function(x, name_row, titulo){
        superheat::superheat(x,
                             title = titulo,
                             title.size = 5,
                             # row title
                             row.title = name_row,
                             #row.title.size = 6,
                             row.title.size = rtitlesize,
                             # col title
                             column.title = "",
                             column.title.size = 3,
                             # add mpg as a scatterplot next to the rows
                             yr = rowSums(x),
                             order.rows = order(rowSums(x)),
                             yr.axis.name = "Casos Acumulados",
                             yr.point.size = 4,
                             yr.plot.type = "bar",
                             # change the color of the points
                             yr.obs.col = rev(viridis::viridis(n = nrow(x))),
                             # add mpg as a scatter line plot next to the rows
                             yt = colSums(x),
                             yt.axis.name = "",
                             yt.axis.size = 10,
                             yt.point.size = 4,
                             yt.plot.type = "scatterline",
                             yt.obs.col = viridis::viridis(n = ncol(x)),
                             # add text matrix
                             X.text = round(as.matrix(x), 1),
                             X.text.size = mtextsize,
                             X.text.col = "white",
                             # Text alignment
                             left.label.text.alignment = "left",
                             # change the size of the labels
                             #left.label.size = 0.4,
                             #bottom.label.size = 0.01,
                             left.label.size = rlabelsize,
                             bottom.label.size = clabelsize,
                             ##
                             grid.hline.col = "white",
                             grid.vline.col = "white",
                             # change the size of the label text
                             left.label.text.size = rlabeltextsize,
                             bottom.label.text.size = clabeltextsize)
    }

    ##
    if(probable == TRUE){
        fun_superheat_loc(x = y_p[1:n_loc,,drop = FALSE],
                          name_row = "Localidades",
                          titulo = "Casos Probables de Dengue")
    }else{
        fun_superheat_loc(x = y_c[1:n_loc,,drop = FALSE],
                          name_row = "Localidades",
                          titulo = "Casos Confirmados de Dengue")
    }


}
