#' Generates a table of the cases confirmed by institution or age groups
#'
#' @param x is the dataset.
#' @param inst is the parameter for define the type of table. If the inst is TRUE the table is by institution, else by age group.
#' @param year is the current year.
#' @param state is the state.
#'
#'  @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a formattable object.
#'
#' @export
#'
#'
#' @examples 1+1
tb_conf <- function(x, inst, year, state){
    if (inst == TRUE){
        y <- x %>%
            dplyr::filter(ESTATUS_CASO == 2 &
                               ANO %in% c(year) &
                              DES_EDO_RES %in% c(state) &
                              DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                                    "DENGUE NO GRAVE",
                                                    "DENGUE GRAVE")) %>%
            dplyr::group_by(DES_INS_UNIDAD, DES_DIAG_FINAL) %>%
            dplyr::summarise(count = dplyr::n()) %>%
            tidyr::spread(key = "DES_DIAG_FINAL", fill = 0, value = "count")

        if(ncol(y) == 2) {
            if(names(y)[2] == c("DENGUE NO GRAVE")) {
                names(y) <- c("Instituciones", "DNG")
            } else if(names(y)[2] == c("DENGUE CON SIGNOS DE ALARMA")) {
                names(y) <- c("Instituciones", "DCSA")
            } else if (names(y)[2] == c("DENGUE GRAVE")) {
                names(y) <- c("Instituciones", "DG")
            }
        }

       if(ncol(y) == 4){
           names(y) <- c("Instituciones", "DCSA", "DG", "DNG")
       }

        y$Total <- rowSums(y[,-1])
        #Set a few color variables to make our table more visually appealing
        formattable::formattable(y,
                    align = c("c","c","c", "c", "c"),
                    list(Instituciones = formattable::formatter("span", style = x ~ ifelse(x %in% c("IMSS", "SSA"),
                                                                              style(
                                                                                  #display = "block",
                                                                                  "border-radius" = "4px",
                                                                                  "padding-right" = "4px",
                                                                                  color = "white",
                                                                                  "border-color" = "blue",
                                                                                  font.weight = "bold",
                                                                                  "background-color" = "orange"),
                                                                              style(color = "black",
                                                                                    font.weight = "bold",
                                                                                    "background-color" = "#DeF7E9"))),
                    DCSA = formattable::color_tile("#DeF7E9", "#71CA97"),
                    DG = formattable::color_tile("#DeF7E9", "#71CA97"),
                    DNG = formattable::color_tile("#DeF7E9", "#71CA97"),
                    Total = formattable::color_bar("orange")))
    } else {
        x$age_class <- cut(x$IDE_EDA_ANO,
                           c(0,1,5,10, 15, 20, 25, 30,
                             35, 40, 45, 50, 55, 60,65, max(x$IDE_EDA_ANO)),
                           labels = c("<1", "1-4", "5-9", "10-14",
                                      "15-19", "20-24", "25-29",
                                      "30-34", "35-39", "40-44",
                                      "45-49", "50-54", "55-59",
                                      "60-64", ">65"),
                           right = FALSE,
                           include.lowest = TRUE)
        #### cases of age-group and sex
        y <- x %>%
            dplyr::filter(ESTATUS_CASO == 2 &
                              ANO %in% c(year) &
                              DES_EDO_RES %in% c(state) &
                              DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                                    "DENGUE NO GRAVE",
                                                    "DENGUE GRAVE")) %>%
            dplyr::group_by(age_class, IDE_SEX) %>%
            dplyr::summarise(count = n()) %>%
            tidyr::spread(key = "IDE_SEX", value = "count", fill = 0)

        names(y)<- c("Edad", "Masculino", "Femenino")
        y$Total <- rowSums(y[,-1])
        #Set a few color variables to make our table more visually appealing
        formattable::formattable(y,
                    align = c("c","c","c", "c"),
                    list(Edad = formattable::color_tile("white", "lightblue"),
                        Edad = formattable::formatter("span", style = x ~ ifelse(x %in% c("10-14",
                                                                             "15-19",
                                                                             "20-24",
                                                                             "25-29"),
                                                                    style(color = "red",
                                                                          font.weight = "bold"),
                                                                    style(color = "black",
                                                                          font.weight = "bold"))),
                        Masculino = formattable::color_tile("#DeF7E9", "#71CA97"),
                        Femenino = formattable::color_tile("#DeF7E9", "#71CA97"),
                        Total = formattable::color_tile("white", "orange"),
                        Total = formattable::color_bar("orange")))
    }
}

