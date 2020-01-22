#' Generate a pyramid plot by health jurisdiction or state.
#'
#' @param x is the epidemiological dataset.
#' @param by_juris is the parameter for define the pyramid plot. if by_juris is false the plot is the state, else the plot is by jurisdiction.
#' @param state is the parameter for select the state.
#' @param label is the parameter of define the size of label.
#' @param year is the current year.
#' @param pal is the color of palette.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a pyramid plot of class ggplot.
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
pyramid_plot <- function(x, by_juris, state, year, pal){
    ## create the class
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
    ####
    ## we nead a dataset with age, sex, count, prop, loc
    dat <- x %>%
        dplyr::filter(ESTATUS_CASO == 2 &
                          ANO == year &
                          DES_EDO_RES == state) %>%
        dplyr::group_by(age_class, IDE_SEX) %>%
        dplyr::summarise(count = dplyr::n())
    ## barplots for male populations goes to the left (thus negative sign)
    dat$sex <- ifelse(dat$IDE_SEX == 1, "Masculino", "Femenino")
    dat$count_ok <- ifelse(dat$sex == "Masculino", -1*dat$count, dat$count)
    ###
    if(by_juris == FALSE){
        ggplot2::ggplot(dat, aes(x = age_class,
                        y = count_ok,
                        fill =  sex)) +
            ggplot2::geom_bar(data = subset(dat, sex == "Femenino"),
                     stat = "identity", alpha = 1, col = "gray90") +
            ggplot2::geom_text(data = subset(dat, sex == "Femenino"),
                      aes(label = count_ok),
                      colour = "white",
                      size = 3.5,
                      position = position_stack(vjust = 0.5)) +
            ggplot2::geom_bar(data = subset(dat, sex == "Masculino"),
                     stat = "identity", alpha = 1, col = "gray90") +
            ggplot2::geom_text(data = subset(dat, sex == "Masculino"),
                      aes(label = abs(count_ok)),
                      colour = "white",
                      size = 3.5,
                      position = position_stack(vjust = 0.5)) +
            ggplot2::coord_flip() +
            ggplot2::scale_fill_brewer("", palette = pal) +
            #theme_bw() +
            ggplot2::scale_x_discrete("Grupos Etarios") +
            ggplot2::scale_y_continuous("Número de Casos") +
            #scale_y_continuous("Porcentaje", labels = scales::percent) +
            #ggplot2::scale_y_continuous("Número de Casos",
            #                   breaks = seq(from = min(dat$count_ok)-1, to = max(dat$count_ok),
            #                                by= median(dat$count_ok)/max(dat$count_ok)),
             #                  labels = abs(seq(from = min(dat$count_ok)-1, to = max(dat$count_ok),
              #                                  by= median(dat$count_ok)/max(dat$count_ok)))) +
            ggplot2::theme(legend.position = c(.1, .9)) +
            #theme_linedraw() +
            ggplot2::theme(title = element_text(size = 12)) +
            ggplot2::theme(axis.text.x= element_text(size = 12)) +
            ggplot2::theme(axis.text.y= element_text(size = 14)) +
            ggplot2::theme(axis.title.x = element_text(size = 15)) +
            ggplot2::theme(axis.title.y = element_text(size = 15)) +
            ggplot2::theme(legend.background = element_rect(fill="transparent")) +
            ggplot2::theme(legend.text = element_text(size = 12))
    }else {
        ## we nead a dataset with age, sex, count, prop, loc
        dat2 <- x %>%
            dplyr::filter(ESTATUS_CASO == 2 &
                              ANO == year &
                              DES_EDO_RES == state) %>%
            dplyr::group_by(age_class, IDE_SEX, DES_JUR_RES) %>%
            dplyr::summarise(count = dplyr::n())
        ## barplots for male populations goes to the left (thus negative sign)
        dat2$sex <- ifelse(dat2$IDE_SEX == 1, "Masculino", "Femenino")
        dat2$count_ok <- ifelse(dat2$sex == "Masculino", -1*dat2$count, dat2$count)
        ggplot2::ggplot(dat2,
               aes_(x = age_class, y = count_ok, fill = sex)) +
            ggplot2::geom_bar(data = subset(dat2, sex == "Femenino"),
                     stat = "identity", alpha = 1, col = "gray90") +
            ggplot2::geom_text(data = subset(dat2, sex == "Femenino"),
                      aes_(label = count_ok),
                      colour = "white",
                      size = 3.5,
                      position = position_stack(vjust = 0.5)) +
            ggplot2::geom_bar(data = subset(dat2, sex == "Masculino"),
                     stat = "identity",alpha = 1, col = "gray90") +
            ggplot2::geom_text(data = subset(dat2, sex == "Masculino"),
                      aes_(label = abs(count_ok)),
                      colour = "white",
                      size = 3.5,
                      position = position_stack(vjust = 0.5)) +
            ggplot2::coord_flip() +
            ggplot2::scale_fill_brewer("", palette = pal) +
            ggplot2::facet_wrap(DES_JUR_RES) +
            #theme_bw() +
            ggplot2::scale_x_discrete("Grupos Etarios") +
            ggplot2::scale_y_continuous("Número de Casos",
                               breaks = seq(from = min(dat$count_ok)-1, to = max(dat$count_ok),
                                            by= 5),
                               labels = abs(seq(from = min(dat$count_ok)-1, to = max(dat$count_ok),
                                                by= 5))) +
            ggplot2::theme(legend.position = c(.1, .9)) +
            ggplot2::theme(axis.text.x= element_text(size = 12)) +
            ggplot2::theme(axis.text.y= element_text(size = 14)) +
            ggplot2::theme(axis.title.x = element_text(size = 15)) +
            ggplot2::theme(axis.title.y = element_text(size = 15)) +
            ggplot2::theme(legend.background = element_rect(fill="transparent")) +
            ggplot2::theme(legend.text = element_text(size = 12))
    }
}
