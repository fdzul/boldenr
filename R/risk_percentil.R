#' Calculate the risk based in the percentils
#'
#' @param x is the dataset of class data.frame, tibble or data.table.
#' @param var is tha variable target for calculate the risk.
#' @param en  is parameter for risk in english if en is true, else en is false the risk is in spanish.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a object of same class of x.
#'
#' @export
#'
#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom stats qnorm
#'
#'
#' @importFrom utils data
#' @examples 1+1
#'
#' @references xxxxx
#'
#' @seealso \link[boldenr]{epidemiological_channel}, \link[stats]{quantile}
#'
#' @details xxxx
risk_percentil <- function(x, var, en){
    cuts <- stats::quantile(unlist(x[,c(var)]),
                         probs = c(seq(from = .1, to = 1, by = 0.1)),
                         na.rm = TRUE,
                         names = FALSE)
    x$percentil <- findInterval(unlist(x[,c(var)]), vec = sort(cuts))
    if(en == TRUE){
        x$risk <- ifelse(x$percentil <= 4, "Low Risk",
                         ifelse(x$percentil > 4 & x$percentil < 8, "Moderate Risk",
                                ifelse(x$percentil >= 8 & x$percentil < 9, "High Risk",
                                       ifelse(x$percentil >= 9, "Very High Risk", NA))))
        x$risk <- factor(x$risk,
                         levels = c("High Risk", "Low Risk",
                                    "Moderate Risk", "Very High Risk")[c(2,3,1,4)])
        x
    } else {
        x$risk <- ifelse(x$percentil <= 4, "Riesgo Bajo",
                         ifelse(x$percentil > 4 & x$percentil < 8, "Riesgo Moderado",
                                ifelse(x$percentil >= 8 & x$percentil < 9, "Riesgo Alto",
                                       ifelse(x$percentil >= 9, "Riesgo Muy Alto", NA))))
        x$risk <- factor(x$risk,
                         levels = c("Riesgo Alto", "Riesgo Bajo",
                                    "Riesgo Moderado", "Riesgo Muy Alto")[c(4, 1, 3,2)])
        x
        }
}
