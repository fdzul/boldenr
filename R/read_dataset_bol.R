#' Read the dataset of Sinave o vector
#'
#' This function has been designed for read the dataset of sinave or vector
#'
#' @param path is the directory where the files are located.
#' @param dataset is the parameter for define the type information.
#' @param inf is the parameter for define the type vector information. defaul is null for Sinave
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a dataset of sinave or vectors.
#'
#' @export
#'
#' @import utils
#' @importFrom utils data
#' @importFrom utils read.table
#'
#' @examples 1 + 1
read_dataset_bol <- function(path, dataset, inf = NULL){

    if (dataset == "sinave"){
        ## Step 1. Make the list files
        l <- list.files(path, full.names = TRUE, pattern = "txt")
        ## Step 2. Make the function
        unzip_list <- function(x){
            y <- data.table::fread(x, header = TRUE, quote = "",
                                   fill=TRUE,
                                   encoding = "Latin-1")
            #y <- readr::read_delim(x)
            y <- dplyr::select(y,
                               CVE_EDO_RES,
                               DES_EDO_RES,
                               CVE_JUR_RES, DES_JUR_RES,
                               CVE_MPO_RES, DES_MPO_RES,
                               CVE_LOC_RES, DES_LOC_RES,
                               IDE_SEX, IDE_EDA_ANO,
                               ESTATUS_CASO, DES_DIAG_PROBABLE,
                               DES_DIAG_FINAL, ANO, SEM, DES_INS_UNIDAD,
                               DENGUE_SER_TRIPLEX,
                               FEC_INGRESO)
        }
        ## Step 3. apply the function for each file and row bind
        #y <- data.table::fread(l[[1]], header = TRUE, quote="")
        x <- purrr::map_dfr(purrr::map(l, unzip_list), rbind)

        ## Step 4. correct the acentos and ñ
        x$DES_JUR_RES <- stringr::str_replace(x$DES_JUR_RES, pattern = "<d1>",replacement = "N")
        x$DES_LOC_RES <- stringr::str_replace(x$DES_LOC_RES, pattern = "<c1>",replacement = "A")
        x$DES_LOC_RES <- stringr::str_replace(x$DES_LOC_RES, pattern = "<d9>",replacement = "E")
        x$DES_LOC_RES <- stringr::str_replace(x$DES_LOC_RES, pattern = "<cd>",replacement = "I")
        x$DES_LOC_RES <- stringr::str_replace(x$DES_LOC_RES, pattern = "<d3>",replacement = "O")
        x$DES_LOC_RES <- stringr::str_replace(x$DES_LOC_RES, pattern = "<da>",replacement = "U")
        x
    } else if ("Lecturas" == inf) {
        l_files <- purrr::map(list.dirs(path = path,
                                        full.names = TRUE),
                              list.files, pattern = "txt", full.names = TRUE)
        l <- unlist(purrr::map(l_files, stringr::str_subset, c(inf)))
        x <- purrr::map_dfr(l, utils::read.table,
                            sep = "\t",
                            header = TRUE,
                            stringsAsFactors = FALSE,
                            fileEncoding = "UCS-2LE") %>%
            tidyr::separate(Municipio, into = c(NA, "Municipio"), extra = "merge") %>%
            tidyr::separate(Localidad, into = c(NA,"Localidad"), extra = "merge") %>%
            dplyr::mutate(Localidad = stringr::str_to_title(Localidad))
        x$Municipio <- stringr::str_trim(x$Municipio, side = "both")
        x
    } else if ("Control" == inf) {
        l_files <- purrr::map(list.dirs(path = path,
                                        full.names = TRUE),
                              list.files, pattern = "txt", full.names = TRUE)
        l <- unlist(purrr::map(l_files, stringr::str_subset, c(inf)))
        x <- purrr::map_dfr(l, read.table,
                            sep = "\t",
                            allowEscapes = TRUE,
                            header = TRUE,
                            skipNul = TRUE,
                            fileEncoding = "UCS-2LE") %>%
            tidyr::separate(Municipio, into = c(NA, "Municipio"), extra = "merge") %>%
            tidyr::separate(Localidad, into = c(NA,"Localidad"), extra = "merge") %>%
            tidyr::separate(Jurisdiccion, into = c(NA, "Jurisdiccion"), extra = "merge") %>%
            dplyr::mutate(Localidad = stringr::str_to_title(Localidad))
        x$Municipio <- stringr::str_trim(x$Municipio, side = "both")
        x$Jurisdiccion <- stringr::str_trim(x$Jurisdiccion, side = "both")
        x
    } else if ("Nebulizacion" == inf){
        l_files <- purrr::map(list.dirs(path = path,
                                        full.names = TRUE),
                              list.files, pattern = "txt", full.names = TRUE)
        l <- unlist(purrr::map(l_files, stringr::str_subset, c(inf)))
        x <- purrr::map_dfr(l, read.table,
                            sep = "\t",
                            allowEscapes = TRUE,
                            header = TRUE,
                            skipNul = TRUE,
                            fileEncoding = "UCS-2LE") %>%
            tidyr::separate(Municipio, into = c(NA, "Municipio"), extra = "merge") %>%
            tidyr::separate(Localidad, into = c(NA,"Localidad"), extra = "merge") %>%
            tidyr::separate(Jurisdiccion, into = c(NA, "Jurisdiccion"), extra = "merge") %>%
            dplyr::mutate(Localidad = stringr::str_to_title(Localidad))
        x$Municipio <- stringr::str_trim(x$Municipio, side = "both")
        x$Jurisdiccion <- stringr::str_trim(x$Jurisdiccion, side = "both")
        x
    } else if("RociadoAcaso" == inf){
        l_files <- purrr::map(list.dirs(path = path,
                                        full.names = TRUE),
                              list.files, pattern = "txt", full.names = TRUE)
        l <- unlist(purrr::map(l_files, stringr::str_subset, c(inf)))
        x <- purrr::map_dfr(l, read.table,
                            stringsAsFactors = FALSE,
                            sep = "\t",
                            allowEscapes = TRUE,
                            header = TRUE,
                            colClasses = c("character", "integer", "character",
                                           "character", "character", "character",
                                           "integer", "character", "character",
                                           "integer", "character", "character",
                                           "character",
                                           "integer", "integer", "character",
                                           "integer", "integer", "integer",
                                           "integer","character", "character",
                                           "character", "character", "character",
                                           "character" ),
                            skipNul = TRUE,
                            fileEncoding = "UTF-16") %>%
            tidyr::separate(Municipio, into = c(NA, "Municipio"), extra = "merge") %>%
            tidyr::separate(Localidad, into = c(NA,"Localidad"), extra = "merge") %>%
            tidyr::separate(Jurisdiccion, into = c(NA, "Jurisdiccion"), extra = "merge") %>%
            dplyr::mutate(Localidad = stringr::str_to_title(Localidad))
        x$Municipio <- stringr::str_trim(x$Municipio, side = "both")
        x$Jurisdiccion <- stringr::str_trim(x$Jurisdiccion, side = "both")
        x
    } else if("RociadoIntra" == inf){
        l_files <- purrr::map(list.dirs(path = path,
                                        full.names = TRUE),
                              list.files, pattern = "txt", full.names = TRUE)
        l <- unlist(purrr::map(l_files, stringr::str_subset, c(inf)))

        x <- purrr::map_dfr(l, read.table,
                            stringsAsFactors = FALSE,
                            sep = "\t",
                            allowEscapes = TRUE,
                            header = TRUE,
                            colClasses = c("character", "integer", "character",
                                           "character", "character", "character",
                                           "integer", "character", "integer",
                                           "character", "character", "character",
                                           "integer", "integer", "integer", "integer",
                                           "integer", "character", "character", "character",
                                           "integer", "character", "character",
                                           "character", "character"),
                            skipNul = TRUE,
                            fileEncoding = "UTF-16") %>%
            tidyr::separate(Municipio, into = c(NA, "Municipio"), extra = "merge") %>%
            tidyr::separate(Localidad, into = c(NA,"Localidad"), extra = "merge") %>%
            tidyr::separate(Jurisdiccion, into = c(NA, "Jurisdiccion"), extra = "merge") %>%
            dplyr::mutate(Localidad = stringr::str_to_title(Localidad))
        x$Municipio <- stringr::str_trim(x$Municipio, side = "both")
        x$Jurisdiccion <- stringr::str_trim(x$Jurisdiccion, side = "both")
        x
    } else if("Entomologico" == inf){
        l_files <- purrr::map(list.dirs(path = path,
                                        full.names = TRUE),
                              list.files, pattern = "txt", full.names = TRUE)
        l <- unlist(purrr::map(l_files, stringr::str_subset, c(inf)))
        x <- purrr::map_dfr(l, read.table,
                            stringsAsFactors = FALSE,
                            sep = "\t",
                            allowEscapes = TRUE,
                            header = TRUE,
                            skipNul = TRUE,
                            fileEncoding = "UTF-16") %>%
            tidyr::separate(Municipio, into = c(NA, "Municipio"), extra = "merge") %>%
            tidyr::separate(Localidad, into = c(NA,"Localidad"), extra = "merge") %>%
            tidyr::separate(Jurisdiccion, into = c(NA, "Jurisdiccion"), extra = "merge") %>%
            dplyr::mutate(Localidad = stringr::str_to_title(Localidad))
        x$Municipio <- stringr::str_trim(x$Municipio, side = "both")
        x$Jurisdiccion <- stringr::str_trim(x$Jurisdiccion, side = "both")
        x
    } else {
        l_files <- purrr::map(list.dirs(path = path,
                                        full.names = TRUE),
                              list.files, pattern = "txt", full.names = TRUE)
        l <- unlist(purrr::map(l_files, stringr::str_subset, c(inf)))
        x <- purrr::map_dfr(l, read.table,
                            stringsAsFactors = FALSE,
                            sep = "\t",
                            allowEscapes = TRUE,
                            na.strings = TRUE,
                            colClasses = c("character", "character", "character", "character", "character",
                                           "integer", "integer", "integer", "integer",
                                           "character", "character",
                                           "integer",
                                           "character", "character","character", "character",
                                           "character","character", "character", "character",
                                           "character",
                                           "integer",
                                           "character", "character"),
                            header = TRUE,
                            skipNul = TRUE,
                            fileEncoding = "UTF-16") %>%
            tidyr::separate(Municipio, into = c(NA, "Municipio"), extra = "merge") %>%
            tidyr::separate(Localidad, into = c(NA,"Localidad"), extra = "merge") %>%
            tidyr::separate(Jurisdiccion, into = c(NA, "Jurisdiccion"), extra = "merge") %>%
            dplyr::mutate(Localidad = stringr::str_to_title(Localidad))
        x$Municipio <- stringr::str_trim(x$Municipio, side = "both")
        x$Jurisdiccion <- stringr::str_trim(x$Jurisdiccion, side = "both")
        x
    }

}
