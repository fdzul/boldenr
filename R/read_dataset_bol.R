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
        read_dat <- function(x){
            select_columns <- c("VEC_ID",
                                "IDE_EDA_ANO", "IDE_SEX",
                                "IDE_CAL", "NUM_EXT", "NUM_INT",
                                "IDE_COL", "IDE_CP",
                                "CVE_LOC_RES", "DES_LOC_RES", "CVE_MPO_RES", "DES_MPO_RES",
                                "DES_JUR_RES", "CVE_EDO_RES", "DES_EDO_RES",
                                "ESTATUS_CASO", "CVE_DIAG_PROBABLE", "DES_DIAG_PROBABLE", "DES_DIAG_FINAL",
                                "FEC_INI_SIGNOS_SINT", "ANO", "SEM",
                                "MANEJO",
                                "DES_INS_UNIDAD", "DENGUE_SER_TRIPLEX","FEC_INGRESO")
            column_type = list(vroom::col_character(),
                               vroom::col_integer(),vroom::col_integer(),
                               vroom::col_character(),vroom::col_character(), vroom::col_character(),
                               vroom::col_character(), vroom::col_character(),

                               vroom::col_character(), vroom::col_character(), vroom::col_character(), vroom::col_character(),
                               vroom::col_character(), vroom::col_character(), vroom::col_character(),

                               vroom::col_integer(),vroom::col_integer(), vroom::col_character(), vroom::col_character(),
                               vroom::col_date(), vroom::col_integer(), vroom::col_integer(),
                               vroom::col_integer(), vroom::col_character(), vroom::col_character(), vroom::col_date())

            vroom::vroom(x,
                         delim = "|",
                         guess_max = 0,
                         col_select = select_columns,
                         col_types = column_type,
                         locale = readr::locale(encoding = "Latin1"))
        }

        ## Step 3. apply the function for each file and row bind
        purrr::map_dfr(l, read_dat)



    } else if ("Lecturas" == inf) {
        l_files <- purrr::map(list.dirs(path = path,
                                        full.names = TRUE),
                              list.files, pattern = "txt", full.names = TRUE)
        l <- unlist(purrr::map(l_files, stringr::str_subset, c(inf)))
        x <- purrr::map_dfr(l, utils::read.table,
                            sep = "\t",
                            header = TRUE,
                            stringsAsFactors = FALSE,
                            colClasses = "character",
                            fileEncoding = "UCS-2LE") %>%
            tidyr::separate(Municipio, into = c(NA, "Municipio"), extra = "merge") %>%
            tidyr::separate(Localidad, into = c(NA,"Localidad"), extra = "merge") %>%
            dplyr::mutate(Localidad = stringr::str_to_title(Localidad))
        x$Municipio <- stringr::str_trim(x$Municipio, side = "both")
        x$Huevecillos <- as.numeric(x$Huevecillos)
        x$Semana.Epidemiologica <- as.numeric(x$Semana.Epidemiologica)
        x$Sector <- as.numeric(x$Sector)
        x$Manzana <- as.numeric(x$Manzana)
        x
    } else if ("Control" == inf) {
        l_files <- purrr::map(list.dirs(path = path,
                                        full.names = TRUE),
                              list.files, pattern = "txt", full.names = TRUE)
        l <- unlist(purrr::map(l_files, stringr::str_subset, c(inf)))
        col_class <- c(rep("character", 7),
                       rep("character", 4), "integer",
                       "character", "integer", "integer", "numeric",
                       "character", "integer", "integer",
                       rep("character", 3), rep("integer", 11),
                       "numeric", rep("character", 5),
                       rep("character", 4), "character", "character")

        x <- purrr::map_dfr(l, read.table,
                            sep = "\t",
                            allowEscapes = TRUE,
                            header = TRUE,
                            skipNul = TRUE,
                            colClasses = col_class,
                            fileEncoding = "UCS-2LE") %>%
            tidyr::separate(Entidad, into = c("cve_ent", "Entidad"), extra = "merge") %>%
            tidyr::separate(Municipio, into = c("cve_mpo", "Municipio"), extra = "merge") %>%
            tidyr::separate(Localidad, into = c("cve_loc","Localidad"), extra = "merge") %>%
            tidyr::separate(Jurisdiccion, into = c(NA, "Jurisdiccion"), extra = "merge") %>%
            dplyr::mutate(Localidad = stringr::str_to_title(Localidad)) %>%
            dplyr::mutate(Municipio = stringr::str_trim(Municipio, side = "both"),
                          Jurisdiccion = stringr::str_trim(Jurisdiccion, side = "both"))
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
                            fileEncoding = "latin1",
                            colClasses = c(rep("character", 10),
                                           "integer", "integer","character",
                                           rep("integer", 3),
                                           "character", "character",
                                           rep("integer", 7),
                                           rep("character", 4), "NULL", "NULL"))
        names(x)[1] <- c("Entidad")
        x <- x %>%
            tidyr::separate(Entidad, into = c("cve_ent", "Entidad"), extra = "merge") %>%
            tidyr::separate(Municipio, into = c("cve_mpo", "Municipio"), extra = "merge") %>%
            tidyr::separate(Localidad, into = c("cve_loc","Localidad"), extra = "merge") %>%
            tidyr::separate(Jurisdiccion, into = c(NA, "Jurisdiccion"), extra = "merge") %>%
            dplyr::mutate(Localidad = stringr::str_to_title(Localidad))
        x$Municipio <- stringr::str_trim(x$Municipio, side = "both")
        x$Jurisdiccion <- stringr::str_trim(x$Jurisdiccion, side = "both")
        names(x)[31] <- "Tipo.de.Nebulizacion"
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
            tidyr::separate(Entidad, into = c("cve_ent", "Entidad"), extra = "merge") %>%
            tidyr::separate(Municipio, into = c("cve_mpo", "Municipio"), extra = "merge") %>%
            tidyr::separate(Localidad, into = c("cve_loc","Localidad"), extra = "merge") %>%
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
            tidyr::separate(Entidad, into = c("cve_ent", "Entidad"), extra = "merge") %>%
            tidyr::separate(Municipio, into = c("cve_mpo", "Municipio"), extra = "merge") %>%
            tidyr::separate(Localidad, into = c("cve_loc","Localidad"), extra = "merge") %>%
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
            tidyr::separate(Entidad, into = c("cve_ent", "Entidad"), extra = "merge") %>%
            tidyr::separate(Municipio, into = c("cve_mpo", "Municipio"), extra = "merge") %>%
            tidyr::separate(Localidad, into = c("cve_loc","Localidad"), extra = "merge") %>%
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
            tidyr::separate(Entidad, into = c("cve_ent", "Entidad"), extra = "merge") %>%
            tidyr::separate(Municipio, into = c("cve_mpo", "Municipio"), extra = "merge") %>%
            tidyr::separate(Localidad, into = c("cve_loc","Localidad"), extra = "merge") %>%
            tidyr::separate(Jurisdiccion, into = c(NA, "Jurisdiccion"), extra = "merge") %>%
            dplyr::mutate(Localidad = stringr::str_to_title(Localidad))
        x$Municipio <- stringr::str_trim(x$Municipio, side = "both")
        x$Jurisdiccion <- stringr::str_trim(x$Jurisdiccion, side = "both")
        x
    }

}
