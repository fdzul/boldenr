#' read_vbd
#'
#' This function read the vector-borne diseases txt file of sinave.
#'
#' @param x is the directory where the files are located.
#' @param vbd is the parameter for define the vector-borne diseases.
#' @param year is the year dataset.
#' @param arbovirus is a logical value for define the group of vector-borne diseases, if is TRUE, the define DENV, CHIKV & ZIKV, else for other etvs.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @return a dataframe
#' @export
#' @import utils
#' @importFrom stringr str_subset
#' @importFrom data.table fread
#' @importFrom purrr map_dfr
#'
#' @examples 1+1
read_vbd <- function(x, vbd, year, arbovirus){

    # Step 1 create the list directories ####
    l_files <- list.files(x,
                          pattern = "txt",
                          full.names = TRUE)

    # Step 2 extract only the txt of dengue ####
    l <- unlist(purrr::map(l_files, stringr::str_subset, c(vbd)))


    # Step 3. read the dataset ####

    if(arbovirus == TRUE){
        read_arbo <- function(x){
            vect_cols <- c("VEC_ID","IDE_EDA_ANO", "IDE_SEX",
                           "DES_CAL","IDE_CAL", "NUM_EXT", "NUM_INT",
                           "IDE_COL", "IDE_CP",
                           "CVE_LOC_RES", "DES_LOC_RES", "CVE_MPO_RES", "DES_MPO_RES",
                           "DES_JUR_RES", "CVE_EDO_RES", "DES_EDO_RES",
                           "ESTATUS_CASO", "CVE_DIAG_PROBABLE", "DES_DIAG_PROBABLE", "DES_DIAG_FINAL",
                           "FEC_INI_SIGNOS_SINT", "ANO", "SEM",
                           "MANEJO",
                           "DES_INS_UNIDAD",
                           "DENGUE_SER_TRIPLEX",
                           "FEC_INGRESO")
            data.table::fread(x,
                              header = TRUE,
                              quote = "",
                              select = vect_cols,
                              #encoding = "Latin-1
                              fill = TRUE)
        }
        purrr::map_dfr(l, read_arbo)
    } else {
        data.table::fread(l,
                          header = TRUE,
                          quote = "",
                          #encoding = "Latin-1
                          fill = TRUE)
    }
}
