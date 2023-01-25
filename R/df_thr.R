#' df_thr
#'
#' This function extracts step parameter estimates from xxx_thr.txt file. This is
#' associated with test named 'test'. If any item label is longer than 16
#' characters' fixed width, use long_label=TRUE to read complete labels
#' from 'test_Labels.txt' file in 'data' folder.
#'
#' @param folder Folder where ConQuest output files are located.
#' @param test Name of test.
#' @param long_label Whether item labels are longer than 16 characters' fixed
#' width. Default is FALSE.
#' @return Dataframe of step parameter estimates.
#' @examples
#' df_thr(test='FPA')
#' df_thr(test='FPA', long_label=TRUE)
#' @export

df_thr <- function(folder='output', test, long_label=FALSE){
    items_thr <- str_file(folder, test, 'thr') |>
        as_tibble() |>
        mutate(tabbed = str_detect(value, '\\t')) |>
        dplyr::filter(tabbed) |>
        dplyr::select(-tabbed) |>
        separate(
            value,
            into = c("iNum", "threshold", "temp1", "temp2", "iNum2", "iLab", "temp3"),
            sep = "\\t"
        ) |>
        dplyr::select(-contains("temp"), -iNum2) |>
        separate(
            iNum,
            into = c("iNum", "category"),
            sep = "\\."
        ) |>
        mutate_at(c("iNum", "category", "threshold"), as.numeric)

    if (long_label){
        labs <- read.table(paste0('data/', test, '_Labels.txt')) |>
            rowid_to_column('iNum') |>
            dplyr::rename(Label = V1)
        items_thr |>
            left_join(labs, by='iNum') |>
            select(-iLab, iLab=Label)
    } else {
        items_thr
    }
}
