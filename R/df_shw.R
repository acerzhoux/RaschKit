#' df_shw
#'
#' This function extracts item delta and error estimates from _shw.txt file. This
#' is associated with test named 'test'. If any item label is longer than 15
#' characters' fixed width, use long_label=TRUE to read complete labels from
#' 'test_Labels.txt' file in 'data' folder.
#'
#' @param folder Folder where ConQuest output files are located.
#' @param test Name of test.
#' @param long_label Whether item labels are longer than 15 characters' fixed
#' width. Default is FALSE.
#' @return Dataframe of item name, delta, and error.
#' @examples
#' df_shw(test, TRUE)
#' df_shw(test)
#' @export

df_shw <- function(folder, test, long_label=FALSE){
    items_shw <- Path(folder, test, 'shw') |>
        read_fwf(
            fwf_cols(
                iNum = c(2, 5),
                item = c(6, 20),
                delta = c(21, 27),
                error = c(31, 35)
            ),
            skip = Lines(folder, test, 'shw', 'TERM 1: item')[2] + 5,
            n_max = N_item(folder, test),
            show_col_types = FALSE
        )

    if (long_label){
        labs <- read.table(paste0('data/', test, '_Labels.txt')) |>
            rowid_to_column('iNum') |>
            dplyr::rename(Label=V1)
        items_shw |>
            left_join(labs, by='iNum') |>
            dplyr::select(-iNum, -item) |>
            dplyr::select(item=Label, everything())
    } else {
        items_shw |>
            dplyr::select(-iNum)
    }
}
