#' delta_error_DIF_dich
#'
#' This function extracts error statistics for two categories of dichotomous
#' DIF variable.
#'
#' @param folder Folder where xxx_shw.txt file is located with delta estimates for
#' interaction term between dichotomous DIF variable and item. Default is 'DIF' folder.
#' @param test Name of test.
#' @param long_label Whether item labels are longer than 15 characters' fixed
#' width. Default is FALSE.
#' @return Dataframe of three variables of 'item', 'error.x', 'error.y'.
#' @examples
#' delta_error_DIF_dich(folder=folder, test='RandD2')
#' delta_error_DIF_dich(folder=folder, test='RandD2', long_label=TRUE)
#' @export

delta_error_DIF_dich <- function(folder, test, long_label=FALSE){
    items_shw <- read_fwf(
        Path(folder, test, 'shw'),
        fwf_cols(
            iNum=c(2, 5),
            item=c(6, 16),
            var=c(17, 19),
            error=c(46, 50)
        ),
        skip = (Lines(folder, test, 'shw', 'TERM 3: item*')[[2]] + 5),
        n_max = N_item(folder, test) * 2,
        show_col_types = FALSE
        ) |>
        pivot_wider(
            names_from=var,
            values_from=error
        ) |>
        `colnames<-`(c('iNum', 'item', 'error.x', 'error.y'))

    if (long_label){
        labs <- read.table(paste0('data/', test, '_Labels.txt')) |>
            rowid_to_column('iNum') |>
            dplyr::rename(Label=V1)
        items_shw |>
            left_join(labs, by='iNum') |>
            dplyr::select(-iNum, -item, item=Label)
    } else {
        items_shw |>
            dplyr::select(-iNum)
    }
}
