#' create_lab
#'
#' This function reads in 'test_Labels.txt' from 'data' folder and creates
#' 'test.lab' file in 'input' folder. Both procedures are associated with
#' test named 'test'.
#'
#' They key file has one row for all-multiple-choice-item test, or multiple
#' rows if any item has double key or polytomous scoring.
#'
#' @param test Name of the test.
#' @export

create_lab <- function(test){
    write_delim(
        read.table(paste0('data/', test, '_Labels.txt')) |>
           rowid_to_column() |>
           `colnames<-`(c('===>', 'item')),
        paste0('Input/', test, '_lab.txt'),
        delim = ' ',
        col_names = TRUE
    )
}
