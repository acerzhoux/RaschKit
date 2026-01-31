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
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' @export

create_lab <- function(test, run){
    write_delim(
        read.table(paste0('data/', run, '/', test, '_Labels.txt')) |>
           rowid_to_column() |>
           `colnames<-`(c('===>', 'item')),
        paste0('Input/', run, '/', test, '_lab.txt'),
        delim = ' ',
        col_names = TRUE
    )
}
