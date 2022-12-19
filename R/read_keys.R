#' read_keys
#'
#' This function read from 'data' folder the key file associated with 'test'.
#'
#' They key file has one row for all-multiple-choice-item test, or multiple 
#' rows if any item has double key or polytomous scoring.
#'
#' @param folder The 'data' folder where key 'test_Key.txt' file is located.
#' @param test Name of the test.
#' @export

read_keys <- function(folder, test){
    read_tsv(file.path(folder, paste0(test, '_Key.txt')),
             col_names = FALSE, col_types = 'c') %>% .[[1]]
}
