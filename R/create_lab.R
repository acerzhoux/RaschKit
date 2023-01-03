#' create_lab
#'
#' This function reads in 'test_Labels.txt' from 'data' folder and creates 
#' 'test.lab' file in 'input' folder. Both procedures are associated with 
#' test named 'test'.
#'
#' They key file has one row for all-multiple-choice-item test, or multiple 
#' rows if any item has double key or polytomous scoring.
#'
#' @param wd Working directory. Default is the folder where .Rproj is located.
#' @param test Name of the test.
#' @export

create_lab <- function(wd, test){
    labs <- read_tsv(file.path(file.path(wd, 'data'), paste0(test, '_Labels.txt')),
                     col_names = FALSE,
                     show_col_types = FALSE) %>%
        rowid_to_column() %>%
        `colnames<-`(c('===>', 'item'))
    labs_path <- file.path(wd, 'Input', paste0(test, '.lab'))
    
    write_delim(labs, labs_path, delim = ' ', col_names = TRUE)
}
