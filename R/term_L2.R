#' term_L2
#'
#' This function finds out a line number. This is associated with test named 'test'.
#'
#' @param folder Folder where .shw file is located.
#' @param test Name of test.

term_L2 <- function(folder, test) {
    file_shw(folder=folder, test=test) %>%
        str_detect('TERM 1: item') %>%
        which() %>% .[2]
}
