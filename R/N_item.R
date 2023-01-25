#' N_item
#'
#' This function finds out number of items in a test. This is associated
#' with test named 'test'.
#'
#' @param folder Place of output files from ConQuest. Default is NULL where
#' 'output' folder is used.
#' @param test Name of test.
#' @export

N_item <- function(folder, test){
    Lines(folder, test, 'shw', 'An asterisk')[[1]] -
    Lines(folder, test, 'shw', 'TERM 1: item')[[2]] - 7
}
