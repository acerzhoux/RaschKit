#' N_item_Step
#'
#' This function extracts line numbers from _shw.txt file and calculates number
#' of item steps. This is associated with test named 'test'.
#'
#' @param folder Folder where xxx_shw.txt file is located.
#' @param test Name of test.
#' @examples
#' N_item_Step()
#' @export

N_item_Step <- function(folder, test){
    Lines(folder, test, 'shw', 'An asterisk')[[2]] -
    Lines(folder, test, 'shw', 'TERM 2: item\\*step')[[2]] - 7
}
