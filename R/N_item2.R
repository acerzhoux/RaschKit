#' N_item2
#'
#' This function calculates number of items in a test based on 'test.its' file.
#'  This is associated with test named 'test'.
#'
#' @param folder Folder where ConQuest output files are located.
#' @param test Name of test.
#' @examples
#' N_item2(test='FPA')
#' @export

N_item2 <- function(folder, test){
    Lines(folder, test, 'its', '=====')[[3]] -
    Lines(folder, test, 'its', '-----') - 1
}
