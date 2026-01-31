#' N_item2
#'
#' This function calculates number of items in a test based on 'test.its' file.
#'  This is associated with test named 'test'.
#'
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' @param test Name of test.
#' @examples
#' N_item2(run, test='AACA')
#' @export

N_item2 <- function(run, test){
    # Lines(folder, test, 'its', '=====')[[3]] -
    # Lines(folder, test, 'its', '-----') - 1

    nrow(read.table(paste0('data/', run, '/', test, '_Labels.txt')))
}
