#' N_item2
#'
#' This function calculates number of items in a test based on 'test.its' file. This is associated with test named 'test'.
#'
#' @param folder Folder where 'test.its' file is located. Default is 'output' folder.
#' @param test Name of test.
#' @examples
#' N_item2(test='elana_poly_score')
#' @export

N_item2 <- function(folder=here::here('output'), test){
    its_str <- list.files(folder, full.names=TRUE) %>%
        str_subset(paste0(test, '.its')) %>%
        readLines()
    its_skip <- its_str %>%
        str_detect('-----') %>% which()
    which(str_detect(its_str, '====='))[[3]] - its_skip - 1
}
