#' skip_its
#'
#' This function finds out the number of lines to skip when reading 'test.its' 
#' output file from ConQuest.

#' @param file File name to read.
#' @return Number of lines to skip.
#' @examples
#' skip_its()
#' @export

skip_its <- function(file){
    readLines(file) %>%
        str_detect('-----') %>%
        which()
}
