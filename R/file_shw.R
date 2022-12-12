#' file_shw
#'
#' This function reads .shw file. This is associated with test named 'test'.

#' @param folder Folder where .shw file is located.
#' @param test Name of test.
#' @return String of lines in 'test.shw' file.
#' @examples
#' file_shw()
#' @export

file_shw <- function(folder, test){
    list.files(folder, full.names=TRUE) %>%
        str_subset(paste0(test, '.shw')) %>%
        readLines()
}
