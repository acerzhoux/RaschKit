#' N_item
#'
#' This function finds out number of items in a test. This is associated with test named 'test'.
#'
#' @param folder Folder where .shw file is located.
#' @param test Name of test.
#' @examples
#' N_item()
#' @export

N_item <- function(folder, test){
    {file_shw(folder=folder, test=test) %>%
            str_detect('An asterisk') %>%
            which() %>% .[1]} -
        term_L2(folder=folder, test=test) - 7
}
