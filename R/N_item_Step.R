#' N_item_Step
#'
#' This function extracts line numbers from .shw file and calculates number of item steps. This is associated with test named 'test'.
#'
#' @param folder Folder where .shw file is located.
#' @param test Name of test.
#' @examples
#' N_item_Step()
#' @export

N_item_Step <- function(folder, test){
    {file_shw(folder=folder, test=test) %>%
            str_detect('An asterisk') %>%
            which() %>% .[2]} -
        term_L2_Step(folder=folder, test=test) - 7
}
