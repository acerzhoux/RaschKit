#' term_L2_Step
#'
#' This function a line number close to interaction term estimates of 
#' 'item*step' in .shw file. This is associated with test named 'test'.
#'
#' @param folder Folder where .shw file is located.
#' @param test Name of test.
#' @export

term_L2_Step <- function(folder, test) {
    file_shw(folder=folder, test=test) %>%
        str_detect('TERM 2: item\\*step') %>%
        which() %>% .[2]
}
