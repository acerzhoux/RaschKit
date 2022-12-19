#' qid_its
#'
#' This function generates vector of item orders that appeared in .its file. 
#' This is associated with test named 'test'.
#'
#' @param folder Folder that contains ConQuest output files associated with 'test'.
#' @param test Name of test.
#' @return Vector of item orders.
#' @examples
#' qid_its()
#' @export

qid_its <- function(folder, test){
    n_item <- N_item2(folder=folder, test=test)
    file <- file_path_type(folder=folder, test=test, type='its')

    strs <- read_fwf(file,
             fwf_empty(file),
             skip=skip_its(file=file),
             n_max=n_item,
             show_col_types = FALSE)
    str_sub(strs$X1, 6, 8) %>%
        parse_number()
}
