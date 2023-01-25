#' qid_its
#'
#' This function generates vector of item orders that appeared in _its.txt file.
#' This is associated with test named 'test'.
#'
#' @param folder Folder where ConQuest output files are located.
#' @param test Name of test.
#' @return Vector of item orders.
#' @examples
#' qid_its('output', 'FPA')
#' @export

qid_its <- function(folder, test){
    file <- Path(folder, test, 'its')
    strs <- file_its(folder, test, DIFVar=NULL)
    parse_number(str_sub(strs$X1, 6, 8))
}
