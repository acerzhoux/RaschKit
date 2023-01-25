#' file_its
#'
#' This function extracts item analysis statistics from CQ output file 'test.its'.
#' This is associated with test named 'test'.

#' @param folder Folder where ConQuest output files are located.
#' @param test Name of test.
#' @param DIFVar Name of variable to perform DIF analysis on.
#' @return Dataframe of delta, facility, etc..
#' @examples
#' # Not run
#' # file_its(test=test)
#' @export

file_its <- function(folder='output', test, DIFVar=NULL){
    if (!is.null(DIFVar)) folder <- paste0('DIF/', DIFVar)
    n_item <- N_item(folder, test)
    if (!is.null(DIFVar)) n_item <- n_item*2
    file <- Path(folder, test, 'its')

    read_fwf(
        file,
        fwf_empty(file),
        skip = Lines(folder, test, 'its', '-----'),
        n_max = n_item,
        show_col_types = FALSE
    )
}
