#' str_file
#'
#' This function reads into string a file of 'type' and 'test'.
#' This is associated with test named 'test'.
#'
#' @param folder Folder where ConQuest output files are located.
#' @param test Name of test.
#' @param type File type.
#' @return Vector of item orders.
#' @examples
#' str_file('FPA', 'shw')
#' @export

str_file <- function(folder, test, type){
    readLines(Path(folder, test, type))
}
