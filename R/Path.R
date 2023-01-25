#' Path
#'
#' This function generates file path to a specified type. This is associated
#' with test named 'test'.
#'
#' @param folder Place of output files from ConQuest.
#' @param test Name of test.
#' @param type Type of output file from ConQuest such as 'txt', 'its', 'shw',
#' and 'itn'.
#' @return Path of the file.
#' @examples
#' file_path_type(test='FPA', type='its')
#' @export

Path <- function(folder, test, type){
    paste0(folder, '/', test, '_', type, '.txt')
}
