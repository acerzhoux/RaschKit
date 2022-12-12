#' file_path_type
#'
#' This function generates file path to a specified type. This is associated with test named 'test'.
#'
#' @param folder Folder where .shw file is located with delta estimates for interaction term between dichotomous DIF variable and item.
#' @param test Name of test.
#' @param type Type of output file from ConQuest such as 'txt', 'its', 'shw', and 'itn'
#' @return Path of the file.
#' @examples
#' file_path_type(folder=folder, test=test, type)
#' @export

file_path_type <- function(folder, test, type){
    list.files(folder, full.names=TRUE) %>%
        str_subset(paste0(test, '.', type))
}
