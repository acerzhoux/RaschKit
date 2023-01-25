#' Lines
#'
#' This function detects lines where 'str' appears in file of 'type' and 'test'.
#'
#' @param folder Place of output files from ConQuest. Default is NULL where
#' 'output' folder is used.
#' @param test Name of test.
#' @export

Lines <- function(folder, test, type, str) {
    which(str_detect(readLines(Path(folder, test, type)), str))
}
