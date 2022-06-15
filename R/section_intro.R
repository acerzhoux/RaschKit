#' section_intro
#'
#' This function creates string of introduction specification for 'test.cqc' file in 'input' folder. This is associated with test named 'test'.
#'
#' @param test Name of test.
#' @param run_ls List of data filters. Element is column number in data. Element name is filter variable's name.
#' @param path_output Route of folder to put export files.
#' @param DIFVar Name of DIF variable.
#' @param poly_catgrs Vector of polytomous DIF variable's categories.
#' @return String of characters used in introduction section of 'test.cqc' file in 'input' folder.
#' @examples
#' section_intro()

section_intro <- function(test, run_ls, path_output, DIFVar, poly_catgrs){
    c(if (!is.null(poly_catgrs)) {c(paste('dofor', DIFVar, '=', paste0(poly_catgrs, collapse=','), ";\n"),
                                    paste('let test =', test, ";\n"),
                                    paste('let DIFVar =', DIFVar, ";\n"))},
      if (is.null(poly_catgrs)) paste0('let name = ', test, ';\n'),
      paste0('let path = ', path_output, ';\n'),
      if (is.null(poly_catgrs)) 'execute;\n',
      if (!is.null(poly_catgrs)) paste("title", '%DIFVar% - %test% DIF', ";\n") else
          paste('title', 'MCQ Item Calibration', test, ';'))
}
