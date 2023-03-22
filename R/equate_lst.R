#' equate_lst
#'
#' This function returns a list of functions to add format such as header and
#' body style to Excel sheet.
#'
#' @param statsLst List of tests' anchor statistics dataframe of variables 'item',
#' 'delta.x', 'error.x', 'delta.y', 'error.y'. The names of the elements are
#' test names.
#' @param vars Vector of length 2 such as c('2022','2023'). Its order corresponds
#' to two tests associated with .x and .y.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between
#' two tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference
#' between two tests. Default is 4.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @param step TRUE if DIF analysis is performed on step parameters.
#' Default is FALSE.
#' @param iterative TRUE to iteratively remove DIF items. Default is TRUE.
#' @return List of chi-square test results on Form A and Form B for each grade.
#' @export

equate_lst <- function(statsLst, vars, p_cut=0.05, DIF_cut=0.5, DIF_adj_cut=4,
                       desig_effect=1, step=FALSE, iterative=TRUE){
  tests <- names(statsLst)
  for (i in seq_along(tests)){
    Equate(statsLst[[i]], tests[[i]], vars, p_cut, DIF_cut, DIF_adj_cut, TRUE,
           desig_effect, step, FALSE, iterative)
  }

  read2one('equating', tests)

}
