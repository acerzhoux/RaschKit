#' section_model
#'
#' This function creates string of model specification for 'test.cqc' file
#' in 'input' folder. This is associated with test named 'test'.
#'
#' @param regr_ls List of regressors with reference group excluded,
#' e.g., list('G4'='110', 'G5'='111', 'G6'='112').
#' @param codes Vector of valid codes for item responses, e.g., c(1, 2, 3, 4, 5, 6, 7, 8, 9).
#' @param poly_key TRUE if any item has polytomous scoring.
#' @param DIFVar Name of DIF variable.
#' @param step TRUE if DIF analysis is performed on step parameters.
#' Default is FALSE.
#' @param poly_group TRUE if model is run per group and no DIF variable is used.
#' @return String of characters used in model section of 'test.cqc' file in
#' 'input' folder.
#' @examples
#' section_model()
#' @export

section_model <- function(regr_ls, codes, poly_key,
              DIFVar, step, poly_group){
  c(
    if (!is.null(regr_ls)) {
      str_c('Regression ', paste0(names(regr_ls), collapse=' '), ';\n')
    },
    str_c('codes ', paste0(codes, collapse=','), ';\n'),

    if (poly_key){
      if (step){
        paste0(
          'model item',
          if (!is.null(DIFVar) & !poly_group) {
            paste0('-', DIFVar, '+item*', DIFVar)
          },
          '+item*step*', DIFVar, ';\n'
        )
      } else {
        paste0(
          'model item',
          if (!is.null(DIFVar) & !poly_group) {
             paste0('-', DIFVar, '+item*', DIFVar)
          },
          '+item*step;\n'
        )
      }
    } else {
      paste0(
        'model item',
        if (!is.null(DIFVar) & !poly_group) {
          paste0('-', DIFVar, '+item*', DIFVar)
        },
        ';\n'
      )
    }
  )
}
