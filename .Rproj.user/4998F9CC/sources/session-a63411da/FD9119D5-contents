#' DIF_comment_dich_equate
#'
#' This function generates comments for DIF analysis on dichotomous variable.
#' This is associated with test named 'test'.
#'
#' @param vars Vector of length 2 such as c('girls','boys'). Its order
#' corresponds to the alphabetic/numeric order of DIF variables' two categories in data.
#' @param iDIF Vector of DIF items.
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'.
#' @export

DIF_comment_dich_equate <- function(vars, iDIF, DIFVar){
  opt <- ifelse(!is.null(DIFVar), 'items', 'anchors')
  tibble(
    Comment = 1,
    Description =
      if (length(iDIF)==0) {
        paste0('No', opt, 'showed DIF between ', vars[[1]], ' and ', vars[[2]], '.')
      } else {
        c(paste(c(paste(paste('A total of', length(iDIF), opt, 'showed DIF')),
          paste0('Consider removing ', opt, ' ',
          paste(iDIF, collapse=', '), '.')), collapse='. '))
      }
  )
}
