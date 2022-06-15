#' DIF_comment_dich
#'
#' This function generates comments for DIF analysis on dichotomous variable. This is associated with test named 'test'.
#'
#' @param DIFVar Name of DIF variable.
#' @param iDIF Vector of DIF items.
#' @examples
#' DIF_comment_dich()

DIF_comment_dich <- function(DIFVar, iDIF){
    tibble(Comment = 1,
           Details = if (length(iDIF)==0) {
               paste0('No items showed DIF on variable \'', str_to_title(DIFVar), '\'.')
           } else {
               c(paste(c(paste(paste('A total of', length(iDIF), 'items showed DIF')),
                         paste0('Please consider removing the DIF items of ',
                                paste(iDIF, collapse=', '), '.')), collapse='. '))
           })
}
