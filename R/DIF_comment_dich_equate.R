#' DIF_comment_dich_equate
#'
#' This function generates comments for DIF analysis on dichotomous variable. This is associated with test named 'test'.
#'
#' @param DIFVar Name of DIF variable.
#' @param iDIF Vector of DIF items.
#' @param DIF TRUE if DIF analysis is performed on a dichotomous DIF variable. Default is FALSE (anchor check).
#' @export

DIF_comment_dich_equate <- function(vars, iDIF, DIF){
    tibble(Comment = 1,
           Details = if (length(iDIF)==0) {
               paste0('No ', if (DIF) 'items' else 'anchors', ' showed DIF between ', vars[[1]], ' and ', vars[[2]], '.')
           } else {
               c(paste(c(paste(paste('A total of', length(iDIF), if (DIF) 'items' else 'anchors', 'showed DIF')),
                         paste0('Please consider removing the DIF', if (DIF) ' items' else ' anchors', ' of ',
                                paste(iDIF, collapse=', '), '.')), collapse='. '))
           })
}
