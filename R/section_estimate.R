#' section_estimate
#'
#' This function creates string of estimation specification for 'test.cqc' file in 'input' folder. This is associated with test named 'test'.
#'
#' @param quick TRUE if quick estimation is preferred.
#' @param poly_key TRUE if the key of any item has polytomous scoring. Default is FALSE.
#' @return String of characters used in estimation section of 'test.cqc' file in 'input' folder.
#' @examples
#' section_estimate()
#' @export

section_estimate <- function(quick, poly_key=FALSE){
    paste0('estimate ! convergence=0.0001, iter=7500, stderr=',
           if (quick) 'quick, ' else 'empirical, ',
           'fit=yes, deviancechange=0.000000001, matrixout=e, nodes=',
           if (poly_key) 50 else 15, ';\n')
}

