#' section_estimate
#'
#' This function creates string of estimation specification for 'test.cqc' file in 'input' folder. This is associated with test named 'test'.
#'
#' @param quick TRUE if quick estimation is preferred.
#' @return String of characters used in estimation section of 'test.cqc' file in 'input' folder.
#' @examples
#' section_estimate()

section_estimate <- function(quick){
    paste0('estimate ! convergence=0.0001, iter=7500, stderr=',
           if (quick) 'quick, ' else 'empirical, ',
           'fit=yes, deviancechange=0.000000001, matrixout=e, nodes=30;\n')
}

