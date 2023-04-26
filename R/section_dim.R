#' section_dim
#'
#' This function generates the dimension and relevant label sessions of
#' the .cqc file when the test has more than one dimension.
#'
#' @param scrs Vector of possible scores in the test.
#' @param nDimVec Vector of numbers of responses the dimensions have.
#' @param dimNmVec Vector of the dimensions' names.
#' @examples
#' section_dim(codes=c(1, 2, 3, 4), nDimVec=c(30, 45),
#' dimNmVec=c('Literacy', 'Numeracy'))
#' @export

section_dim <- function(scrs, nDimVec, dimNmVec){
    scrs_par <- paste0('(', paste0(scrs, collapse=','), ')')
    N <- length(nDimVec)
    # dimension lines
    dim_ls <- list()
    strt <- 0
    for (i in 1:N){
        dim_ls[[i]] <- paste0('score ', scrs_par, ' ',
            paste0(rep('( )', i-1), collapse=' '), ' ',
            scrs_par, ' ', paste0(rep('( )', N-i), collapse=' '),
            ' ! items(', strt+1, '-', strt+nDimVec[[i]], ');\n')
        strt <- strt+nDimVec[[i]]
    }
    # label lines
    lab_ls <- list()
    for (i in 1:N){
        lab_ls[[i]] <- paste0('labels ', i, ' \"', dimNmVec[[i]], '\" ! dimension;\n')
    }
    c(reduce(dim_ls, c), '\n', reduce(lab_ls, c))
}
