#' poly_recode_one
#'
#' This function down codes a non-continuous polytomous item response to be continuous and starting from 0. It will keep intact many possible missing codes including c('r','R','m','M','9','x','X','.','',' ',NA).
#'
#' @param x0 A vector of non-continuous polytomous item response.
#' @param miss_code Missing codes. Default is c('r','R','m','M','9','x','X','.','',' ',NA).
#' @examples
#' poly_recode_one(c(0,10,2,2,4,6,10))
#' poly_recode_one(c(0,1,2,3))
#' poly_recode_one(c(0,'.',10,'r','R',2,2,'M','9','x',4,6,10,'m','X',' '))
#' poly_recode_one(c(0,10,7,8,2,2,'M',9,4,6,10,'r'), miss_code=c(7,8,9,'M','r'))
#' @export

poly_recode_one <- function(x0, miss_code=c('r','R','m','M','9','x','X','.','',' ',NA)) {
    x <- setdiff(x0, miss_code) %>%
        as.numeric()

    if (all(x == (rank(x) - 1))){
        x0
    } else {
        cat(paste(paste(x, collapse='_'), 'To', paste((rank(x) - 1), collapse='_'), '\n'))

        # return processed continuous vector
        x_same <- setdiff(x0, x)
        tbl_check <- tibble(origin = x, new = rank(x) - 1) %>%
            rbind(tibble(origin = x_same, new = x_same))
        tibble(origin = x0) %>%
            left_join(tbl_check, by = 'origin') %>%
            pull(new)
    }
}
