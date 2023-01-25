#' itn_poly_comment
#'
#' This function checks whether polytomous items' neighbor categories' ability
#' estimates are too small. This is associated with test named 'test'.
#'
#' @param test Name of test.
#' @examples
#' itn_poly_comment('Numeracy_item')
#' @export

itn_poly_comment <- function(test){
    itn_items <- itn_ls(test=test)
    iCat <- iCat_new <- map(itn_items, 'Label')

    # check ability estimate difference of neighbor categories
    n_cat <- map(itn_items, nrow)
    thrshold <- map2(itn_items, n_cat, ~(.x$PV1[.y]-.x$PV1[1])/(.y-1)) %>%
        map(~.-(.)/3)
    diff_ls <- itn_items %>%
        map(~diff(.$PV1)) %>%
        map2(thrshold, ~which(.x < min(.y, .3))) # may change magnitude

    # items to collapse
    i_max_score <- map_dbl(itn_items, ~.['Score'] %>% max())
    iColps <- intersect(which(map_lgl(diff_ls, ~!identical(., integer(0)))),
        which(i_max_score > 1))
    for (i in iColps){
        ind_col <- diff_ls[[i]]
        cat_end <- length(iCat_new[[i]])
        for (j in ind_col){
            iCat_new[[i]][(j+1):cat_end] <- iCat_new[[i]][(j+1):cat_end] - 1
        }
    }

    # tibble of item and comment
    tibble(qOrder=iColps,
           Comment=paste('May collapse', map(iCat[iColps], ~paste0(., collapse='')), 'to',
           map(iCat_new[iColps], ~paste0(., collapse=''))))
}
