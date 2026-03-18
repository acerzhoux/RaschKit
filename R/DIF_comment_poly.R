#' DIF_comment_poly
#'
#' This function summarizes results from a type of DIF analysis on a polytomous 
#' DIF variable.
#'
#' @param DiF_sum Dataframe of summary of results from a Bonferroni-adjusted 
#' statistical test.
#' @return Tibble of summary of results from polytomous DIF variable analysis.
#' @examples
#' DIF_comment_poly()
#' @export

DIF_comment_poly <- function(DiF_sum){
    # add DIF mark for each item
    n <- dim(DiF_sum)[1]
    DiF_sum$mark <- NA
    for (i in 1:n){
        if (any(DiF_sum[-1][i, ] %in% c('S', 'B'))){
            DiF_sum$mark[i] <- 1
        }
    }
    DIF_n <- sum(DiF_sum$mark, na.rm = TRUE)

    # ######## COMMENTS ########

    # case 0: Neither 'B' nor 'S' exist
    if (DIF_n==0){
        return(tibble(Comments = 1,
                      Details = c('No item showed DIF.')))
    }

    DIF_labs <- DiF_sum %>% filter(!is.na(mark)) %>% pull(items) %>%
        paste0(collapse = ', ')

    # summarize DIF counts
    DIF_tbl <- lapply(DiF_sum[-1] %>% select(-mark), table) %>%
        imap(~as.data.frame(.x) %>% t() %>% as.data.frame %>%
                 select(-V1) %>%
                 janitor::row_to_names(1) %>% mutate(Category = .y)) %>%
        reduce(bind_rows) %>%
        as_tibble() %>%
        modify_if(is.character, as.numeric) %>%
        select(Category, everything())
    DIF_tbl$BS <- rowSums(DIF_tbl[-1], na.rm = TRUE)
    DIF_tbl[is.na(DIF_tbl)] <- 0

    # case 1: NO 'B'
    if (!('B' %in% names(DIF_tbl))){
        return(tibble(Comments = 1:4,
          Details = c(paste('A total of', DIF_n, 'items showed DIF.'),
          paste('Among all the categories, Category(ies)',
                paste0({DIF_tbl %>% filter(BS == max(BS)) %>% pull(Category)}, collapse=', '),
                'had most DIF on',
                unique({DIF_tbl %>% filter(BS == max(BS)) %>% pull(BS)}), 'items.'),
          paste('Category(ies)', paste0({DIF_tbl %>% filter(S == max(S)) %>% pull(Category)}, collapse=', '),
                'had most smaller-than-average DIF on',
                unique({DIF_tbl %>% filter(S == max(S)) %>% pull(S)}), 'items.'),
          paste0('Recommend to remove from the test DIF items of ', DIF_labs, '.'))))
    }

    # case 2: NO 'S'
    if (!('S' %in% names(DIF_tbl))){
        return(tibble(Comments = 1:4,
          Details = c(paste('A total of', DIF_n, 'items showed DIF.'),
          paste('Among all the categories, Category(ies)',
                paste0({DIF_tbl %>% filter(BS == max(BS)) %>% pull(Category)}, collapse=', '),
                'had most DIF on',
                unique({DIF_tbl %>% filter(BS == max(BS)) %>% pull(BS)}), 'items.'),
          paste('Category(ies)', paste0({DIF_tbl %>% filter(B == max(B)) %>% pull(Category)}, collapse=', '),
                'had most bigger-than-average DIF on',
                unique({DIF_tbl %>% filter(B == max(B)) %>% pull(B)}), 'items.'),
          paste0('Recommend to remove from the test DIF items of ', DIF_labs, '.'))))
    }

    # case 3: Both 'B' and 'S' exist
    tibble(Comments = 1:5,
       Details = c(paste('A total of', DIF_n, 'items showed DIF.'),
       paste('Among all the categories, Category(ies)',
             paste0({DIF_tbl %>% filter(BS == max(BS)) %>% pull(Category)}, collapse=', '),
             'had most DIF on',
             unique({DIF_tbl %>% filter(BS == max(BS)) %>% pull(BS)}), 'items.'),
       paste('Category(ies)', paste0({DIF_tbl %>% filter(B == max(B)) %>% pull(Category)}, collapse=', '),
             'had most bigger-than-average DIF on',
             unique({DIF_tbl %>% filter(B == max(B)) %>% pull(B)}), 'items.'),
       paste('Category(ies)', paste0({DIF_tbl %>% filter(S == max(S)) %>% pull(Category)}, collapse=', '),
             'had most smaller-than-average DIF on',
             unique({DIF_tbl %>% filter(S == max(S)) %>% pull(S)}), 'items.'),
       paste0('Recommend to remove from the test DIF items of ', DIF_labs, '.')))
}
