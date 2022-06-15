#' item_stats
#'
#' This function extracts item statistics such as item-rest correlation, facility, delta, and case number from 'test.its' file. This is associated with test named 'test'.
#'
#' @param folder Folder where 'test.its' file is located.
#' @param test Name of test.
#' @examples
#' item_stats(test='elana_poly_score')

item_stats <- function(folder=here::here('output'), test){
    labs <- read.table(here::here('data', paste0(test, '_Labels.txt'))) %>%
        mutate(qOrder=as.numeric(rownames(.))) %>%
        rename(qid=V1)

    its_path <- file_path_type(folder=folder, test=test, type='its')
    its_str <- its_path %>% readLines()
    its_skip <- its_str %>% str_detect('-----') %>% which()
    n_item <- which(str_detect(its_str, '====='))[[3]] - its_skip - 1

    read_fwf(its_path,
             fwf_cols(item=c(6, 26),
                      n=c(27, 31),
                      facil=c(31, 41),
                      iRestCor=c(44, 58),
                      iTotCor=c(59, 71),
                      delta=c(82, 92)),
             skip=its_skip,
             n_max=n_item,
             show_col_types = FALSE) %>%
        separate(item, into=c('qOrder', 'qid'), sep=' ') %>%
        select(-qid) %>%
        mutate(qOrder=as.numeric(qOrder)) %>%
        left_join(labs, by='qOrder')
}
