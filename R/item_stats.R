#' item_stats
#'
#' This function extracts item statistics such as item-rest correlation, facility, delta, and case number from 'test.its' file. This is associated with test named 'test'.
#'
#' @param folder Folder where 'test.its' file is located.
#' @param test Name of test.
#' @examples
#' item_stats(test='AHU')
#' @export

item_stats <- function(folder=here::here('output'), test){
    labs <- read.table(here::here('data', paste0(test, '_Labels.txt'))) %>%
        mutate(qOrder=as.numeric(rownames(.))) %>%
        rename(qid=V1)

    x <- readLines(here::here('output', paste0(test, '.its')))
    ind <- grep('item:', x)
    x  <- x[ind[1]:ind[length(ind)]]

    temp <- tibble(X1=str_squish(x)) %>%
        separate(X1, str_c('V', 1:8), ' ') %>%
        na.omit()

    x <- temp %>%
        mutate(qOrder=str_extract(V1, '\\d+')) %>%
        select(qOrder,
               n=V3, facil=V4,
               iRestCor=V5, iTotCor=V6,
               delta=V8) %>%
        mutate(qOrder=as.numeric(qOrder))

    labs %>%
        right_join(x, by='qOrder') %>%
        select(qOrder, everything())
}
