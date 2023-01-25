#' item_stats
#'
#' This function extracts item statistics such as item-rest correlation,
#' facility, delta, and case number from 'test.its' file. This is associated
#' with test named 'test'.
#'
#' @param test Name of test.
#' @examples
#' item_stats(test='FPA')
#' @export

item_stats <- function(test){
    labs <- read.table(paste0('data/', test, '_Labels.txt')) |>
        rowid_to_column('qOrder') |>
        dplyr::rename(qid=V1)

    x <- str_file('output', test, 'its')
    ind <- grep('item:', x)
    x  <- x[ind[1]:ind[length(ind)]]

    temp <- tibble(X1=str_squish(x)) |>
        separate(X1, str_c('V', 1:8), ' ') |>
        na.omit()

    x <- temp |>
        mutate(qOrder=str_extract(V1, '\\d+')) |>
        dplyr::select(
            qOrder,
            n=V3,
            facil=V4,
            iRestCor=V5,
            iTotCor=V6,
            delta=V8
        ) |>
        mutate(
            qOrder = as.numeric(qOrder),
            n = as.integer(n)
        )

    labs |>
        right_join(x, by='qOrder') |>
        select(qOrder, everything())
}
