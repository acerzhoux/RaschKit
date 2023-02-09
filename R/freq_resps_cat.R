#' freq_resps_cat
#'
#' This function . This is associated with test named 'test'.
#'
#' @param resp Dataframe of responses where each column is a vector of responses
#' to one test item.
#' @param wide TRUE if wide form is desired. Default is FALSE. When default,
#' the output has long format same as frequency table in .txt file output
#' from ConQuest.
#' @param prop TRUE if proportion is desired. Default is FALSE.
#' @return Table of each item's category frequencies.
#' @examples
#' freq_resps_cat(resp, TRUE, TRUE)
#' @export

freq_resps_cat <- function(resp=math3Recode, wide=FALSE, prop=FALSE){
    item_lookup <- tibble(Item = names(resp), qOrder = 1:ncol(resp))
    frequencies <- map(resp, ~table(.x, useNA = 'always')) |>
        map(~as.data.frame(.)) |>
        imap(~mutate(.x, Item = .y)) |>
        reduce(bind_rows) |>
        dplyr::rename(Cat = .x) |>
        dplyr::select(Item, everything()) |>
        left_join(item_lookup, by = "Item")

    if (wide) {
        frequencies <- frequencies |>
            pivot_wider(names_from = "Cat", values_from = "Freq")
        cats <- setdiff(names(frequencies), c('Item','qOrder'))
        # order options in columns
        if (any(c('A','B','C','D') %in% cats)) {
            cats <- sort(cats)
        } else {
            idNA <- which(is.na(as.numeric(cats)))
            if (identical(idNA, integer(0))){
                cats <- as.character(sort(as.numeric(cats)))
            } else {
                cats <- c(sort(as.numeric(cats[setdiff(1:length(cats), idNA)])), cats[idNA])
            }
        }
        frequencies <- frequencies |>
            dplyr::select(qOrder, Item, all_of(cats))
        N <- rowSums(frequencies[-c(1,2)], na.rm=TRUE)
        frequencies <- cbind(frequencies, N)
        if (prop){
            frequencies <- frequencies |>
                mutate(across(-c("qOrder", "Item", 'N'), ~round(.x/N*100, 2)))
        }
    }
    frequencies
}
