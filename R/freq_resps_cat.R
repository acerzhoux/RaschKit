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
#' a=freq_resps_cat(resp=df_DIF[-c(1:5)], wide=TRUE)
#' @export

freq_resps_cat <- function(resp, wide=FALSE, prop=FALSE){
    item_lookup <- tibble(Item = names(resp), qOrder = 1:ncol(resp))
    frequencies <- lapply(resp, table) %>% 
        map(~as.data.frame(.)) %>% 
        imap(~mutate(.x, Item = .y)) %>% 
        reduce(bind_rows) %>% 
        dplyr::rename(Cat = Var1) %>% 
        dplyr::select(Item, everything()) %>% 
        left_join(item_lookup, by = "Item")

    if (wide) {
        frequencies <- frequencies %>% 
            pivot_wider(names_from = "Cat", values_from = "Freq")
        cats <- setdiff(names(frequencies), c('Item','qOrder')) %>% 
            as.numeric() %>% 
            sort() %>% 
            as.character()        
        frequencies <- frequencies %>% 
            dplyr::select(qOrder, Item, all_of(cats)) %>% 
            mutate(N = rowSums(.[-c(1,2)], na.rm=TRUE))
        if (prop){
            frequencies <- frequencies %>% 
                mutate(across(-c("qOrder", "Item", 'N'), ~round(.x/N*100, 2)))
        }
    }
    frequencies
}
