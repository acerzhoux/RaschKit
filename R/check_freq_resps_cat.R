#' check_freq_resps_cat
#'
#' This function compares frequencies of item categories from CQ .txt output file against those calculated with Function 'freq_resps_cat'. This is associated with test named 'test'. An Excel file with comparison results will be save in the 'folder' set in that argument.
#'
#' @param test Name of the test.
#' @param folder Place to find CQ output files.
#' @param resp Responses to items in the test.
#' @return Dataframe with information such as counts in CQ .txt file, counts from R function, and their difference.
#' @examples
#' check_freq_resps_cat()
#' @export

check_freq_resps_cat <- function(folder, test, resp){
    opt_cq <- opt_stats(folder=folder, test=test)
    opt_R <- freq_resps_cat(resp=resp)
    checked <- opt_cq %>% select(qOrder, resp, count) %>%
        modify_at('qOrder', as.numeric) %>%
        modify_at('resp', as.character)%>%
        full_join(opt_R, by=c('qOrder', 'resp'='Cat')) %>%
        filter(!is.na(Item)) %>%
        select(qOrder, Item, Category=resp, cqFreq=count, rFreq=Freq) %>%
        mutate(Dif=cqFreq-rFreq) %>%
        filter(!(Category %in% c('r', '')))

    checked %>%
        writexl::write_xlsx(file.path(folder, paste0(test, '_', 'Frequency_check.xlsx')))
    checked
}

