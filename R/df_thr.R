#' df_thr
#'
#' This function extracts step parameter estimates from .thr file. This is 
#' associated with test named 'test'.If any item label is longer than 16 
#' characters' fixed width, use long_label=TRUE to read complete labels 
#' from 'test_Labels.txt' file in 'data' folder.
#'
#' @param folder Folder where .thr file is located.
#' @param test Name of test.
#' @param long_label Whether item labels are longer than 16 characters' fixed 
#' width. Default is FALSE.
#' @return Dataframe of step parameter estimates.
#' @examples
#' df_thr(test='elana_math')
#' df_thr(test='elana_math', long_label=TRUE)
#' @export

df_thr <- function(folder=here::here('output'), test, long_label=FALSE){
    file_thr <- list.files(folder, full.names=TRUE) %>%
        str_subset(paste0(test, '.thr'))
    items_thr <- readLines(file_thr) %>%
        as_tibble() %>%
        mutate(tabbed=str_detect(value, '\\t')) %>%
        filter(tabbed) %>%
        select(-tabbed) %>%
        separate(value,
                 into = c("iNum", "threshold", "temp1", "temp2", "iNum2", "iLab", "temp3"),
                 sep = "\\t") %>%
        select(-contains("temp"), -iNum2) %>%
        separate(iNum, into = c("iNum", "category"), sep = "\\.") %>%
        mutate_at(c("iNum", "category", "threshold"), as.numeric)

    if (long_label){
        labs <- read.table(here::here('data', paste0(test, '_Labels.txt'))) %>%
            mutate(iNum=as.numeric(rownames(.))) %>%
            rename(Label=V1)
        items_thr %>%
            left_join(labs, by='iNum') %>%
            select(-iLab, iLab=Label)
    } else {
        items_thr
    }
}
