#' df_del
#'
#' This function extracts step parameter estimates from .del file. This is associated with test named 'test'. If any item label is longer than 16 characters' fixed width, use long_label=TRUE to read complete labels from 'test_Labels.txt' file in 'data' folder.
#'
#' @param folder Folder where .del file is located.
#' @param test Name of test.
#' @return Dataframe of item name_step and delta.
#' @param long_label Whether item labels are longer than 16 characters' fixed width. Default is FALSE.
#' @examples
#' df_del(test='elana_math_NSW')
#' df_del(test='elana_math_NSW', long_label=TRUE)

df_del <- function(folder=here::here('output'), test, long_label=FALSE){
    file_del <- file_path_type(folder=folder, test=test, type='.del')

    items_del <- readLines(file_del) %>%
        as_tibble() %>%
        mutate(tabbed=str_detect(value, '\\t')) %>%
        filter(tabbed) %>%
        select(-tabbed) %>%
        separate(value,
                 into = c("iNum_step", 'delta', "temp1", "temp2", "temp3", "iLab", 'temp4'),
                 sep = "\\t") %>%
        select(-contains("temp")) %>%
        separate(iNum_step, into = c("iNum", "step"), sep = "\\.") %>%
        mutate_at(c("iNum", "step", "delta"), as.numeric)

    if (long_label){
        labs <- read.table(here::here('data', paste0(test, '_Labels.txt'))) %>%
            mutate(iNum=as.numeric(rownames(.))) %>%
            rename(Label=V1)
        items_del %>%
            left_join(labs, by='iNum') %>%
            select(-iNum, -iLab, Label) %>%
            unite('iStep', c('Label', 'step'), sep='_')
    } else {
        items_del  %>%
            unite('iStep', c('iLab', 'step'), sep='_') %>%
            select(-iNum)
    }
}
