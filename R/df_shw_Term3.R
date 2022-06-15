#' df_shw_Term3
#'
#' This function extract item statistics from 'test_facet.shw' file and add significance test results and flags. This is associated with test named 'test'.
#'
#' @param folder Folder where .shw file is located.
#' @param test Name of test.
#' @
#' @examples
#' df_shw_Term3(folder=folder, test='randomData')

df_shw_Term3 <- function(folder, test){
    labs <- read.table(here::here('data', paste0(test, '_Labels.txt'))) %>%
        mutate(iNum=as.numeric(rownames(.))) %>%
        rename(Label=V1)

    test <- paste0(test, '_facet')
    n_max <- {file_shw(folder=folder, test=test)%>%
        str_detect('An asterisk ') %>%
        which() %>% .[3] - 2} -
        {term3_L2(folder=folder, test=test)+6} + 1

    term3 <- read_fwf(file_path_type(folder=folder, test=test, type='shw'),
                 fwf_cols(iNum=c(2, 5),
                          Item=c(6, 16),
                          Category=c(21, 25),
                          Estimate=c(36, 43),
                          Error=c(46, 50),
                          Outfit=c(55, 58),
                          Outfit_t=c(73, 77),
                          Infit=c(81, 84),
                          Infit_t=c(100, 103)),
                 skip=(term3_L2(folder=folder, test=test) + 5),
                 n_max= n_max,
                 show_col_types = FALSE) %>%
        left_join(labs, by='iNum') %>%
        select(-Item, Item=Label) %>%
        mutate(Estimate=parse_number(Estimate))

    term3 %>%
        select(iNum, Item, everything()) %>%
        arrange(iNum, Category) %>%
        mutate(Sig=ifelse(abs(Estimate)>1.96*Error, '*', NA),
               `Est>0.5`=ifelse(abs(Estimate)>0.5, '*', NA),
               `Est>1`=ifelse(abs(Estimate)>1, '*', NA),
               Flag=ifelse((Sig=='*' & `Est>0.5`=='*' & `Est>1`=='*'), 1, NA))
}
