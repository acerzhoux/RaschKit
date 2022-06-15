#' df_shw_Step
#'
#' This function extracts step parameter estimates from .shw file. This is associated with test named 'test'. If any item label is longer than 13 characters' fixed width, use long_label=TRUE to read complete labels from 'test_Labels.txt' file in 'data' folder.
#'
#' @param folder Folder where .shw file is located.
#' @param test Name of test.
#' @param long_label Whether item labels are longer than 15 characters' fixed width. Default is FALSE.
#' @examples
#' df_shw_Step(test='elana_poly_score')
#' df_shw_Step(test='elana_poly_score', long_label=TRUE)

df_shw_Step <- function(folder=here::here('output'), test, long_label=FALSE){
    steps_shw <- read_fwf(file_path_type(folder=folder, test=test, type='shw'),
             fwf_cols(iNum=c(2, 5),
                      item=c(6, 18),
                      step=c(19, 35),
                      error=c(46, 54)),
             skip=term_L2_Step(folder=folder, test=test)+5,
             n_max=N_item_Step(folder=folder, test=test),
             show_col_types = FALSE)

    if (long_label){
        labs <- read.table(here::here('data', paste0(test, '_Labels.txt'))) %>%
            mutate(iNum=as.numeric(rownames(.))) %>%
            rename(Label=V1)
        steps_shw %>%
            left_join(labs, by='iNum') %>%
            select(-iNum, -item) %>%
            unite('iStep', c('Label', 'step'), sep='_')
    } else {
        steps_shw %>%
            unite('iStep', item:step, sep='_') %>%
            select(-iNum)
    }
}
