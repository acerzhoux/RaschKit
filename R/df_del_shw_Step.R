#' df_del_shw_Step
#'
#' This function extract item steps' delta estimates from .shw file. This is associated with test named 'test'. If any item label is longer than 13 characters' fixed width, use long_label=TRUE to read complete labels from 'test_Labels.txt' file in 'data' folder.
#'
#' @param folder Folder where .shw file is located. Default is 'output' folder.
#' @param test Name of test.
#' @param long_label Whether item labels are longer than 15 characters' fixed width. Default is FALSE.
#' @return Dataframe of item name_step, delta, and error.
#' @examples
#' df_del_shw_Step(test='elana_poly_score')
#' df_del_shw_Step(test='elana_poly_score', long_label=TRUE)

df_del_shw_Step <- function(folder=here::here('output'), test, long_label=FALSE){
    df_del(folder=folder, test=test, long_label=long_label) %>%
        left_join(
            df_shw_Step(folder=folder, test=test, long_label=long_label),
            by=c('iStep')) %>%
        na.omit()
}
