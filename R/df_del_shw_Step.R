#' df_del_shw_Step
#'
#' This function extract item steps' delta estimates from .shw file. This is
#' associated with test named 'test'.
#'
#' @param folder Folder where xxx_shw.txt file is located. Default is 'output' folder.
#' @param test Name of test.
#' @param long_label Whether item labels are longer than 15 characters' fixed
#' width. Default is FALSE.
#' @return Dataframe of item name_step, delta, and error.
#' @examples
#' df_del_shw_Step(test='ELNA')
#' @export

df_del_shw_Step <- function(folder='output', test){
    df_del(folder, test) |>
    right_join(
        df_shw_Step(folder, test),
        by=c('item')
    )
}
