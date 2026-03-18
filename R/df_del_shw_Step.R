<<<<<<< HEAD
#' df_del_shw_Step
#'
#' This function extract item steps' delta estimates from .shw file. This is
#' associated with test named 'test'.
#'
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' @param test Name of test.
#' @return Dataframe of item name_step, delta, and error.
#' @examples
#' df_del_shw_Step(test='ELNA')
#' @export

df_del_shw_Step <- function(run, test){
    df_del(run, test) |>
    right_join(
        df_shw_Step(run, test),
        by=c('item')
    )
}
=======
#' df_del_shw_Step
#'
#' This function extract item steps' delta estimates from .shw file. This is
#' associated with test named 'test'.
#'
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' @param test Name of test.
#' @return Dataframe of item name_step, delta, and error.
#' @examples
#' df_del_shw_Step(test='ELNA')
#' @export

df_del_shw_Step <- function(run, test){
    df_del(run, test) |>
    right_join(
        df_shw_Step(run, test),
        by=c('item')
    )
}
>>>>>>> 200a4cdb5116cf069a4061c06c737fe9e45a4f72
