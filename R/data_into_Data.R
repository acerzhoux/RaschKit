#' data_into_Data
#'
#' This function saves dataset 'test_Data.txt' into 'data' folder. The saved
#' dataset is imported into ConQuest later for modeling. This is associated
#' with test named 'test'.

#' @param test Name of test. Default is NULL.
#' @param data Dataframe where responses are put together.
#' @param DIFVar Name of DIF variable. Should be lowercase for ConQuest to run.
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' Default is NULL.
#' @examples
#' data_into_Data()
#' @export

data_into_Data <- function(test=NULL, data, DIFVar=NULL, run){
    if (is.null(DIFVar)){
        file_path <- paste0('data/', run, '/', test, '_', 'Data.txt')
    } else {
        file_path <- paste0('data/', test, '_', DIFVar, '.txt')
    }

    as.data.frame(data) |>
    gdata::write.fwf(
        file_path,
        justify="left",
        na=" ",
        colnames = FALSE,
        sep = ""
    )
}
