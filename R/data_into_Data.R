#' data_into_Data
#'
#' This function saves dataset 'test_Data.txt' into 'data' folder. The saved dataset is imported into ConQuest later for modeling. This is associated with test named 'test'.

#' @param test Name of test. Default is NULL.
#' @param data Dataframe where responses are put together.
#' @examples
#' data_into_Data()
#' @export

data_into_Data <- function(test=NULL, data){
    data %>% as.data.frame() %>%
        gdata::write.fwf(
            here::here('data', paste0(if(!is.null(test)) paste0(test, '_'), 'Data.txt')),
            justify="left", na=" ",
            colnames = FALSE, sep = "")
}
