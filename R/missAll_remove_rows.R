#' missAll_remove_rows
#'
#' This function removes cases with all missing responses of 
#' c('@@', '@@@', NA, '9', 'R', 'r', '', ' ').

#' @param df Dataframe that contains responses of all missing values.
#' @param begin The 1st item's order in test, e.g., 7.
#' @param end The last item order in test, e.g., 46.
#' @param rm Whether to remove all-missing cases. Default is TRUE.
#' @return Dataframe that has responses of all missing values removed.
#' @examples
#' missAll_remove_rows()
#' @export

missAll_remove_rows <- function(df, begin, end, rm=TRUE){
    df$flag_miss <- NA
    n_case <- dim(df)[1]
    for (i in 1:n_case){
        if (all(flatten_chr(df[i, (begin:end)]) %in% c('@@', '@@@', NA, '9', 'R', 'r', '', ' '))){
            df[i, 'flag_miss'] <- 1
        }
    }
    if (rm) df <- df %>% filter(is.na(flag_miss)) %>% select(-flag_miss)
    df
}
