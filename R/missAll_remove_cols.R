#' missAll_remove_cols
#'
#' This function removes columns of all missing values from a dataframe.
#'
#' @param df Dataframe with columns of all missing values.
#' @return Dataframe without columns of all missing values.
#' @examples
#' missAll_remove_cols()
#' @export

missAll_remove_cols <- function(df){
    #https://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
    df[, colSums(is.na(df))<nrow(df)]
}
