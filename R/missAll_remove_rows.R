#' missAll_remove_rows
#'
#' This function removes cases with all missing responses of
#' c('@@', '@@@', NA, '9', '.').

#' @param df Dataframe that contains responses of all missing values.
#' @param begin The 1st item's order in test, e.g., 7.
#' @param end The last item order in test, e.g., 46.
#' @param rm Whether to remove all-missing cases. Default is TRUE.
#' @return Dataframe that has responses of all missing values removed.
#' @examples
#' missAll_remove_rows()
#' @export

missAll_remove_rows <- function(df, begin, end, rm=TRUE){
  Flag <- apply(
    df[begin:end], 1,
    function(x) all(x %in% c('@@', '@@@', NA, '9', '.'))
  )
  df <- cbind(df, Flag)

  if (rm) {
    filter(df, !Flag) |>
      dplyr::select(-Flag)
  } else {
    df
  }
}
