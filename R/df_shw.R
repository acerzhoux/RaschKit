#' df_shw
#'
#' This function extracts item delta and error estimates from _shw.txt file. This
#' is associated with test named 'test'. If any item label is longer than 15
#' characters' fixed width, use long_label=TRUE to read complete labels from
#' 'test_Labels.txt' file in 'data' folder.
#'
#' @param folder Folder where ConQuest output files are located.
#' @param test Name of test.
#' @return Dataframe of item name, delta, and error.
#' @examples
#' df_shw(test, TRUE)
#' df_shw(test)
#' @export

df_shw <- function(folder='output', test){
  dfShw <- item_stats(folder, test)
  dplyr::select(
    dfShw,
    item="Item Title" ,
    delta="Item Estimate (item centred)",
    error="Item Error"
  )
}
