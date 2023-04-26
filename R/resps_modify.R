#' resps_modify
#'
#' This function consecutively remove the last item from the delete list of
#' items until no last item is in it.
#'
#' @param keyDf Dataframe of 'Item', 'Key', and 'Max_score' (add Key2 if double key).
#' @param resps_cols Column number(s) of responses in the data .txt file.
#' @param delVec Vector of item order number to be removed from the test,
#' e.g., c(2, 3, 45, 46).
#' @export

resps_modify <- function(keyDf, resps_cols, delVec){
    n_key <- nrow(keyDf)

    # if last item(s) in delete list, remove last consecutive items
    col_range <- as.numeric(str_split(resps_cols, '-')[[1]])
    item_last <- (col_range[[2]]-col_range[[1]]+1)
    if (item_last %in% delVec){
        delVec <- sort(delVec)
        n_end <- sum(tail_while(diff(delVec), function(x) x == 1)) + 1
        delVec <- delVec[1:(length(delVec)-n_end)]
        resps_cols <- str_c(col_range[[1]], (col_range[[2]]-n_end), sep='-')
    }
    resps_cols
}
