#' resps_modify
#'
#' This function consecutively remove the last item from the delete list of 
#' items until no last item is in it.
#'
#' @param folder The 'data' folder where key 'test_Key.txt' file is located.
#' @param test Name of the test.
#' @param resps_cols Column number(s) of responses in the data .txt file.
#' @param delete Vector of item order number to be removed from the test, 
#' e.g., c(2, 3, 45, 46).
#' @export

resps_modify <- function(folder, test, resps_cols, delete){
    keys <- read_keys(folder=folder, test=test)
    n_key <- length(keys)

    # if last item(s) in delete list, remove last consecutive items
    col_range <- as.numeric(str_split(resps_cols, '-')[[1]])
    item_last <- (col_range[[2]]-col_range[[1]]+1)
    if (item_last %in% delete){
        delete <- sort(delete)
        n_end <- sum(tail_while(diff(delete), function(x) x == 1)) + 1
        delete <- delete[1:(length(delete)-n_end)]
        keys <- str_sub(keys, 1, -(n_end+1))
        resps_cols <- str_c(col_range[[1]], (col_range[[2]]-n_end), sep='-')
    }
    resps_cols
}
