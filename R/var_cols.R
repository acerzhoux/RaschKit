#' var_cols
#'
#' This function calculates and outputs a variable's beginning and ending in a dataframe if saved as a fixed-width .txt file.

#' @param df Dataframe with ID, covariates and responses.
#' @param var_name Variable name in the dataframe, e.g., 'gender'.
#' @return List. Name is variable. Element is variable's column numbers in 'test_Data.txt' file in 'data' folder.
#' @examples
#' var_cols(df=cov_respns, var_name='gender')
#' @export

var_cols <- function(df, var_name){
    ls <- list()
    var_order <- which(names(df)==var_name)
    if (var_order==1){
        ls[[var_name]] <- paste0(1, '-', {df[[var_name]] %>% na.omit() %>% nchar %>% max})
    }  else {
        cols_bfr_id <- var_order - 1
        strt <- sum(map_int(1:cols_bfr_id, ~df[[.]] %>% na.omit() %>% nchar %>% max))
        edj <- strt + {df[[var_name]] %>% na.omit() %>% nchar %>% max}
        var_cols <- paste0((strt + 1), '-', edj)
        ls[[var_name]] <- var_cols
    }
    ls
}
