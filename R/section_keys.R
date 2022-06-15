#' section_keys
#'
#' This function reads in 'test_Key.txt' from 'data' folder and specifies the Key section of the 'test.cqc' file in 'input' folder. Both procedures are associated with test named 'test'.
#'
#' They key file has one row for all-multiple-choice-item test, or multiple rows if any item has double key or polytomous scoring.
#'
#' @param folder The 'data' folder where key 'test_Key.txt' file is located.
#' @param test Name of the test.
#' @param dbl_key TRUE if the key of any item is double key.
#' @param poly_key TRUE if the key of any item has polytomous scoring.
#' @param delete Vector of to-be-removed items' order numbers in the test, e.g., c(4, 7, 65, 114).
#' @return String of characters used in key section of 'test.cqc' file in 'input' folder.
#' @examples
#' section_keys()

section_keys <- function(folder, test, dbl_key, poly_key, delete){
    # double or polytomous keys: Read from 'keys.xlsx' file in 'data' folder
    if (dbl_key | poly_key){
        keys <- readxl::read_xlsx(here::here('data', 'keys.xlsx'), test)
        key_conv <- function(keys){
            max_scr <- max(keys$Max_score)
            key_ls <- list()
            for (i in 1:nrow(keys)){
                if (keys[i, ]$Key %in% c('x', 'X')){
                    key_ls[[i]] <- rep('x', max_scr)
                } else {
                    if (keys[i, ]$Max_score == 1) {
                        key_ls[[i]] <- c(keys[i, ]$Key, rep('x', max_scr-1))
                    } else {
                        i_max <- keys[i, ]$Max_score
                        key_ls[[i]] <- c(1:i_max, rep('x', max_scr-i_max))
                    }
                }
            }
            data.table::transpose(key_ls)
        }
        keys <- key_conv(keys)
        keys <- map(1:length(keys), ~keys[[.]] %>% paste0(collapse=''))
        keys <- unlist(keys, recursive = TRUE, use.names = TRUE)
    } else {
        # MC items: Read from 'racp1_Key.txt' file in 'data' folder
        keys <- read_keys(folder=folder, test=test)
    }

    # change deleted items' keys to 'x' to keep Alpha
    if (!is.null(delete)){
        key_ls <- strsplit(keys, "")
        for (i in seq_along(key_ls)){
            key_ls[[i]][delete] <- 'x'
        }
        keys <- map(key_ls, ~paste0(., collapse='')) %>%
            unlist
    }

    # special treatment for double and polytomous key lines in .cqc file
    if (dbl_key){
        cq_key <- NULL
        for (i in seq_along(keys)){
            cq_key <- c(cq_key, paste0('key ', keys[[i]], ' !', 1, ';\n'))
        }
        cq_key <- paste0(cq_key, collapse = '')
    }
    if (poly_key){
        cq_key <- NULL
        for (i in seq_along(keys)){
            cq_key <- c(cq_key, paste0('key ', keys[[i]], ' !', i, ';\n'))
        }
        cq_key <- paste0(cq_key, collapse = '')
    } else {
        cq_key <- paste('key', keys, '!1;\n')
    }

    # return key lines
    cq_key
}
