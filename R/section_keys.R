#' section_keys
#'
#' This function reads in 'test_Key.txt' from 'data' folder and specifies
#' the Key section of the 'test.cqc' file in 'input' folder. Both procedures
#' are associated with test named 'test'.
#'
#' They key file has one row for all-multiple-choice-item test, or multiple
#' rows if any item has double key or polytomous scoring.
#'
#' @param test Name of the test.
#' @param keyDf Dataframe of 'Item', 'Key', and 'Max_score'.
#' @param dblKeyLst List of items with double keys. Element is double keys, and
#' element name is item order, e.g., list(`7`=c(1,3), `9`=c(3,4). Default is NULL.
#' @param poly_key TRUE if the key of any item has polytomous scoring.
#' Default is FALSE.
#' @param delVec Vector of to-be-removed items' order numbers in the test,
#' e.g., c(4, 7, 65, 114). Default is NULL.
#' @return String of characters used in key section of 'test.cqc' file in
#' 'input' folder.
#' @examples
#' section_keys(test='b', delVec=c(2, 5))
#' section_keys(test='b', dblKeyLst=list(`5`=c(1,3), `29`=c(3,4)))
#' section_keys(test='a', poly_key=TRUE)
#' section_keys(test='b', poly_key=TRUE, delVec=c(2, 5))
#' @export

section_keys <- function(test, keyDf, dblKeyLst=NULL,
                         poly_key=FALSE, delVec=NULL){
  # ########## Step 1: Process double key #########
  if (poly_key){
    # if double key, record and add one extra line on top
    if (('Key2' %in% names(keyDf)) & any(!is.na(keyDf$Key2))) {
      dblKeyLst <- TRUE
      keyDf$Key2[which(is.na(keyDf$Key2))] <- 'x'
      Line_top <- paste0(keyDf$Key2, collapse='')
    }

    # Keys into string: One line per score
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
    keyDf <- key_conv(keyDf)
    keyDf <- map(1:length(keyDf), ~keyDf[[.]] %>% paste0(collapse=''))
    keyDf <- unlist(keyDf, recursive = TRUE, use.names = TRUE)

    # If double score, add one extra 1-score line on top
    if (isTRUE(dblKeyLst)) keyDf <- c(Line_top, keyDf)
  } else { # MC items
    # double-key: add double key
    if (!is.null(dblKeyLst)){
      id_dbl <- names(dblKeyLst)
      key_vec <- keyDf$Key
      key_vec_x <- rep('x', length(key_vec))
      for (i in id_dbl){
        key_vec[[as.numeric(i)]] <- dblKeyLst[i][[1]][[1]]
        key_vec_x[[as.numeric(i)]] <- dblKeyLst[i][[1]][[2]]
      }
      keyDf <- c(paste0(key_vec, collapse=''),
            paste0(key_vec_x, collapse=''))
    } else {
      keyDf <- paste0(keyDf$Key, collapse='')
    }
  }

  # ########## Step 2: Delete items #########
  # change deleted items' keys to 'x' to keep Alpha
  if (!is.null(delVec)){
    key_ls <- strsplit(keyDf, "")
    for (i in seq_along(key_ls)){
      key_ls[[i]][delVec] <- 'x'
    }
    keyDf <- map(key_ls, ~paste0(., collapse='')) %>% unlist

    # remove deleted, last item(s)
    N <- length(key_ls[[1]])
    if (N %in% delVec){
      delVec <- sort(delVec)
      n_end <- sum(tail_while(diff(delVec), function(x) x == 1)) + 1
      keyDf <- str_sub(keyDf, 1, -(n_end+1))
    }
  }

  # ########## Step 3: Specify scores #########
  # polytomous items
  if (poly_key){
    if (is.null(dblKeyLst)){
      cq_key <- NULL
      for (i in seq_along(keyDf)){
        cq_key <- c(cq_key, paste0('key ', keyDf[[i]], ' !', i, ';\n'))
      }
      cq_key <- paste0(cq_key, collapse = '')
    } else if (dblKeyLst){
      cq_key <- NULL
      for (i in seq_along(keyDf)){
        if (i %in% c(1, 2)) {
          cq_key <- c(cq_key, paste0('key ', keyDf[[i]], ' !', 1, ';\n'))
        } else {
          cq_key <- c(cq_key, paste0('key ', keyDf[[i]], ' !', i-1, ';\n'))
        }
      }
      cq_key <- paste0(cq_key, collapse = '')
    }
  } else {# MC items
    if (is.null(dblKeyLst)){
      cq_key <- paste('key', keyDf, '!1;\n')
    } else {
      cq_key <- NULL
      for (i in seq_along(keyDf)){
        cq_key <- c(cq_key, paste0('key ', keyDf[[i]], ' !', 1, ';\n'))
      }
      cq_key <- paste0(cq_key, collapse = '')
    }
  }

  # return key lines
  cq_key
}
