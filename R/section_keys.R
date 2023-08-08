#' section_keys
#'
#' This function uses keyDf to generate key strings for CQ control file. A test
#' of multiple choice with one key has one row/string, and otherwise multiple
#' rows/strings (e.g., double key or polytomous scoring).
#'
#' @param keyDf Dataframe of 'Item', 'Key', and 'Max_score'. Can have 'Key2',
#' 'Key3', etc. if multiple keys.
#' @return String of characters used in key section of 'test.cqc' file in
#' 'input' folder.
#' @examples
#' section_keys(cdLst[[1]])
#' @export

section_keys <- function(keyDf){
  if (('Key2' %in% names(keyDf)) && any(!is.na(keyDf$Key2))) {
    multiKeyLgt <- TRUE
    ks <- names(select(keyDf, contains('Key')))[-1]
    kMax <- max(parse_number(ks))
    for (k in ks) {
      keyDf[[k]][which(is.na(keyDf[[k]]))] <- 'x'
    }
    lineMulti <- unlist(map(ks, ~paste0(keyDf[[.x]], collapse='')))
  } else {
    multiKeyLgt <- FALSE
  }

  if (max(keyDf$Max_score) > 1) {
    poly_key <- TRUE
  } else {
    poly_key <- FALSE
  }

  # ########## Step 1: Process multiple key #########
  if (poly_key){
    # keyDf into string: One line per score
    key_conv <- function(keyDf){
      max_scr <- max(keyDf$Max_score)
      key_ls <- list()
      for (i in 1:nrow(keyDf)){
        if (keyDf[i, ]$Key %in% c('x', 'X')){
          key_ls[[i]] <- rep('x', max_scr)
        } else {
          if (keyDf[i, ]$Max_score == 1) {
            key_ls[[i]] <- c(keyDf[i, ]$Key, rep('x', max_scr-1))
          } else {
            i_max <- keyDf[i, ]$Max_score
            key_ls[[i]] <- c(1:i_max, rep('x', max_scr-i_max))
          }
        }
      }
      key_ls |>
        unlist() |>
        matrix(nrow(keyDf), max_scr, T) |>
        as.data.frame() |>
        map(~paste0(.x, collapse='')) |>
        unlist()
    }
    keyStrs <- key_conv(keyDf)
  } else { # MC items
    # the single key row
    keyRow <- paste0(keyDf$Key, collapse='')

    # If multiple keys, add one extra key lines below
    if (multiKeyLgt){
      keyStrs <- c(keyRow, lineMulti)
    } else {
      keyStrs <- keyRow
    }
  }

  # ########## Step 2: Specify scores #########
  # polytomous items
  if (poly_key){
    cq_key <- map(1:length(keyStrs), ~paste0('key ', keyStrs[[.x]], ' !', .x, ';\n')) |>
      unlist()
    if (multiKeyLgt){
      cq_key <- map(1:length(lineMulti), ~paste0('key ', lineMulti[[.x]], ' !', 1, ';\n')) |>
        unlist() |>
        c(cq_key)
    }
  } else {# MC items
    cq_key <- map(1:length(keyStrs), ~paste0('key ', keyStrs[[.x]], ' !', 1, ';\n')) |>
      unlist()
  }

  # return key string
  paste0(cq_key, collapse = '')
}
