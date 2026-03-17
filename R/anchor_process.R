#' anchor_process
#'
#' This function removes deleted or no-data items, reorders items,
#' extracts anchors with new orders, and saves file into 'input' folder. The file
#' will be imported into ConQuest as anchor file. This is
#' associated with test named 'test'.
#'
#' @param test Name of test.
#' @param respDf Dataframe with pid, covariables (e.g,, DIF variable), and responses.
#' Default is NULL where Excel file with name 'test' in 'data' folder is used.
#' @param keyDf Vector of keys in the test. Default is NULL.
#' @param n_cov Number of covariates before responses.
#' @param nDimVec Vector of numbers of responses the dimensions have. Default is NULL.
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' Define this vector if multi-dimensional model is to be run, e.g., c(30, 45).
#' Also should define this if there are variables after response columns, e.g., 30.
#' @param ancDf Dataframe of 'Item', 'Delta' (and 'Step') for anchors.
#' @export

anchor_process <- function(test, respDf, keyDf, n_cov, nDimVec, ancDf, run){

  if (!(is.data.frame(ancDf) & ncol(ancDf) >= 2L & all(c('Item', 'Delta') %in% names(ancDf)))) {
    stop('ancDf should be a dataframe with variables Item, Delta (and Step if needed)!')
  }

  # id of removed item
  id_x <- which(keyDf$Key %in% c('x', 'X'))
  # id of item with one score category
  id_no_data <- which(
    {map_df(
      respDf[(n_cov+1):(n_cov+sum(nDimVec))],
      ~as.character(.x) |>
        str_replace_all(
          c('r' = NA_character_,
            'R' = NA_character_,
            'm' = NA_character_,
            'M' = NA_character_,
            'x' = NA_character_,
            'X' = NA_character_,
            '9' = NA_character_)
        )
    ) |>
    map_int(~table(.x) |> length())
    } == 1
  )
  id <- unique(c(id_x, id_no_data))
  if (!identical(id, integer(0))) {
    keyDf <- keyDf[-id, ]
  }

  folder <- paste0('input/', run, '/', test, '_anc.txt')
  if ('Step' %in% names(ancDf)) {
    keys1 <- keyDf |>
      select(Item) |>
      mutate(Step='')
    keys2 <- keyDf |>
      filter(Max_score>1)

    ancStepLst <- list()
    for (item in keys2$Item) {
      ancStepLst[[item]] <- tibble(
        Item=item,
        Step=as.character(1:(keys2[keys2$Item==item, ]$Max_score-1))
      )
    }

    bind_rows(
        keys1,
        reduce(ancStepLst, bind_rows)
      ) |>
      left_join(
        ancDf |>
          mutate(Step=ifelse(is.na(Step), '', Step)),
        by = c("Item", 'Step')
      ) |>
      unique() |>
      rowid_to_column('iNumNew') |>
      dplyr::filter(!is.na(Delta)) |>
      mutate(
        Item = str_c('/* ', Item, ' ', Step, ' */'),
        Delta=round(as.numeric(Delta), 3)
      ) |>
      dplyr::select(iNumNew, Delta, Item) |>
      write_fwf(folder)
  } else {
    keyDf |>
      select(Item) |>
      left_join(ancDf, by = "Item") |>
      unique() |>
      rowid_to_column('iNumNew') |>
      dplyr::filter(!is.na(Delta)) |>
      mutate(
        Item = str_c('/*', Item, '*/'),
        Delta=round(as.numeric(Delta), 3)
      ) |>
      dplyr::select(iNumNew, Delta, Item) |>
      write_fwf(folder)
  }
}
