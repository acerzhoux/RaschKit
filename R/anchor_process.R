#' anchor_process
#'
#' This function processes anchor file of all-MC test to be input into ConQuest. This is
#' associated with test named 'test'.
#'
#' @param test Name of test.
#' @param data Dataframe with pid, covariables (e.g,, DIF variable), and responses.
#' Default is NULL where Excel file with name 'test' in 'data' folder is used.
#' @param keys Vector of keys in the test. Default is NULL.
#' @param labels Vector of item labels that correspond to order of item response columns in data.
#' @param delete Vector of orders of items to be removed. Default is NULL.
#' @param poly_key TRUE if the key of any item has polytomous scoring. Default is FALSE.
#' @param n_cov Number of covariates before responses.
#' @param n_dims Vector of numbers of responses the dimensions have. Default is NULL.
#' Define this vector if multi-dimensional model is to be run, e.g., c(30, 45).
#' Also should define this if there are variables after response columns, e.g., 30.
#' @param dfAnc Dataframe of 'Item' and 'Delta' for anchors.
#' @export

anchor_process <- function(test, data, keys, labels, delete, poly_key,
               n_cov, n_dims, dfAnc){
  # id of removed item
  id_x <- unique(c(which(keys$Key %in% c('x', 'X')), delete))
  # id of item with one score category
  id_no_data <- which(
    {map_df(
      data[(n_cov+1):(n_cov+sum(n_dims))],
      ~as.character(.x) |>
        str_replace_all(
          c('r' = NA_character_,
            'R' = NA_character_,
            'm' = NA_character_,
            'M' = NA_character_,
            'x' = NA_character_,
            'X' = NA_character_,
            ' ' = NA_character_,
            '9' = NA_character_)
        )
    ) |>
    map_int(~table(.x) |> length())
    } == 1
  )
  id <- unique(c(id_x, id_no_data))

  # remove deleted or no-data items, reorder items
  # extrac anchors with new orders, save into 'input' folder
  tibble(
    iNum = 1:length(labels),
    Item = labels
  ) |>
  left_join(dfAnc, by = "Item") |>
  unique() |>
  dplyr::filter(!(iNum %in% id)) |>
  rowid_to_column('iNumNew') |>
  dplyr::filter(!is.na(Delta)) |>
  mutate(Item = str_c('/*', Item, '*/')) |>
  dplyr::select(iNumNew, Delta, Item) |>
  write.table(
    paste0('input/', test, '_anc.txt'),
    quote=FALSE,
    sep="\t",
    row.names=FALSE,
    col.names=FALSE
  )
}
