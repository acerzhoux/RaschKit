#' poly_recode
#'
#' This function down ranks non-continuous polytomous item response vectors
#' in a dataframe. If any item is recoded, a file with recoding ways is saved
#' into 'data' folder.
#'
#' @param data Dataframe with pid, covariables (e.g,, DIF variable), and
#' responses. Default is NULL where Excel file with name 'test' in 'data'
#' folder is used.
#' @param keys Dataframe of 'Item', 'Key', and 'Max_score' (add Key2 if double key).
#' @param n_cov Number of covariates before responses.
#' @param miss_code Missing codes. Default is c('r','R','m','M','9','x','X','.','',' ',NA).
#' @export

poly_recode <- function(keys, data, n_cov, miss_code=c('r','R','m','M','9','x','X','.','',' ',NA)){
  id_poly <- which(keys$Max_score > 1) + n_cov

  # save recoding ways to 'data' folder
  rcde <- map(data[id_poly], ~poly_recode_one(.x, miss_code = miss_code)[[2]])
  if (!all(map_lgl(rcde, is.null))){
    Filter(Negate(is.null), rcde) |>
      imap(~mutate(.x, Item=.y)) |>
      reduce(bind_rows) |>
      select(Item, everything()) |>
      write.csv(paste0('data/', test, '_recode_score.csv'), row.names=FALSE)
  }

  # output recoded dataframe
  modify_at(data, id_poly, ~poly_recode_one(.x, miss_code = miss_code)[[1]])
}
