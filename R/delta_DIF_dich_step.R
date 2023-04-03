#' delta_DIF_dich_step
#'
#' This function extracts delta statistics for two categories of dichotomous
#' DIF variable.
#'
#' @param test Name of test.
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'.
#' @param quick Whether quick error is needed. Default is TRUE for DIF analysis.
#' @return Dataframe of variables of 'delta.x', 'delta.y'.
#' @examples
#' delta_DIF_dich_step(test='Writing', DIFVar='gender')
#' @export

delta_DIF_dich_step <- function(test, DIFVar, quick=TRUE){
  folder <- paste0('DIF/', DIFVar)

  labs <- read.table(paste0('data/', test, '_Labels.txt')) |>
    rowid_to_column('qOrder') |>
    rename(qid=V1)

  test <- str_c(test, '_', 'step')
  n_item <- N_item(folder, test)

  # delta
  x <- str_file(folder, test, 'itn')
  ind <- grep('Item Delta\\(s\\)', x)
  x <- map(x[ind], ~str_sub(.x, 15, -1) |> str_squish() |> str_split(' ')) |>
    map(1) |>
    map(as.numeric)

  deltas_ls <- list(
    map2(x[2*(1:n_item)-1], labs$qid,
       ~tibble(item=str_c(.y, '_', 1:length(.x)), delta=.x)) |>
      reduce(bind_rows),
    map2(x[2*(1:n_item)], labs$qid,
       ~tibble(item=str_c(.y, '_', 1:length(.x)), delta=.x)) |>
      reduce(bind_rows)
  )

  if (quick){ # for each DIFVar, adjust by mean of each item's steps' mean
    MofM1 <- map_dbl(x[2*(1:n_item)-1], mean) |> mean()
    MofM2 <- map_dbl(x[2*(1:n_item)], mean) |> mean()
    deltas_ls <- map2(deltas_ls, list(MofM1, MofM2), ~mutate(.x, delta=delta-.y))
  }

  deltas <- map2(
      deltas_ls,
      list('.x', '.y'),
      ~.x |> `colnames<-`(c('item', str_c('delta', .y)))
    ) |>
    reduce(inner_join, by='item')

  # error
  file_shw <- Path(folder, test, 'shw')
  y <- readLines(file_shw)
  ind1 <- grep('TERM 4\\: item\\*step\\*', y)[[2]]+5
  ind2 <- grep('An asterisk next to', y)[[4]]-2

  dfError <- readxl::read_xls(
      paste0(folder, '/', test, '_shw.xls'),
      sheet='ResponseModel',
      skip=8+n_item+24+n_item*2+9,
      n_max=ind2-ind1+1,
      .name_repair="unique_quiet",
      col_types='numeric'
    ) |>
    select(
      qOrder=item,
      cat=`...4`,
      DIFVar=DIFVar,
      error=`ERROR^`
    ) |>
    filter(!is.na(qOrder), cat!=0)

  errors <- split(dfError, list(dfError$qOrder, dfError$DIFVar)) |>
    map(~mutate(.x, error=ifelse(is.na(error), error[1], error))) |>
    reduce(bind_rows) |>
    left_join(labs, by = "qOrder") |>
    mutate(item=str_c(qid, '_', cat)) |>
    pivot_wider(
      names_from = DIFVar,
      values_from = error
    ) |>
    select(item, error.x=`1`, error.y=`2`)

  left_join(deltas, errors, by = "item")
}
