#' delta_DIF_dich_step
#'
#' This function extracts delta statistics for two categories of dichotomous
#' DIF variable.
#'
#' @param test Name of test.
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'.
#' @return Dataframe of variables of 'delta.x', 'delta.y'.
#' @examples
#' delta_DIF_dich_step(test='WA', DIFVar='ATSI')
#' @export

delta_DIF_dich_step <- function(test, DIFVar){
  folder <- paste0('DIF/', DIFVar)

  labs <- read.table(paste0('data/', test, '_Labels.txt')) |>
    rowid_to_column('qOrder') |>
    rename(qid=V1)

  test <- str_c(test, '_', 'step')
  n_item <- N_item(folder, test)

  ######## delta

  x <- str_file(folder, test, 'itn')
  ind <- grep('Item Delta\\(s\\)', x)
  x <- map(
      x[ind],
      ~str_sub(.x, 15, -1) |>
        str_replace_all('-', ' -') |>
        str_squish() |>
        str_split(' ')
    ) |>
    map(1) |>
    map(as.numeric)

  deltas_ls <- list(
    map2(
      map(x[2*(1:n_item)-1], ~.x - mean(map_dbl(x[2*(1:n_item)-1], mean))),
      labs$qid,
      ~tibble(item=str_c(.y, '_', 1:length(.x)), delta=.x)
    ) |>
    reduce(bind_rows),
    map2(
      map(x[2*(1:n_item)], ~.x - mean(map_dbl(x[2*(1:n_item)], mean))),
      labs$qid,
      ~tibble(item=str_c(.y, '_', 1:length(.x)), delta=.x)
    ) |>
    reduce(bind_rows)
  )

  deltas <- map2(
      deltas_ls,
      list('.x', '.y'),
      ~.x |> `colnames<-`(c('item', str_c('delta', .y)))
    ) |>
    reduce(inner_join, by='item')

  ######## error

  file_shw <- Path(folder, test, 'shw')
  y <- readLines(file_shw)
  ind1 <- grep('TERM 4\\: item\\*step\\*', y)[[1]]+5
  ind2 <- grep('An asterisk next to', y)[[4]]-2

  # locate skip rows
  shw.1 <- readxl::read_xls(
    paste0(folder, '/', test, '_shw.xls'),
    sheet='ResponseModel',
    .name_repair="unique_quiet"
  ) |>
  select(a=1)
  skips <- which(shw.1$a=='item')

  dfError <- readxl::read_xls(
      paste0(folder, '/', test, '_shw.xls'),
      sheet='ResponseModel',
      skip=skips[[3]],
      n_max=ind2-ind1+1,
      .name_repair="unique_quiet",
      col_types='numeric'
    ) |>
    select(
      qOrder=item,
      cat=`...4`,
      DIFVar=tolower(DIFVar),
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

  dfDelta <- inner_join(deltas, errors, by = "item")

  # record items with missing delta
  id_na <- which(apply(dfDelta, 1, function(x) any(is.na(x[2:5]))))
  if (length(id_na)){
    fileRmd <- 'DIF/0 iRmd.csv'
    missDf <- dfDelta[id_na, ] |>
      mutate(
        Test=test,
        DIFVar=DIFVar
      )
    if (file.exists(fileRmd)) {
      read.csv(fileRmd) |>
        bind_rows(
          missDf
        ) |>
        write.csv(fileRmd, row.names=FALSE)
    } else {
      missDf |>
        write.csv(fileRmd, row.names=FALSE)
    }
    dfDelta <- na.omit(dfDelta)
  }

  dfDelta
}
