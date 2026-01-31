#' delta_DIF_dich
#'
#' This function extracts delta and indice statistics for two categories of
#' dichotomous DIF variable.
#'
#' @param test Name of test.
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'.
#' @param quick Whether quick error is needed. Default is TRUE for DIF analysis.
#' @return List of delta and indice dataframes.
#' @examples
#' delta_DIF_dich(test, DIFVar, quick)
#' @export

delta_DIF_dich <- function (test, DIFVar, quick = TRUE){
  labs <- read.table(paste0('data/', test, '_Labels.txt')) |>
    rowid_to_column('iNum') |>
    dplyr::rename(item=V1) |>
    as_tibble()

  folder <- paste0('DIF/', DIFVar)
  n_item <- N_item2(NULL, test)

  dfDIF <- readxl::read_xls(
      paste0(folder, '/', test, '_its.xls'),
      skip=5,
      n_max=n_item*2,
      .name_repair="unique_quiet",
      col_types='text'
    ) |>
    select(
      N, Facil=Facility,
      Discr=`Item-Rest Cor`,
      delta=`Avg Delta`
    ) |>
    mutate(Category=rep(c('x', 'y'), n_item)) |>
    pivot_wider(names_from = Category, values_from = N:delta, values_fn = list) |>
    map_dfr(unlist) |>
    modify_if(is.character, as.numeric) |>
    rowid_to_column('iNum')

  names(dfDIF) <- gsub("\\_x", "\\.x", names(dfDIF))
  names(dfDIF) <- gsub("\\_y", "\\.y", names(dfDIF))

  dfIndice <- left_join(
      labs,
      dfDIF |>
      select(1, 2, 4, 6, 3, 5, 7),
      by='iNum'
    ) |>
    select(-iNum) |>
    na.omit() |>
    as_tibble()

  shw.col.1 <- readxl::read_xls(
      paste0(folder, '/', test, '_shw.xls'),
      sheet='ResponseModel',
      .name_repair="unique_quiet"
    ) |>
    select(a=1)
  row.item <- which(shw.col.1$a=='item')

  dfErrorLong <- readxl::read_xls(
      paste0(folder, '/', test, '_shw.xls'),
      sheet='ResponseModel',
      skip=row.item[[2]],
      n_max=n_item*2+1,
      .name_repair="unique_quiet",
      col_types='text'
    ) |>
    select(item, error=`ERROR^`) |>
    mutate(
      item=sub("^[^ ]+ ", "", item),
      error=as.numeric(error)
    ) |>
    na.omit()
  n_item1 <- round(nrow(dfErrorLong)/2)
  dfError <- dfErrorLong |>
    mutate(Category=rep(c('error.x', 'error.y'), each=n_item1)) |>
    pivot_wider(
      names_from = 'Category',
      values_from = 'error'
    ) |>
    map_dfr(unlist) |>
    rowid_to_column('iNum') |>
    dplyr::select(-item) |>
    as_tibble()

  dfDelta <- left_join(
    labs,
    left_join(
      dfDIF |>
        select(iNum, delta.x, delta.y),
      dfError,
      by='iNum'
    ),
    by='iNum'
  ) |>
  select(-iNum)

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

  if (quick){
    dfDelta$delta.x <- dfDelta$delta.x - mean(dfDelta$delta.x)
    dfDelta$delta.y <- dfDelta$delta.y - mean(dfDelta$delta.y)
  }

  list(
    dfDelta=dfDelta,
    dfIndice=dfIndice
  )
}
