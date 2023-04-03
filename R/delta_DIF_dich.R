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
#' delta_DIF_dich(test='Writing', DIFVar='gender')
#' @export

delta_DIF_dich <- function (test, DIFVar, quick = TRUE){
  labs <- read.table(paste0('data/', test, '_Labels.txt')) |>
    rowid_to_column('iNum') |>
    dplyr::rename(item=V1)

  folder <- paste0('DIF/', DIFVar)
  n_item <- N_item2(folder, test)

  dfDIF <- readxl::read_xls(
      paste0(folder, '/', test, '_its.xls'),
      skip=5,
      n_max=n_item*2,
      .name_repair="unique_quiet",
      col_types='numeric'
    ) |>
    select(
      N, Facil=Facility,
      Discr=`Item-Rest Cor`,
      # Infit=`Wghtd MNSQ`,
      delta=`Avg Delta`
    ) |>
    mutate(Category=rep(c('x', 'y'), n_item)) |>
    pivot_wider(names_from = Category, values_from = N:delta) |>
    map_dfr(unlist) |>
    rowid_to_column('iNum')

  names(dfDIF) <- gsub("\\_x", "\\.x", names(dfDIF))
  names(dfDIF) <- gsub("\\_y", "\\.y", names(dfDIF))

  dfIndice <- left_join(
      labs,
      dfDIF |>
      select(1, 2, 4, 6, 3, 5, 7),
      by='iNum'
    ) |>
    select(-iNum)

  dfError <- readxl::read_xls(
      paste0(folder, '/', test, '_shw.xls'),
      sheet='ResponseModel',
      skip=8+n_item+22,
      n_max=n_item*2+1,
      .name_repair="unique_quiet",
      col_types='numeric'
    ) |>
    select(item, error=`ERROR^`) |>
    na.omit() |>
    mutate(Category=rep(c('error.x', 'error.y'), n_item)) |>
    pivot_wider(names_from = 'Category', values_from = 'error') |>
    map_dfr(unlist)

  dfDelta <- left_join(
      labs,
      left_join(
        dfDIF |>
        select(iNum, delta.x, delta.y),
        dfError,
        by=c('iNum'='item')
      ),
      by='iNum'
    ) |>
    select(-iNum)

  # record items with missing delta
  id_na <- which(apply(dfDelta, 1, function(x) any(is.na(x[2:5]))))
  if (length(id_na)){
    write.csv(
      dfDelta[id_na, ],
      paste0('DIF/', DIFVar, '_', test, '_iRemoved', '.csv'),
      row.names=FALSE
    )
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
