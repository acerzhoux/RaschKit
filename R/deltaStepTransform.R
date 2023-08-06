#' deltaStepTransform
#'
#' This function transforms step Tau estimates to deltas by adding each item
#' estimate to that item's step Tau estimates. For quick method, item deltas are
#' first adjusted by their mean to center at 0.
#'
#' @param test Name of test.
#' @param quick Whether quick error is needed. Default is TRUE.
#' @return Dataframe of variables 'item', 'delta', 'error'.
#' @examples
#' deltaStepTransform('WA')
#' @export

deltaStepTransform <- function(test, quick=TRUE){
  folder <- 'output'

  # indexes
  n_item <- N_item(folder, test)

  file_shw <- Path(folder, test, 'shw')
  y <- readLines(file_shw)
  ind1 <- grep('TERM 2\\: item\\*step', y)[[1]]+5
  ind2 <- grep('An asterisk next to', y)[[4]]-2

  labs <- read.table(paste0('data/', test, '_Labels.txt')) |>
    rowid_to_column('qOrder') |>
    rename(qid=V1)

  # item deltas
  deltas <- readxl::read_xls(
      paste0(folder, '/', test, '_shw.xls'),
      sheet='ResponseModel',
      skip=5,
      n_max=2+n_item,
      .name_repair="unique_quiet",
      col_types='numeric'
    ) |>
    select(
      qOrder=item,
      delta=ESTIMATE
    ) |>
    filter(!is.na(delta))
  if (quick) {
    deltas <- deltas |>
      mutate(delta=delta-mean(delta))
  }

  # step deltas
  steps <- readxl::read_xls(
      paste0(folder, '/', test, '_shw.xls'),
      sheet='ResponseModel',
      skip=7+n_item+8,
      n_max=ind2-ind1+1,
      .name_repair="unique_quiet",
      col_types='numeric'
    ) |>
    select(
      qOrder=item,
      cat=`...4`,
      delta=ESTIMATE,
      error=`ERROR^`
    ) |>
    filter(!is.na(qOrder), cat!=0)

  # adjust step taus by item delta
  split(steps, list(steps$qOrder)) |>
    # use Step 1 error as last step error
    map(~mutate(.x, error=ifelse(is.na(error), error[1], error))) |>
    map2(deltas$delta, ~mutate(.x, delta=delta+.y)) |>
    reduce(bind_rows) |>
    left_join(
      labs,
      by='qOrder'
    ) |>
    mutate(item=paste0(qid, cat)) |>
    select(item, delta, error)
}
