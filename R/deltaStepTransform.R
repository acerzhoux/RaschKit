#' deltaStepTransform
#'
#' This function transforms step Tau estimates to deltas by adding each item
#' estimate to that item's step Tau estimates. For quick method, item deltas are
#' first adjusted by their mean to center at 0.
#'
#' @param test Name of test.
#' @return Dataframe of variables 'item', 'delta', 'error'.
#' @examples
#' deltaStepTransform('WA')
#' @export

deltaStepTransform <- function(test){
  folder <- 'output'

  labs <- read.table(paste0('data/', test, '_Labels.txt')) |>
    rowid_to_column('qOrder') |>
    rename(qid=V1)

  # indexes
  n_item <- N_item(folder, test)

  file_shw <- Path(folder, test, 'shw')
  y <- readLines(file_shw)
  ind1 <- grep('TERM 2\\: item\\*step', y)[[1]]+5
  ind2 <- grep('An asterisk next to', y)[[4]]-2

  # deltas
  x <- str_file(folder, test, 'itn')
  ind <- grep('Item Delta\\(s\\)', x)
  deltas <- map(
    x[ind],
    ~str_sub(.x, 15, -1) |>
      str_replace_all('-', ' -') |>
      str_squish() |>
      str_split(' ')
    ) |>
    map(1) |>
    map(as.numeric)

  deltas.centered <- map(deltas, ~.x - mean(map_dbl(deltas, mean))) |>
    unlist()

  # locate skip rows
  shw.1 <- readxl::read_xls(
    paste0(folder, '/', test, '_shw.xls'),
    sheet='ResponseModel',
    .name_repair="unique_quiet"
  ) |>
    select(a=1)
  skips <- which(shw.1$a=='item')

  # step error: same as delta error
  steps <- readxl::read_xls(
      paste0(folder, '/', test, '_shw.xls'),
      sheet='ResponseModel',
      skip=skips[[2]],
      n_max=ind2-ind1+1,
      .name_repair="unique_quiet",
      col_types='numeric'
    ) |>
    select(
      qOrder=item,
      cat=`...4`,
      error=`ERROR^`
    ) |>
    filter(!is.na(qOrder), cat!=0) |>
    group_by(qOrder) |>
    mutate(error=ifelse(is.na(error), error[1], error)) |>
    ungroup()

  # merge delta and error, add labels
  steps |>
    cbind(tibble(delta=deltas.centered)) |>
    left_join(
      labs,
      by='qOrder'
    ) |>
    mutate(item=paste0(qid, cat)) |>
    select(item, delta, error)
}
