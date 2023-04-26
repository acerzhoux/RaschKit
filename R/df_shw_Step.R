#' df_shw_Step
#'
#' This function extracts step parameter estimates from .shw file. This is
#' associated with test named 'test'.
#'
#' @param folder Folder where xxx_shw.txt file is located.
#' @param test Name of test.
#' @examples
#' df_shw_Step(test='ELNA')
#' @export

df_shw_Step <- function(folder='output', test){
  labs <- read.table(paste0('data/', test, '_Labels.txt')) |>
    rowid_to_column('item') |>
    dplyr::rename(Label = V1)

  # #### solve line difference issue arising from CQ versions
  n_item <- N_item(folder, test)
  lnT2 <- Lines(folder, test, 'shw', 'TERM 2: ')
  nSkip <- ifelse(length(lnT2)==1, 7+n_item+8, 8+n_item+9)
  # #### End

  n_max <- {Lines(folder, test, 'shw', 'An asterisk ')[2] - 2} -
    {lnT2 + 6} + 1

  readxl::read_xls(
    paste0(folder, '/', test, '_shw.xls'),
    sheet='ResponseModel',
    skip=nSkip,
    n_max=n_max+1,
    .name_repair = "unique_quiet"
  ) |>
  select(
    item,
    step=`...4`,
    error=`ERROR^`
  ) |>
  dplyr::filter(!is.na(item), !is.na(error)) |>
  left_join(labs, by='item') |>
  unite('item', c('Label', 'step'), sep='_')
}
