#' df_shw_Step
#'
#' This function extracts step parameter estimates from .shw file. This is
#' associated with test named 'test'.
#'
#' @param folder Folder where xxx_shw.txt file is located.
#' @param test Name of test.
#' @examples
#' df_shw_Step(test='elana_poly_score')
#' @export

df_shw_Step <- function(folder='output', test){
  steps_shw <- read_fwf(
    Path(folder, test, 'shw'),
    fwf_cols(
      iNum=c(2, 5),
      item=c(6, 18),
      step=c(19, 35),
      error=c(46, 54)
    ),
    skip = Lines(folder, test, 'shw', 'TERM 2: item\\*step')[[2]] + 5,
    n_max = N_item_Step(folder, test),
    show_col_types = FALSE
  )

  labs <- read.table(paste0('data/', test, '_Labels.txt')) %>%
    rowid_to_column('iNum') %>%
    dplyr::rename(Label=V1)

  steps_shw %>%
    left_join(labs, by='iNum') %>%
    dplyr::select(-iNum, -item) %>%
    unite('iStep', c('Label', 'step'), sep='_')
}
