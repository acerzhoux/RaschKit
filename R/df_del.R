#' df_del
#'
#' This function extracts step parameter estimates from .del file. This is
#' associated with test named 'test'.
#'
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' @param test Name of test.
#' @return Dataframe of item name_step and delta.
#' @examples
#' df_del(test='elana_math_NSW')
#' df_del(test='elana_math_NSW', long_label=TRUE)
#' @export

df_del <- function(run, test){

  folder <- file.path('calibration', run)

  items_del <- str_file(folder, test, 'del') |>
    as_tibble() |>
    mutate(tabbed = str_detect(value, '\\t')) |>
    dplyr::filter(tabbed) |>
    dplyr::select(-tabbed) |>
    separate(
      value,
      into = c("iNum_step", 'delta', "temp1", "temp2", "temp3", "iLab", 'temp4'),
      sep = "\\t"
    ) |>
    dplyr::select(-contains("temp")) |>
    separate(
      iNum_step,
      into = c("iNum", "step"),
      sep = "\\."
    ) |>
    mutate_at(c("iNum", "step", "delta"), as.numeric)

  labs <- read.table(paste0('data/', test, '_Labels.txt')) |>
    rowid_to_column('iNum') |>
    dplyr::rename(Label=V1)

  items_del |>
    left_join(labs, by='iNum') |>
    dplyr::select(-iNum, -iLab, Label) |>
    unite('item', c('Label', 'step'), sep='_')
}
