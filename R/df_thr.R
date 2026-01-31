#' df_thr
#'
#' This function extracts step parameter estimates from xxx_thr.txt file. This is
#' associated with test named 'test'.
#'
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' @param folder Folder where ConQuest output files are located.
#' @param test Name of test.
#' @return Dataframe of step parameter estimates.
#' @examples
#' df_thr(test='FPA')
#' df_thr(test='FPA', long_label=TRUE)
#' @export

df_thr <- function(run, folder, test){
  items_thr <- str_file(folder, test, 'thr') |>
    as_tibble() |>
    mutate(tabbed = str_detect(value, '\\t')) |>
    dplyr::filter(tabbed) |>
    dplyr::select(-tabbed) |>
    separate(
      value,
      into = c("iNum", "threshold", "temp1", "temp2", "iNum2", "iLab", "temp3"),
      sep = "\\t"
    ) |>
    dplyr::select(-contains("temp"), -iNum2) |>
    separate(
      iNum,
      into = c("iNum", "category"),
      sep = "\\."
    ) |>
    mutate_at(c("iNum", "category", "threshold"), as.numeric)

  labs <- read.table(paste0('data/', run, '/', test, '_Labels.txt')) |>
    rowid_to_column('iNum') |>
    dplyr::rename(Label = V1)

  items_thr |>
    left_join(labs, by='iNum') |>
    select(-iLab, iLab=Label)
}
