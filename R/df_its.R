#' df_its
#'
#' This function extracts parameter estimates from its.txt file. This is
#' associated with test named 'test'.
#'
#' @param test Name of test.
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' @return Dataframe of item statistics such as N, facility, and discrimination.
#' @examples
#' df_its(test='Bang_3')
#' @export

df_its <- function(test, run){
    options(warn=-1)

    readxl::read_xls(
      paste0('calibration/', run, '/', test, '_its.xls'),
      skip=5,
      n_max=N_item2(run, test)
    ) |>
    rowid_to_column('iNum') |>
    left_join(
      read.table(paste0('data/', run, '/', test, '_Labels.txt')) |>
        rowid_to_column('iNum') |>
        dplyr::rename(Label=V1),
      by = 'iNum'
    ) |>
    dplyr::select(
      iNum, Label, N, Facil=Facility,
      Discr=`Item-Rest Cor`, Fitw=`Wghtd MNSQ`
    ) |>
    # dplyr::filter(Fitw!='NA') |>
    modify_at(5:6, function(x) round(as.numeric(x), digits=3))
}
