#' item_stats
#'
#' This function extracts item statistics such as item-rest correlation,
#' facility, delta, and case number from 'xxx_its.txt' and 'xxx_shw.txt' files
#' and merge them into one dataframe. This is associated
#' with test named 'test'.
#'
#' @param folder Folder where ConQuest output files are located.
#' @param test Name of test. Default is 'output'.
#' @examples
#' a <- item_stats(test='math_35')
#' @export

item_stats <- function(folder='outoput', test){
  labs <- read.table(paste0('data/', test, '_Labels.txt')) |>
    rowid_to_column('seqNo') |>
    dplyr::rename(`Item Title`=V1) |>
    mutate(seqNo=as.integer(seqNo))

  # stats from shw file
  iShw <- labs |>
    left_join(
      readxl::read_xls(
        paste0(folder, '/', test, '_shw.xls'),
        sheet='ResponseModel',
        skip=6,
        n_max=N_item('output', test)+1,
        .name_repair = "unique_quiet"
      ) |>
      select(1, 3:5, 9:12) |>
      `names<-`(c('seqNo', 'Estimate', 'Item Error', 'Outfit', 'Infit',
                  'C.I. (L)', 'C.I. (H)', 'T')) |>
      dplyr::filter(!is.na(Estimate)),
      by='seqNo'
    ) |>
    as_tibble()

  mDelta <- mean(iShw$Estimate, na.rm=TRUE)
  if (abs(mDelta) > 0.001){
    iShw <- iShw |>
      mutate(`Item Estimate (item centred)`=round(Estimate-mDelta, 3)) |>
      rename(`Item Estimate (case centred)`=Estimate)
  } else {
    iShw <- iShw |>
      rename(`Item Estimate (item centred)`=Estimate)
  }

  # stats from its file
  iIts <- readxl::read_xls(
      paste0(folder, '/', test, '_its.xls'),
      skip=5,
      n_max=N_item2('output', test),
      .name_repair="unique_quiet",
      col_types='numeric'
    ) |>
    rowid_to_column('seqNo') |>
    select(-c(2, 7, 8)) |>
    `names<-`(c('seqNo', 'Number of Students', 'Facility',
                'Item-Rest Corr.', 'Item-Total Corr.')) |>
    as_tibble() |>
    modify_at(3:5, ~round(.x, 2))

  iIts |>
    left_join(
      iShw,
      by='seqNo'
    )

}
