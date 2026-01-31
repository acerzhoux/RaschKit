#' item_stats
#'
#' This function extracts item statistics such as item-rest correlation,
#' facility, delta, and case number from 'xxx_its.txt' and 'xxx_shw.txt' files
#' and merge them into one dataframe. This is associated
#' with test named 'test'.
#'
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' @param test Name of test.
#' @examples
#' a <- item_stats(folder='output', test='Hum', anchor=F)
#' @export

item_stats <- function(run, test){

  folder <- file.path('calibration', run)

  labs <- read.table(paste0('data/', run, '/', test, '_Labels.txt')) |>
    rowid_to_column('seqNo') |>
    dplyr::rename(`Item Title`=V1) |>
    mutate(seqNo=as.integer(seqNo))

  strings <- readxl::read_xls(
    paste0(folder,'/',test,'_shw.xls'),
    'ResponseModel',
    .name_repair="unique_quiet"
  ) |>
    select(1) |>
    unlist()

  skip <- grep("VARIABLES", strings) + 1

  # stats from shw file
  iShw0 <- readxl::read_xls(
    paste0(folder, '/', test, '_shw.xls'),
    sheet='ResponseModel',
    skip=skip,
    n_max=N_item(file.path('calibration', run), test)+1,
    .name_repair = "unique_quiet"
  ) |>
    dplyr::filter(!is.na(ESTIMATE))

  var.exc.1 <- which(str_detect(names(iShw0), '^C'))[[1]]
  var.exc.2 <- var.exc.1 + 2

  iShw <- labs |>
    left_join(
      iShw0 |>
        select(-c(var.exc.1:var.exc.2), -contains('...2')) |>
        `names<-`(c('seqNo', 'Estimate', 'Item Error', 'Outfit', 'Infit',
                    'C.I. (L)', 'C.I. (H)', 'T')) |>
        dplyr::filter(!is.na(Estimate)) |>
        dplyr::mutate(
          seqNo=sub(" .*", "", seqNo) |> as.integer(),
          `Item Error`=as.numeric(`Item Error`)
        ),
      by='seqNo'
    ) |>
    as_tibble() |>
    mutate(Estimate=as.numeric(Estimate))

  mDelta <- mean(iShw$Estimate, na.rm=TRUE)
  if (abs(mDelta) > 0.001){ # anchor; case-centered estimates
    iShw <- iShw |>
      mutate(`Item Estimate (item centred)`=round(Estimate-mDelta, 3)) |>
      dplyr::rename(`Item Estimate (case centred)`=Estimate)
  } else {
    iShw <- iShw |>
      dplyr::rename(`Item Estimate (item centred)`=Estimate)
  }

  # stats from its file
  iIts <- readxl::read_xls(
      paste0(folder, '/', test, '_its.xls'),
      skip=5,
      n_max=N_item2(run, test),
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
