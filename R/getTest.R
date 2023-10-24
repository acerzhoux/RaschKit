#' getTest
#'
#' This function extracts reliabilities from xxx_shw.xls files and puts them into
#' a dataframe.
#'
#' @param tests Vector of test names.
#' @param intLst List of item analysis dataframe for each test.
#' @examples
#' # Not run
#' # getTest(tests=c('V', 'Q'))
#' @export

getTest <- function(tests, intLst){
  getAlpha <- function(test){
    strs <- readxl::read_xls(
      paste0('output/', test, '_its.xls'),
      .name_repair = "unique_quiet"
    )[[1]] |>
    na.omit()

    tibble(
      type='Coefficient Alpha',
      val=parse_number(strs[str_detect(strs, 'Coefficient Alpha')]),
      Test=test
    )
  }

  testStats <- map(
    tests,
    ~readxl::read_xls(
      paste0('output/', .x, '_shw.xls'),
      'Reliabilities',
      .name_repair = "unique_quiet"
    ) |>
      dplyr::mutate(
        type=str_squish(str_sub(`...1`, 1, 34)),
        val=str_squish(str_sub(`...1`, 36, 50))
      ) |>
      dplyr::filter(!(val %in% c('', 'Unavailable')))
    ) |>
    map2(tests, ~dplyr::mutate(.x, Test=.y) |> dplyr::select(-`...1`)) |>
    reduce(bind_rows) |>
    mutate(val=as.numeric(val)) |>
    bind_rows(
      map(tests, getAlpha) |>
        reduce(bind_rows)
    ) |>
    mutate(val=as.numeric(val))  |>
      pivot_wider(names_from = 'type', values_from = 'val') |>
      modify_if(is.numeric, ~round(.x, 3)) |>
    left_join(
      map(
        intLst,
        ~summarise(
          .x,
          `Average Facility`=mean(Facility, na.rm=TRUE),
          `Average Item-Rest Corr.`=mean(`Item-Rest Corr.`, na.rm=TRUE)
        )
      ) |>
      imap_dfr(~mutate(.x, Test=.y)),
      by='Test'
    )

  names(testStats) <- gsub("\\:", '', names(testStats))

  testStats
}
