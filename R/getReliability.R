#' getReliability
#'
#' This function extracts reliabilities from xxx_shw.xls files and puts them into
#' a dataframe.
#'
#' @param tests Vector of test names.
#' @export

getReliability <- function(tests){
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

  Reliabilities <- map(
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
    mutate(val=as.numeric(val)) |>
    pivot_wider(names_from = 'type', values_from = 'val') |>
    modify_if(is.numeric, ~round(.x, 3))

  names(Reliabilities) <- gsub("\\:", '', names(Reliabilities))

  Reliabilities
}
