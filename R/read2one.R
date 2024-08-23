#' read2one
#'
#' This function reads from 'folder' files associated with 'tests'
#' and puts them into one file.
#'
#' @param folder Folder where to-be-read files are located. Must be one of
#' c('results', 'DIF', 'equating').
#' @param tests Vector of test names after '_'.
#' @param prefix Common prefix in file names before first '_'. Default is NULL.
#' @param file_name Name given to new file, each sheet of which is one test's result.
#' Default is NULL.
#' @param difVarVec Vector of DIF variables used to extract results from folders
#' named as elements in the vector.
#' Its order corresponds to the alphabetic/numeric order of DIF variables'
#' two categories in data.
#' @examples
#' # Not run
#' # read2one('results', c('AACA'), 'itn')
#' @export

read2one <- function (folder = c('results', 'DIF', 'equating'), tests,
                      prefix=NULL, file_name = NULL, difVarVec=NULL) {
  # Excel names to read files from
  if (folder=='DIF') {
    files <- file.path('DIF', difVarVec, paste0(tests, '_process.xlsx'))
    prefix <- tests
  } else {
    files <- file.path(folder, str_c(prefix, '_', tests, '.xlsx'))
  }

  # sheet name to read from
  sheetNm <- ifelse(folder %in% c('DIF', 'equating'), 'final', 1)
  ex_ls <- map(files, ~readxl::read_xlsx(.x, sheetNm))
  if (folder=='DIF') {
    names(ex_ls) <- difVarVec
  } else {
    names(ex_ls) <- tests
  }

  # Excel name to save files into one
  if (!is.null(file_name)) {
    file <- ifelse(
      is.null(prefix),
      file.path(folder, str_c(file_name, '.xlsx')),
      file.path(folder, str_c(prefix, '_', file_name, '.xlsx'))
    )
  } else {
    file <- file.path(folder, str_c(prefix, ".xlsx"))
  }

  # equating, DIF
  if (sheetNm == 'final'){
    ex_ls |>
      writexl::write_xlsx(file)
    cat('\nFiles combined to:\n\t', file)
  }

  # itn
  if (sheetNm == 1){
    if (prefix=='itn'){
      # add summary and flag sheets
      itnSums <- map(ex_ls, ~dplyr::filter(.x, Priority %in% 1:4)) |>
        keep(~ nrow(.x) != 0)
      Flagged <- itnSums[lengths(itnSums) > 0L] |>
        map(~mutate(.x, Key=as.character(Key))) |>
        reduce(bind_rows) |>
        select(-ncol(itnSums[[1]]))

      ls_save <- list(
          TestStats=getTest(tests, ex_ls),
          Note=itn_comment(),
          Flagged=Flagged |>
            mutate(ICC=paste0('=HYPERLINK(', Test, '!U$2, "ICC")'))
        ) |>
        append(
          map(ex_ls, ~mutate(.x, ICC='=HYPERLINK(U$2, "ICC")'))
        )

      # move combined files into a folder named prefix before saving
      move_into_folder(folder, prefix)

      add_format()[['itn']](ls_save, file, folder, prefix) # hyperlink, color, format

    } else if (prefix=='eqv') {
      move_into_folder(folder, prefix)

      if (ncol(ex_ls[[1]])==3) {
        cls <- c('Est_', 'SE_')
      } else {
        cls <- c('Est_', 'SE_', 'Scale_')
      }

      list(
        allLong=ex_ls |>
          imap(~mutate(.x, Test=.y)) |>
          reduce(bind_rows) |>
          modify_if(is.numeric, ~round(.x, 3)),
        allWide=ex_ls |>
          reduce(full_join, by='Score_raw') |>
          arrange(Score_raw) |>
          modify_if(is.numeric, ~round(.x, 3)) |>
          `names<-`(c('Score_raw', map(tests, ~paste0(cls, .x)) |> unlist()))
      ) |>
      append(ex_ls) |>
      writexl::write_xlsx(file)

    } else {
      move_into_folder(folder, prefix)
      ex_ls |>
        writexl::write_xlsx(file)
    }
    cat('\nFiles combined to:\n\t', file)
  }
}
