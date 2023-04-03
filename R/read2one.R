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
#' # read2one('DIF', 'Writing', difVarVec=c('gender', 'LBOTE', 'VCEVCAL'))
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
  names(ex_ls) <- difVarVec

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
    names(ex_ls) <- tests
    if (prefix=='itn'){
      # add hyperlink, color, format #######
      add_format()[['itn']](ex_ls, file, folder, prefix)
    } else {
      move_into_folder(folder, prefix)
      ex_ls |>
        writexl::write_xlsx(file)
    }
    cat('\nFiles combined to:\n\t', file)
  }
}
