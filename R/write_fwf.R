#' write_fwf
#'
#' This function inserts seven spaces between columns of dataframe 'dat' and
#' write the final string to a text file with path 'file'. This is usually
#' used to process anchor files.
#'
#' @param dat Dataframe which is usually read from anchor file in 'output' folder.
#' @param file Path of processed anchor file to be saved, usually in 'input' folder.
#' @export

write_fwf <- function(dat, file) {
  strCol <- NULL
  for (i in 1:nrow(dat)) {
    strRow <- dat[[i, 1]]
    for (j in 2:ncol(dat)) {
      strRow <- paste0(
        strRow,
        paste0(rep(' ', 7), collapse = ""),
        dat[[i, j]],
        collapse = ""
      )
    }
    strCol <- c(strCol, strRow)
  }
  writeLines(strCol, file)
}
