#' anchor_getLab
#'
#' This function puts dataframe read from anchor file to dataframe of columns of item
#' label, delta, and step number (if existent).
#'
#' @param folder Folder to extract anchor dataframe, either 'input' or 'output'.
#' @param test Name of test in 'output' folder.
#' @export

anchor_getLab <- function(folder, test) {
  iN <- N_item('output', test)
  dfRead <- readr::read_fwf(
      paste0(folder, '/', test, '_anc.txt'),
      fwf_widths(
        c(8, 10, 50),
        c("iNum", "Delta", "paramLab")
      ),
      show_col_types=FALSE
    ) |>
    mutate(
      paramLab = stringr::str_remove_all(paramLab, "[/*]"),
      paramLab = stringr::str_remove_all(paramLab, "category|item"),
      paramLab = stringr::str_squish(paramLab),
      Item = stringr::word(paramLab, 1) |> str_trim("both"),
      Step = stringr::str_remove(paramLab, Item) |> str_trim("both"),
      Delta=as.numeric(Delta)
    )

  extra <- nrow(dfRead) - iN

  if (extra > 0) {
    dfRead |>
      dplyr::select(Item, Delta, Step)
  } else {
    dfRead |>
      dplyr::select(Item, Delta)
  }
}
