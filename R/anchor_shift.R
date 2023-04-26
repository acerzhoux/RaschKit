#' anchor_shift
#'
#' This function adds anchor shift to item deltas in Test B (in 'output' folder),
#' keeps step estimates (if any) intact, and outputs shifted deltas in 'input' folder.
#'  This is associated with test named 'test' (Test B).
#'
#' @param test Name of test.
#' @param shift Mean of non-DIF anchor deltas in Test A minus that in Test B.
#' @export

anchor_shift <- function(test, shift){
  iN <- N_item('output', test)

  readr::read_fwf(
      paste0('output/', test, '_anc.txt'),
      fwf_widths(
        c(8, 10, 50),
        c("iNum", "delta", "label")
      ),
      show_col_types=FALSE
    ) |>
    mutate(
      delta=case_when(
        iNum<=iN ~ delta+shift,
        TRUE ~ delta
      ),
      delta=round(delta, 3)
    ) |>
    write_fwf(paste0('input/', test, '_anc.txt'))
}
