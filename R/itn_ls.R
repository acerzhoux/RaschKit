#' itn_ls
#'
#' This function extract item analysis results for each item from xxx_itn.txt file
#' and put them into a list. This is associated with test named 'test'.
#'
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' @param test Name of test.
#' @export

itn_ls <- function(run, test){
  folder_output <- file.path('calibration', run)
  a <- Lines(folder_output, test, 'itn', 'Label  ')
  b <- Lines(folder_output, test, 'itn', '==========')[-c(1, 2, N_item2(run, test)+3)]
  nn_opt <- (b-1) - (a+2) + 1

  blocks <- map2(
    a+3, b-1,
    ~str_file(folder_output, test, 'itn')[.x:.y]
  )

  map(
    blocks,
    ~readr::read_fwf(
      I(.x),
      fwf_cols(
        Label   = c(2, 6),
        Score   = c(11, 15),
        Count   = c(21, 28),
        Percent = c(29, 36),
        PtBis   = c(39, 44),
        Tau     = c(47, 52),
        P       = c(54, 57),
        PV1     = c(59, 67),
        SD      = c(68, 75)
      ),
      show_col_types = FALSE
    )
  )
}
