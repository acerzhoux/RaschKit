#' itn_ls
#'
#' This function extract item analysis results for each item from xxx_itn.txt file
#' and put them into a list. This is associated with test named 'test'.
#'
#' @param test Name of test.
#' @export

itn_ls <- function(test){
  a <- Lines('output', test, 'itn', 'Label  ')
  b <- Lines('output', test, 'itn', '==========')[-c(1, 2, N_item2('output', test)+3)]
  nn_opt <- (b-1) - (a+2) + 1

  map2(
    a+2, b-1,
    ~str_file('output', test, 'itn')[.x:.y]
  ) |>
  map2(
    nn_opt,
    ~readr::read_fwf(
      I(.x),
      fwf_cols(
        Label = c(2, 6),
        Score = c(11, 15),
        Count = c(21, 28),
        Percent = c(29, 36),
        PtBis = c(39, 44),
        Tau = c(47,52),
        P = c(54, 57),
        PV1 = c(59, 67),
        SD = c(68, 75)
      ),
      skip = 0,
      n_max = .y,
      show_col_types = FALSE
    )
  )
}
