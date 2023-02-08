#' keys_into_Data
#'
#' This function saves raw key file 'test_Key.txt' into 'data' folder. Later,
#' this saved file is used to produce the key section of 'test.cqc' file in
#' 'input' folder. This is associated with test named 'test'.

#' @param test Name of test.
#' @param keys Vector of item keys in the test, e.g., c('A', 'C', 'D')
#' @examples
#' keys_into_Data()
#' @export

keys_into_Data <- function(test, keys){
  if (length(keys) == 1){
    paste0(keys, collapse = '') |>
      write_lines(paste0('data/', test, "_Key.txt"))
  }
}
