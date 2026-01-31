#' labels_into_Data
#'
#' This function saves raw label file 'test_Labels.txt' into 'data' folder.
#' Later, this saved file is used to produce label file 'test.lab' in 'input'
#' folder. This is associated with test named 'test'.

#' @param test Name of test.
#' @param labels Vector of item labels in the test, e.g.,
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' c('iTrack3984232', 'iTrack4890298')
#' @export

labels_into_Data <- function(test, labels, run){
    write_lines(labels, paste0('data/', run, '/', test, "_Labels.txt"))
}
