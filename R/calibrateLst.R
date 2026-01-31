#' calibrateLst
#'
#' This function calibrates a list of tests and put results into a summary file
#' in 'results' folder with 'itn' and run number in name. Make sure elements in keyDfLst
#' and respDfLst (if not NULL) in same order correspond to one test.
#' Also, keyDfLst should have test name for each element.
#'
#' @param respDfLst List of response dataframe. Default is NULL (reruns after review).
#' If NULL and items need collapsing, put processed data with test name in
#' 'data' folder before running.
#' @param keyDfLst List of key dataframes. Element names are test names.
#' @param pid Name of candidates' ID variable.
#' @param n_cov Number of covariates before responses.
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' @param useR TRUE when code 'R' is used for scoring. Default is FALSE.
#' @param quick TRUE if empirical error is not needed. Default is TRUE.
#'
#' @export

calibrateLst <- function(respDfLst=NULL, keyDfLst, pid, n_cov, run, useR=FALSE, quick=TRUE){

  sink_start()

  map(
    paste0(c('data', 'input', 'calibration'), '/', run),
    dir.create
  )

  tests <- names(keyDfLst)
  for (i in seq_along(tests)){
    test <- tests[[i]]
    if (is.null(respDfLst)) {
      respDf <- NULL
    } else {
      respDf <- respDfLst[[i]]
    }
    calibrate(test, respDf, keyDfLst[[test]], pid, n_cov, useR=useR, quick=quick, run=run)
  }

  read2one(file.path('calibration', run), tests, 'itn', paste0('Run_', run))

  # Stop sinking
  sink_stop()
}
