#' calibrate_lst
#'
#' This function calibrates a list of tests and put results into a summary file
#' in 'results' folder with 'itn' and #run in name.
#'
#' @param respDfLst List of response dataframe. Default is NULL (reruns after review).
#' If NULL and items need collapsing, put processed data with test name in
#' 'data' folder before running.
#' @param keyDfLst List of key dataframes. Element names are test names.
#' @param pid Name of candidates' ID variable.
#' @param n_cov Number of covariates before responses.
#' @param run Integer that indicates round of run.
#'
#' @export

calibrate_lst <- function(respDfLst=NULL, keyDfLst, pid, n_cov, run){
  testVec <- names(keyDfLst)

  for (test in testVec){
    if (is.null(respDfLst)) {
      respDf <- NULL
    } else {
      respDf <- respDfLst[[test]]
    }
    calibrate(test, respDf, keyDfLst[[test]], pid, n_cov)
  }

  read2one('results', testVec, 'itn', pasts0('Run_', run))
}
