#' calibrateScale
#'
#' This function anchors a list of tests. Make sure elements in keyDfLst,
#' ancShiftLst (if not NULL), and ancDfLst (if not NULL) in same order
#' correspond to one test. Also, keyDfLst should have test name for each element.
#'
#' @param keyDfLst List of key dataframes. Element names are test names.
#' @param pid Name of candidates' ID variable.
#' @param n_cov Number of covariates before responses.
#' @param est_type Type of ability estimate to use for score equivalence table,
#' 'wle' or 'mle'. 'wle' is commonly used. Default is 'wle'.
#' @param trial TRUE when trial item diagnostics is needed after anchoring is done.
#' Default is FALSE.
#' @param ancShiftLst List of shift of mean anchor delta from previous cycle obtained from
#' equating analysis. It will be added to each item delta of 'test' in 'output'
#' folder and put in 'input' folder as anchor file for 'test'. Default is NULL.
#' @param ancTest2ReadLst List of name of test in 'output' folder to read delta from. This
#' is usually concurrent calibration results such as all grades and/or all test
#' forms. Only delta's (and step estimates) in 'tests' are extracted and put in
#' 'input' folder as anchor file. Default is NULL.
#' @param ancDfLst List of dataframe of 'Item' and 'Delta' for anchors. Include 'Step' column
#' for step estimates (step number). Default is NULL. If all of ancShift, ancTest2Read,
#' and ancDf are NULL, an xxx_anc.txt file with
#' anchor tbl (Item, Delta) should be put in 'input' folder beforehand.
#' Check output 'xxx_anc.txt' file from previous run for correct anchor order,
#' especially for polytomous items with step parameters.
#' @param slope Slope to multiply ability estimates. Default is NULL
#' @param intercept Value/intercept to add to ability estimates. Default is NULL.
#' @param extrapolation Whether to extrapolate the minimum and maximum estimates.
#' Default is FALSE.
#' @export

calibrateScale <- function(keyDfLst, pid, n_cov, est_type='wle', trial=FALSE,
                           ancShiftLst=NULL, ancTest2ReadLst=NULL, ancDfLst=NULL,
                           slope=NULL, intercept=NULL, extrapolation=FALSE){
  testVec <- names(keyDfLst)

  # process arguments
  n <- length(keyDfLst)
  if (!is.null(ancShiftLst)) {
    ancTest2ReadLst <- ancDfLst <- rep(list(NULL), n)
  } else if (!is.null(ancTest2ReadLst)) {
    ancShiftLst <- ancDfLst <- rep(list(NULL), n)
  } else if (!is.null(ancDfLst)) {
    ancTest2ReadLst <- ancShiftLst <- rep(list(NULL), n)
  }

  # anchor tests in testVec
  for (i in seq_along(keyDfLst)){
    calibrate(testVec[[i]], NULL, keyDfLst[[i]], pid, n_cov, trial=trial,
              ancShift=ancShiftLst[[i]], ancTest2Read=ancTest2ReadLst[[i]],
              ancDf=ancDfLst[[i]], est_type=est_type,
              slope=slope, intercept=intercept, extrapolation=extrapolation)
  }

  # read into one file
  if (!is.null(est_type)) {
    read2one('results', testVec, 'eqv')
  }
  if (trial) {
    read2one('results', testVec, 'itn', 'trial')
  }
}
