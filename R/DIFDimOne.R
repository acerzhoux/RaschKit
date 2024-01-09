#' DIFDimOne
#'
#' This function can perform DIF analysis on either dichotomous or polytomous
#' variable. This is associated with test named 'test'.
#'
#' Should preprocess data in three ways. First, convert non-used categories and
#' missing values to NA. Second, remove item responses with all missing values.
#' Third, remove items that have all missing values or all correct/incorrect
#' responses on any category of the DIF variable.
#'
#' @param method One of 'chi_square', 'Bonferroni', or 'Facet'.
#' @param DIFVar Name of DIF variable. Should be lowercase for ConQuest to run.
#' Default is NULL.
#' @param vars Vector of length 2 such as c('girls','boys'). Its order corresponds
#' to the alphabetic/numeric order of DIF variables' two categories in data.
#' @param poly_facet TRUE if facet model is to be run on a polytomous DIF variable.
#' Default is FALSE.
#' @param test Name of the test.
#' @param respDf Dataframe with pid, covariables (e.g,, DIF variable), and responses.
#' Default is NULL where Excel file with name 'test' in 'data' folder is used.
#' @param filetype Format for input dataset. Default is 'sav'. Also support
#' csv format.
#' @param keyDf Dataframe of 'Item', 'Key', and 'Max_score' (add Key2 if double key).
#' @param pid Name of candidates' ID variable.
#' @param n_cov Number of covariates before responses.
#' @param n_resp Number of items. Default is NULL.
#' @param regrNmVec Vector of character regressors' names.
#' @param quick Whether quick error is needed. Default is TRUE for DIF analysis.
#' @param section_extr Extra sections to be added to .cqc file in 'input' folder.
#' Default is NULL.
#' @param prep_process TRUE if it is needed to remove items without data on
#' some categories of DIF variable. Default is FALSE.
#' @param save_xlsx Whether to save summary file and plots. Default is TRUE
#' (one DIF variable).
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between
#' two tests. Default is 0.5.
#' @param DIF_std_cut Threshold of an item's standardized delta estimate difference
#' between two tests. Default is 4.
#' @param design_effect Value to adjust errors. Default is 1.
#' @param iter TRUE to iteratively remove DIF items. Default is FALSE
#' @param step TRUE if polytomous items are involved. Default is FALSE.
#' @param pweight Variable name of person weights in response dataframe. Should
#' be specified if weight is used for modeling. Default is NULL.
#' @export

DIFDimOne <- function(method=c('chi_square', 'Bonferroni', 'Facet'),
                        test, pid, n_cov, DIFVar, respDf=NULL, filetype='sav', keyDf,
                        vars=NULL, poly_facet=FALSE, n_resp=NULL, regrNmVec=NULL,
                        quick=TRUE, section_extr=NULL, prep_process=FALSE, save_xlsx=TRUE,
                        p_cut=0.05, DIF_cut=0.5, DIF_std_cut=4, iter=FALSE,
                        step=FALSE, design_effect=1, pweight=NULL){
  # ####### check inputs
  cat('Checking inputs...\n')
  if (length(method)!=1 || !(method %in% c('chi_square', 'Bonferroni', 'Facet'))) {
    stop('Please set \'method\' as one of \'chi_square\', \'Bonferroni\', or \'Facet\'.')
  }

  # read data
  if (is.null(respDf)) {
    cat('Reading data...\n')
    if (filetype == 'sav') {
      respDf <- haven::read_sav(paste0('data/', test, '.sav'))
      label_csv <- paste0('data/', test, '_labels.csv')
      if (file.exists(label_csv)){
        nms <- read.csv(label_csv)$iLabel
        names(respDf) <- nms
      }
    } else if (filetype == 'csv') {
      respDf <- read.csv(paste0('data/', test, '.csv'))
    } else {
      stop('respDf must use sav or csv.')
    }
  }

  if (!all(c(pid, regrNmVec) %in% names(respDf))) {
    stop('Pid or regressor is not in data column names!')
  }

  poly_key <- ifelse(any(keyDf$Max_score > 1), TRUE, FALSE)
  labels <- keyDf$Item

  # calculate arguments
  cat('Using default arguments if not given...\n')
  if (is.null(n_resp)) n_resp <- ncol(respDf) - n_cov

  if (DIFVar %in% regrNmVec){
    print('Cannot use DIF variable as covariate! Will remove it...')
    regrNmVec <- setdiff(regrNmVec, DIFVar)
  }
  # change 'DIFVar' to all lower-case; otherwise, R cannot call CQC
  if (method=='Bonferroni'){
    if (DIFVar != tolower(DIFVar)){
      print(paste0('\'', DIFVar, '\'', ' changed to ', '\'', tolower(DIFVar), '\'',
             ' as ConQuestr requires.\n'))
      ID_DIFVar <- which(names(respDf) == DIFVar)
      names(respDf)[ID_DIFVar] <- tolower(DIFVar)
      DIFVar <- tolower(DIFVar)
    }
  }

  # ####### preprocess data
  if (poly_key){
    cat('Checking polytomou-score items; recode if score are not continuous...\n')
    strRec <- poly_recode(test, keyDf, respDf, n_cov, c('r','R','m','M','9','x','X','.','',' ',NA))
  }

  if (prep_process){
    cat('Checking and removing items without data on any category of DIF variable...\n')
    processed <- sparse_data_process(test=test, respDf=respDf, keyDf=keyDf, labels=labels,
                     n_cov=n_cov, nDimVec=n_resp, miss_code=c('.', 'r', 'R', 'x', 'X', '', ' '),
                     DIFVar=DIFVar)
    respDf <- processed[['respDf']]
    n_resp <- processed[['nDimVec']]
    keyDf <- processed[['keyDf']]
    labels <- processed[['labels']]
  }

  # ####### prepare arguments
  cat('Preparing ConQuest control file...\n')
  prep <- df_key_lab_args(test, respDf, pid, n_cov, n_resp, DIFVar,
                          regrNmVec, section_extr, labels, FALSE, pweight)

  arg_DIF <- list(method=method, anchor=FALSE, domain=NULL,
          section_extr=prep$section_extr, poly_key=poly_key,
          quick=quick, step=step, keyDf=keyDf,
          p_cut=p_cut, DIF_cut=DIF_cut, DIF_std_cut=DIF_std_cut,
          design_effect=design_effect,
          test=test, DIFVar=DIFVar,
          vars=vars, poly_facet=poly_facet, poly_group=FALSE,
          poly_catgrs=NULL, save_xlsx=save_xlsx, iter=iter,
          pweight=pweight) |>
    append(within(prep, rm(section_extr)))

  # ####### run models
  if (method=='chi_square'){
    do.call(DIF, arg_DIF)
  }
  if (method=='Bonferroni'){
    cats <- sort(unique(respDf[[DIFVar]]))
    cats <- cats[!is.na(cats)]
    arg_DIF[['poly_catgrs']] <- cats
    do.call(DIF, arg_DIF)
  }
  if (method=='Facet'){
    arg_DIF[['poly_facet']] <- TRUE
    do.call(DIF, arg_DIF)
  }
}
