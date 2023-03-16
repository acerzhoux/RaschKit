#' DIF_dim_one
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
#' @param data Dataframe with pid, covariables (e.g,, DIF variable), and responses.
#' Default is NULL where Excel file with name 'test' in 'data' folder is used.
#' @param filetype Format for input dataset. Default is 'sav'. Also support
#' csv format.
#' @param keys Dataframe of 'Item', 'Key', and 'Max_score' (add Key2 if double key).
#' @param pid Name of candidates' ID variable.
#' @param n_cov Number of covariates before responses.
#' @param n_resp Number of items. Default is NULL.
#' @param regr_vec_char Vector of character regressors' names.
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
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference
#' between two tests. Default is 4.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @param facil_cut Threshold of number of percent to flag an item with large
#' facility difference between two groups of test takers. Default is 10.
#' @param iterative TRUE to iteratively remove DIF items. Default is FALSE
#' @param step TRUE if polytomous items are involved. Default is FALSE.
#' @param pweight Variable name of person weights in response dataframe. Should
#' be specified if weight is used for modeling. Default is NULL.
#' @export

DIF_dim_one <- function(method=c('chi_square', 'Bonferroni', 'Facet'),
            test, pid, n_cov, DIFVar, data=NULL, filetype='sav', keys,
            vars=NULL, poly_facet=FALSE,
            n_resp=NULL, regr_vec_char=NULL,
            quick=TRUE, section_extr=NULL,
            prep_process=FALSE, save_xlsx=TRUE,
            p_cut=0.05, DIF_cut=0.5,
            DIF_adj_cut=4, facil_cut=10, iterative=FALSE,
            step=FALSE, desig_effect=1, pweight=NULL){
  # read data
  if (is.null(data)) {
    cat('Reading data...\n')
    if (filetype == 'sav') {
      data <- haven::read_sav(paste0('data/', test, '.sav'))
      label_csv <- paste0('data/', test, '_labels.csv')
      if (file.exists(label_csv)){
        nms <- read.csv(label_csv)$iLabel
        names(data) <- nms
      }
    } else if (filetype == 'csv') {
      data <- read.csv(paste0('data/', test, '.csv'))
    } else {
      stop('Data must use sav or csv.')
    }
  }

  poly_key <- ifelse(any(keys$Max_score > 1), TRUE, FALSE)
  labels <- keys$Item

  # ####### check inputs
  cat('Checking inputs...\n')
  if (length(method)!=1 || !(method %in% c('chi_square', 'Bonferroni', 'Facet'))) {
    stop('Please set \'method\' as one of \'chi_square\', \'Bonferroni\', or \'Facet\'.')
  }
  if (!all(c(pid, regr_vec_char) %in% names(data))) {
    stop('Pid or regressor is not in data column names!')
  }

  # calculate arguments
  cat('Using default arguments if not given...\n')
  if (is.null(n_resp)) n_resp <- ncol(data) - n_cov

  if (DIFVar %in% regr_vec_char){
    print('Cannot use DIF variable as covariate! Will remove it...')
    regr_vec_char <- setdiff(regr_vec_char, DIFVar)
  }
  # change 'DIFVar' to all lower-case; otherwise, R cannot call CQC
  if (method=='Bonferroni'){
    if (DIFVar != tolower(DIFVar)){
      print(paste0('\'', DIFVar, '\'', ' changed to ', '\'', tolower(DIFVar), '\'',
             ' as ConQuestr requires.\n'))
      ID_DIFVar <- which(names(data) == DIFVar)
      names(data)[ID_DIFVar] <- tolower(DIFVar)
      DIFVar <- tolower(DIFVar)
    }
  }

  # ####### preprocess data
  if (poly_key){
    cat('Checking polytomou-score items; recode if score are not continuous...\n')
    data <- poly_recode(keys, data, n_cov, c('r','R','m','M','9','x','X','.','',' ',NA))
  }

  if (prep_process){
    cat('Checking and removing items without data on any category of DIF variable...\n')
    processed <- sparse_data_process(test=test, data=data, keys=keys, labels=labels,
                     n_cov=n_cov, n_dims=n_resp, miss_code=c('.', 'r', 'R', 'x', 'X', '', ' '),
                     DIFVar=DIFVar)
    data <- processed[['data']]
    n_resp <- processed[['n_dims']]
    keys <- processed[['keys']]
    labels <- processed[['labels']]
  }

  # ####### prepare arguments
  cat('Preparing ConQuest control file...\n')
  prep <- df_key_lab_args(test, data, pid, n_cov, n_resp, DIFVar,
                          regr_vec_char, section_extr, labels, FALSE, pweight)

  arg_DIF <- list(method=method, delete=NULL, anchor=FALSE, domain=NULL,
          section_extr=prep$section_extr, dbl_key=NULL, poly_key=poly_key,
          quick=quick, step=step, keys=keys,
          p_cut=p_cut, DIF_cut=DIF_cut, DIF_adj_cut=DIF_adj_cut,
          facil_cut=facil_cut, desig_effect=desig_effect,
          test=test, DIFVar=DIFVar,
          vars=vars, poly_facet=poly_facet, poly_group=FALSE,
          poly_catgrs=NULL, save_xlsx=save_xlsx, iterative=iterative,
          pweight=pweight) %>%
    append(within(prep, rm(section_extr)))

  # ####### run models
  if (method=='chi_square'){
    do.call(DIF, arg_DIF)
  }
  if (method=='Bonferroni'){
    cats <- sort(unique(data[[DIFVar]]))
    cats <- cats[!is.na(cats)]
    arg_DIF[['poly_catgrs']] <- cats
    do.call(DIF, arg_DIF)
  }
  if (method=='Facet'){
    arg_DIF[['poly_facet']] <- TRUE
    do.call(DIF, arg_DIF)
  }
}
