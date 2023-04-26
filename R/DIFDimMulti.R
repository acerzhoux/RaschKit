#' DIFDimMulti
#'
#' This function can perform DIF analysis on either dichotomous or polytomous
#' variable. This is associated with test named 'test'. When dimension is above
#' two, you would be continuously shown "Yes/No" nonstop. In that case, please
#' stop the program and run the generated .cqc file in 'input' folder with
#' ConQuest GUI or Console version on your computer.
#'
#' Sparse data will be processed in three ways before being used for DIF analysis.
#' First, convert non-used categories and missing values to NA. Second, remove
#' item responses with all missing values. Third, remove items that have no data
#' on any category of the DIF variable. An Excel with processed data and
#' removed items is saved in 'data' folder. Its name has both test name and
#' DIF variable.
#'
#' @param method One of 'chi_square' or 'Bonferroni'.
#' @param DIFVar Name of DIF variable. Should be lowercase for ConQuest to run.
#' Default is NULL.
#' @param vars Vector of length 2 such as c('girls','boys'). Its order
#' corresponds to the alphabetic/numeric order of DIF variables' two categories in data.
#' @param test Name of the test.
#' @param respDf Dataframe with pid, covariables (e.g,, DIF variable), and responses.
#' Default is NULL where Excel file with name 'test' in 'data' folder is used.
#' @param filetype Format for input dataset. Default is 'sav'. Also support
#' csv format.
#' @param regrNmVec Vector of character regressors' names. Default is NULL.
#' Make sure to check colinearity of the regressors.
#' @param pid Name of candidates' ID variable.
#' @param n_cov Number of covariates before responses.
#' @param nDimVec Vector of numbers of responses the dimensions have.
#' @param dimNmVec Vector of the dimensions' names.
#' @param keyDf Dataframe of 'Item', 'Key', and 'Max_score' (add Key2 if double key).
#' @param quick TRUE when testing. Default is TRUE
#' @param delVec Vector of orders of items to be removed.
#' @param dblKeyLst TRUE if any item has polytomous scoring. Default is FALSE.
#' @param section_extr Extra sections to be added to 'test.cqc' file in 'input'
#' folder. Default is NULL.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between
#' two tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference
#' between two tests. Default is 4.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @param dim_multi TRUE if the model is multidimensional. Default is FALSE.
#' @param scores Scores possible in the test, e.g., 0:3. Default is NULL.
#' @param prep_process TRUE if it is needed to remove items without data on
#' some categories of DIF variable. Default is FALSE.
#' @param pweight Variable name of person weights in response dataframe. Should
#' be specified if weight is used for modeling. Default is NULL.
#' @param iter TRUE to iteratively remove DIF items. Default is FALSE.
#' @export

DIFDimMulti <- function(method=c('chi_square', 'Bonferroni'), test, DIFVar,
                          pid, n_cov, nDimVec, dimNmVec, respDf=NULL, filetype='sav',
                          keyDf, vars=NULL, regrNmVec=NULL,
                          quick=TRUE, delVec=NULL,
                          dblKeyLst=FALSE, section_extr=NULL,
                          p_cut=0.05, DIF_cut=0.5, DIF_adj_cut=4,
                          desig_effect=1, dim_multi=FALSE, scores=NULL,
                          prep_process=FALSE, pweight=NULL, iter=TRUE){
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
      stop('Data must use sav or csv.')
    }
  }

  poly_key <- ifelse(any(keyDf$Max_score > 1), TRUE, FALSE)
  labels <- keyDf$Item

  # ####### check inputs
  cat('Checking inputs...\n')
  if (method=='chi_square' & is.null(vars)) {
    stop('Please provide \'vars\' as category names that correspond to their alphabetic/numeric order in data.')
  }
  if (!all(c(pid, regrNmVec) %in% names(respDf))) {
    stop('Pid or regressor is not in data column names!')
  }
  if (length(method)!=1 || !(method %in% c('chi_square', 'Bonferroni'))) {
    stop('Please set \'method\' as one of \'chi_square\', \'Bonferroni\', or \'Facet\'.')
  }

  if (DIFVar %in% regrNmVec){
    print('Cannot use DIF variable as covariate! Will remove it...')
    regrNmVec <- setdiff(regrNmVec, DIFVar)
  }
  # change 'DIFVar' to all lower-case; otherwise, R cannot call CQC
  if (method=='Bonferroni'){
    if (DIFVar != tolower(DIFVar)){
      cat('\'', DIFVar, '\'', 'changed to', '\'', tolower(DIFVar), '\'', ' as ConQuestr requires.')
      ID_DIFVar <- which(names(respDf) == DIFVar)
      names(respDf)[ID_DIFVar] <- tolower(DIFVar)
      DIFVar <- tolower(DIFVar)
    }
  }
  # check colinearity?

  # ####### preprocess data
  if (prep_process){
    cat('Checking and removing items without data on any category of DIF variable...\n')
    processed <- sparse_data_process(test=test, respDf=respDf, keyDf=keyDf, labels=labels,
                  n_cov=n_cov, nDimVec=nDimVec, miss_code=c('.', 'r', 'R', 'x', 'X', '', ' '),
                  DIFVar=DIFVar)
    respDf <- processed[['respDf']]
    nDimVec <- processed[['nDimVec']]
    keyDf <- processed[['keyDf']]
    labels <- processed[['labels']]
  }

  # ####### prepare arguments
  cat('Preparing ConQuest control file...\n')
  prep <- df_key_lab_args(test, respDf, pid, n_cov, sum(nDimVec), DIFVar,
              regrNmVec, section_extr, labels, FALSE, pweight)
  if (dim_multi){
    if (is.null(scores)) scrs <- 0:1 else scrs <- scores
    prep[['section_extr']] <- prep[['section_extr']] |>
      c(section_dim(scrs=scrs, nDimVec=nDimVec, dimNmVec=dimNmVec))
  }
  arg_cqc <- list(test=test, run=NULL, run_ls=NULL, keyDf=keyDf,
    codes=prep$codes, pid_cols=prep$pid_cols, resps_cols=prep$resps_cols,
    quick=quick, delVec=delVec, dblKeyLst=dblKeyLst, poly_key=poly_key,
    anchor=FALSE, step=FALSE, regr_ls=prep$regr_ls, section_extr=prep$section_extr,
    DIFVar=DIFVar, DIFVar_cols=prep$DIFVar_cols, poly_catgrs=NULL,
    poly_facet=FALSE, poly_group=FALSE, pweight=pweight, pw_cols=prep$pw_cols)
  arg_DIF <- list(DIFVar=DIFVar, test=test, p_cut=p_cut, step=FALSE)

  N <- length(dimNmVec)

  # ####### DIF analysis
  if (method=='chi_square'){
    cat('Running ConQuest...\n')
    do.call(lab_cqc, arg_cqc)

    cat('Performing iterative chi_square tests...\n')
    # fdr <- paste0('DIF/', DIFVar)
    dfLst <- delta_DIF_dich(test, DIFVar)

    strt <- 1
    for (i in 1:N){
      arg_DIF[['test']] <- paste0(test, '_', dimNmVec[[i]])
      dfDelta <- dfLst[['dfDelta']][strt:(strt+nDimVec[[i]]-1), ]
      dfIndice <- dfLst[['dfIndice']][strt:(strt+nDimVec[[i]]-1), ]

      do.call(
        Equate,
        arg_DIF |>
          append(
            list(
              vars=vars,
              deltaDf=dfDelta,
              indDf=dfIndice,
              DIF_cut=DIF_cut,
              DIF_adj_cut=DIF_adj_cut,
              design_effect=design_effect,
              iter=iter
            )
          )
      )
      strt <- strt+nDimVec[[i]]
    }

    # #### can use add_format() to put together a summary file
  }

  if (method=='Bonferroni'){
    cat('Running ConQuest...\n')
    lab_tbl <- tibble(item=1:length(labels), label=labels) |>
      dplyr::mutate(item=as.character(item))
    cats <- unique(respDf[[DIFVar]])
    cats <- cats[!is.na(cats)] |> sort()
    arg_cqc[['poly_catgrs']] <- cats
    do.call(lab_cqc, arg_cqc)

    cat('Performing Bonferroni adjusted comparison...\n')
    strt <- 0
    for (i in 1:N){
      labs <- lab_tbl[(strt+1):(strt+nDimVec[[i]]), ]
      do.call(
        DIF_poly_shw,
        arg_DIF |> append(list(labels=labs, domain=dimNmVec[[i]]))
      )
      strt <- strt+nDimVec[[i]]
    }

    # #### can use add_format() to put together a summary file
  }
}

