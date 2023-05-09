#' calibrate
#'
#' This function calibrates items in a test and adds comments and flags of
#' levels of priority for review. This is associated with test named 'test'.
#' If save_xlsx is TRUE, an Excel file with a summary of the above information
#' will be saved in 'results' folder in the working directory.
#'
#' @param test Name of the test.
#' @param respDf Dataframe with pid, covariables (e.g,, DIF variable), and
#' responses. Default is NULL where Excel file with name 'test' in 'data'
#' folder is used.
#' @param regrNmVec Vector of character regressors' names.
#' @param pid Name of candidates' ID variable.
#' @param n_cov Number of covariates before responses.
#' @param nDimVec Vector of numbers of responses the dimensions have.
#' Default is NULL. Define this vector if multi-dimensional model is to be run,
#' e.g., c(30, 45). Also should define this if there are variables after
#' response columns, e.g., 30.
#' @param dimNmVec Vector of the dimensions' names. Default is NULL.
#' Define this vector if multi-dimensional model is to be run.
#' @param keyDf Dataframe of 'Item', 'Key', and 'Max_score' (add Key2 if double key).
#' @param quick TRUE if empirical error is not needed. Default is TRUE
#' @param delVec Vector of orders or labels of items to be removed. Default is NULL.
#' @param dblKeyLst List of items with double keys. Element is double keys, and
#' element name is item order, e.g., list(`7`=c(1,3), `9`=c(3,4). Default is NULL.
#' @param trial TRUE when trial item diagnostics is needed after anchoring is done.
#' Default is FALSE.
#' @param section_extr Extra sections to be added to 'test.cqc' file in
#' 'input' folder. Default is NULL.
#' @param easy Threshold to flag easy items. Default is 90 (percent correct).
#' @param hard Threshold to flag hard items. Default is 10 (percent correct).
#' @param iRst Threshold to flag low item-rest correlation statistics.
#' Default is 0.11.
#' @param fit_w Threshold to flag large weighted item fit statistics.
#' Default is 1.1.
#' @param fit_uw Threshold to flag large unweighted item fit statistics.
#' Default is 1.2.
#' @param dFallThr Ability on last bin above which falling distractor is flagged.
#' Default is 0.5.
#' @param dRiseThr Ability on last bin below which rising distractor is unflagged.
#' Default is 0.1.
#' @param numAbilGrps Number of ability groups. Default is NULL.
#' @param recode_poly TRUE if polytomous items have non-continuous scores.
#' Note that code in c('r','R','m','M','9','x','X','.','',' ',NA) will be kept
#' intact while other codes will be down scored to be continuous. Default is FALSE.
#' @param missCode2Conv Response codes to convert to embedded/trailing missing
#' ('M' or 'R'). Use freq_resps_cat() to explore response distribution
#' and determine which codes need recoding. Commonly found missing symbols
#' are '@@' (less or more), 7, 8, 9, 88, 99, '.', '-', '', NA. Default is NULL
#' when responses have been recoded.
#' @param filetype Format for input dataset. Default is 'sav'. Also support
#' csv format.
#' @param slope Slope to multiply ability estimates. Default is NULL
#' @param intercept Value/intercept to add to ability estimates. Default is NULL.
#' @param extrapolation Whether to extrapolate the minimum and maximum estimates.
#' Default is FALSE.
#' @param save_xlsx Whether to save summary file. Default is TRUE (single test).
#' @param est_type Type of ability estimate to use for score equivalence table,
#' 'wle' or 'mle'. 'wle' is commonly used. Default is NULL (equiv table not needed).
#' @param sparse_check Whether to check response column sparsity in general or
#'  regarding any DIF variable category. Default is FALSE. If TRUE, sparse
#'  response columns will be removed.
#' @param CCCip2Wd TRUE if to save CCC and item-person map to a Word file. Default
#' is FALSE.
#' @param pweight Variable name of person weights in response dataframe. Should
#' be specified if weight is used for modeling. Default is NULL.
#' @param ancShift Shift of mean anchor delta from previous cycle obtained from
#' equating analysis. It will be added to each item delta of 'test' in 'output'
#' folder and put in 'input' folder as anchor file for 'test'. Default is NULL.
#' @param ancTest2Read Name of test in 'output' folder to read delta from. This
#' is usually concurrent calibration results such as all grades and/or all test
#' forms. Only delta's (and step estimates) in 'tests' are extracted and put in
#' 'input' folder as anchor file. Default is NULL.
#' @param ancDf Dataframe of 'Item' and 'Delta' for anchors. Include 'Step' column
#' for step estimates (step number). Default is NULL. Specify one of ancShift,
#' ancTest2Read, and ancDf to do anchoring. Check output 'xxx_anc.txt' file
#' from previous run for correct anchor order, especially for polytomous items
#' with step parameters.
#' @examples
#' # Not run
#' # calibrate(respDf=racp, test='RACP', pid="V1", n_cov=1, keyDf=cd,
#' # delVec=c(3,4,5,36), dblKeyLst=list(`7`=c(1,3), `9`=c(3,4)))
#' @export

calibrate <- function(test, respDf=NULL, keyDf, pid, n_cov, regrNmVec=NULL,
                      nDimVec=NULL, dimNmVec=NULL, quick=TRUE, delVec=NULL,
                      dblKeyLst=NULL, trial=FALSE, section_extr=NULL, easy=90,
                      hard=10, iRst=.11, fit_w=1.1, fit_uw=1.2, dFallThr=.5,
                      dRiseThr=.1, numAbilGrps=NULL, recode_poly=FALSE,
                      missCode2Conv=NULL, filetype='sav', slope=NULL, intercept=NULL,
                      extrapolation=FALSE, save_xlsx=TRUE, est_type=NULL,
                      sparse_check=FALSE, CCCip2Wd=FALSE, pweight=NULL,
                      ancShift=NULL, ancTest2Read=NULL, ancDf=NULL){
  options(warn=-1)

  if (!is.null(ancShift) || !is.null(ancTest2Read) || !is.null(ancDf)) {
    anchor <- TRUE
  } else {
    anchor <- FALSE
  }

  cat('\n============', if (anchor) 'Anchoring' else 'Calibrating', ':', test, '============\n\n')

  # check input I
  cat('Checking inputs...\n')
  if (!is.null(ancDf)){
    if (!all(names(ancDf) %in% c('Item', 'Delta'))){
      stop('ancDf should have names as \'Item\' and \'Delta\'!')
    }
  }
  if (!all(names(keyDf) %in% c('Item', 'Key', 'Max_score', 'Key2'))){
    stop('keyDf should have names as \'Item\', \'Key\', \'Max_score\', \'Key2 (if double key)\'!')
  }
  if (any(na.omit(str_extract(keyDf$Item, ' ') == ' '))) {
    stop('Column \'Item\' of keyDf should contain no space!')
  }

  # read data
  save_data <- TRUE
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
    save_data <- FALSE
  }

  # check input II
  if (!all(c(pid, regrNmVec) %in% names(respDf))) {
    stop('Pid or regressor is not in respDf column names!')
  }
  if (length(respDf[[pid]]) != length(unique(respDf[[pid]]))){
    stop('Please use unique pid for cases!')
  }

  # calculate arguments
  cat('Using default arguments if not given...\n')
  if (is.null(nDimVec)) nDimVec <- ncol(respDf) - n_cov
  labels <- keyDf$Item
  names(respDf)[(n_cov+1):(n_cov+sum(nDimVec))] <- labels

  # check input III
  if (nrow(keyDf) != sum(nDimVec)){
    stop('Please ensure row number of keyDf equals item column number in respDf!')
  }

  # ####### Move 'test'-related files I:
  cat('Move files in \'input\' folder with test name into subfolder...\n')
  move_into_folder('input', test)

  # ####### preprocess data
  poly_key <- ifelse(any(keyDf$Max_score > 1), TRUE, FALSE)

  if (poly_key){
    cat('Checking polytomou-score items; recode if score are not continuous...\n')
    if (recode_poly) {
      respDf <- poly_recode(keyDf, respDf, n_cov, c('r','R','m','M','9','x','X','.','',' ',NA))
    }
  }

  if (sparse_check){
    cat('Checking and removing items without data on any item or DIF variable categories...\n')
    processed <- sparse_data_process(test, respDf, keyDf, labels, n_cov, nDimVec,
                                     c('.', 'r', 'R', 'x', 'X', '', ' '), NULL)
    respDf <- processed[['respDf']]
    nDimVec <- processed[['nDimVec']]
    keyDf <- processed[['keyDf']]
    labels <- processed[['labels']]
  }

  # recode data
  if (!is.null(missCode2Conv)){
    cat('Recoding embedded & trailing missing in responses to M & R...\n')
    for (i in 1:length(nDimVec)){
      if (i==1){
        respDf <- miss_recode(respDf, n_cov+1, n_cov+nDimVec[[i]], 'M','R',
                  NA, missCode2Conv, F, F)
      }
      else {
        respDf <- miss_recode(respDf, n_cov+sum(nDimVec[1:(i-1)])+1,
                  n_cov+sum(nDimVec[1:i]), 'M','R', NA,
                  missCode2Conv, F, F)
      }
    }
  }

  # find out deleted item order if delete is item labels
  if (!is.null(delVec)){
    if (typeof(delVec) == "character"){
      delVec <- which(labels %in% delVec)
    }
  }

  # save data
  if (save_data) {
    # ####### Move 'test'-related files II:
    cat('Move files in \'data\' folder with test name into subfolder...\n')
    move_into_folder('data', test)

    cat('Saving data into csv and sav...\n')
    write.csv(respDf, paste0('data/', test, '.csv'), row.names=FALSE)
    # for .sav data
    tryCatch(
      {
        haven::write_sav(respDf, paste0('data/', test, '.sav'))
      },
      error=function(e) {
        data_sav <- respDf
        iSPSS <- paste0('V', 1:ncol(data_sav))
        names(data_sav)[1:ncol(data_sav)] <- iSPSS

        N1 <- sum(n_cov, length(labels))
        if (ncol(respDf)==N1){
          iLabel <- c(names(respDf)[1:n_cov], labels)
        } else {
          iLabel <- c(names(respDf)[1:n_cov], labels, names(respDf)[(N1+1):ncol(respDf)])
        }

        write.csv(
          tibble(iSPSS = iSPSS, iLabel = iLabel),
          paste0('data/', test, '_labels.csv'),
          row.names = FALSE
        )
        haven::write_sav(data_sav, paste0('data/', test, '.sav'))
      }
    )
  }

  # prepare arguments
  cat('Preparing ConQuest control file...\n')
  prep <- df_key_lab_args(test, respDf, pid, n_cov, sum(nDimVec), NULL,
                          regrNmVec, section_extr, labels, anchor, pweight)
  if (length(nDimVec) > 1){
    if(is.null(dimNmVec)) stop('Please set dimension names \'dimNmVec\'!')
    if (poly_key) scrs <- 0:max(keyDf$Max_score) else scrs <- 0:1
    prep[['section_extr']] <- prep[['section_extr']] |>
      c(section_dim(scrs=scrs, nDimVec=nDimVec, dimNmVec=dimNmVec))
  }

  # ####### process anchor file
  if (anchor) {
    cat('Processing anchor file...\n')
    if (!is.null(ancShift)){
      anchor_shift(test, ancShift)
    } else if (!is.null(ancTest2Read)) {
      ancDf <- anchor_getLab('output', ancTest2Read)
      anchor_process(test, respDf, keyDf, delVec, n_cov, nDimVec, ancDf)
    } else if (!is.null(ancDf)) {
      anchor_process(test, respDf, keyDf, delVec, n_cov, nDimVec, ancDf)
    } else { # read from 'input' folder
      ancPath <- paste0('input/', test, '_anc.txt')
      if (!file.exists(ancPath)) {
        stop(paste0(ancPath, ' with correct parameter order should exist!'))
      }
    }
  }

  # ####### Move 'test'-related files III:
  cat('Move files in \'output\' and \'results\' folder with test name into subfolder...\n')
  map(c('output', 'results'), ~move_into_folder(folder=.x, test=test))

  # ####### calibrate
  cat('Calibrating test items...\n')
  lab_cqc(test=test, keyDf=keyDf, run=NULL, run_ls=NULL,
      codes=prep$codes, pid_cols=prep$pid_cols, resps_cols=prep$resps_cols,
      quick=quick, delVec=delVec, dblKeyLst=dblKeyLst, poly_key=poly_key,
      anchor=anchor, step=FALSE, regr_ls=prep$regr_ls,
      section_extr=prep$section_extr,
      DIFVar=NULL, DIFVar_cols=prep$DIFVar_cols, poly_catgrs=NULL,
      poly_facet=FALSE, poly_group=FALSE,
      pweight=pweight, pw_cols=prep$pw_cols)

  # ####### read CQS output for summary
  cat('Reading CQS file...\n')
  cqs <- conquestr::ConQuestSys(paste0('output/', test, ".CQS"))
  saveRDS(cqs, paste0('output/', test, "_CQS.rds"))

  # ####### check: Convergence
  cat('Checking convergence...\n')
  check_convergence(cqs, test)

  if (anchor) {
    # check: input .anc file vs. output .anc file
    ancInput <- anchor_getLab('input', test)
    if (!('Step' %in% names(ancInput))) poly_key <- FALSE

    if (poly_key){
      anchor_dif <- left_join(
          ancInput |>
            rename(input=Delta),
          anchor_getLab('output', test) |>
            rename(output=Delta),
          by=c('Item', 'Step')
        ) |>
        dplyr::mutate(
          dif = input - output
        ) |>
        dplyr::filter(abs(dif) > 0.0001)
    } else {
      anchor_dif <- left_join(
          ancInput |>
            rename(input=Delta),
          anchor_getLab('output', test) |>
            rename(output=Delta),
          by='Item'
        ) |>
        dplyr::mutate(
          dif = input - output
        ) |>
        dplyr::filter(abs(dif) > 0.0001)
    }
    if (nrow(anchor_dif) != 0){
      print(anchor_dif)
      stop('Anchor order was messed up! Check printed difference above.')
    }

    # get equivalence table
    if (!is.null(est_type)) {
      cat('Generating equivalence table...\n')
      equiva_tbl(test, est_type, slope, intercept, extrapolation)
    }
    # est_cas(test)

    # point users to files of varying purposes
    writeLines(c(
      paste0('\n===== Output Files\n'),
      paste0('Anchoring and scaling of ', toupper(test), ':'),
      if (!is.null(est_type)) {
        paste0('\tScore equivalence table:\t', 'results/', 'eqv_', test, '.xlsx')
      },
      paste0('\tRaw and logit score table:\t', 'output/', test, '_cas',  '.xls')
    ))

  }

  if (!anchor || trial) { # summarize item calibration
    # ####### check: Option frequencies
    cat('Checking option frequencies...\n')
    tryCatch(
      {
        check_freq_resps_cat(test, respDf[(n_cov+1):(n_cov+sum(nDimVec))])
      },
      error = function(e){
        invisible()
      }
    )

    # ####### TODO: Plotting, summary stats for multi-dim models #######
    if (length(nDimVec)>1) return('Results are in output folder.')

    # ####### CCC of categories and scores
    cat('Producing Category Characteristic Curve (CCC)...\n')
    # determine whether to use wle or pv1
    n_min <- min(map_int(respDf[-c(1:n_cov)], ~length(str_remove_all(na.omit(.x), 'R'))))
    abilEst2use <- ifelse(n_min >= 200, 'pv1', 'wle')
    plot_data <- CCC_ipMap(test, cqs, abilEst2use, numAbilGrps, poly_key)
    ccc_data <- plot_data[['ccc_data']]
    iType <- plot_data[['itype']]

    # save CCC, imap to Word file
    if (CCCip2Wd) {
      cat('Saving CCC and ipMap to Word file...\n')
      rmd_file <- system.file("rmd", "CCC_ipMap.Rmd", package = "RaschKit")
      rmarkdown::render(
        rmd_file,
        params = list(test=test, plot_data=plot_data),
        output_file = str_c(test, '_CCC_ipMap', '.docx'),
        output_dir = here::here('output'),
        quiet = TRUE
      )
    }

    # ####### item summary
    cat('Putting together item analysis summaries...\n')
    smry <- itn_summary(test, easy, hard, iRst, fit_w, fit_uw,
              dFallThr, dRiseThr, ccc_data, iType, keyDf, dblKeyLst)

    if (save_xlsx){
      file_saved <- paste0('results/', paste0('itn_', test, '.xlsx'))
      writexl::write_xlsx(smry, file_saved)

      # point users to files of varying purposes
      writeLines(c(
        paste0('\n===== Output Files\n'),
        paste0('Item calibration of ', toupper(test), ':'),
        paste0('\tCQ output:\t', 'output/', ' (Files with \'', test, '\' in name)'),
        if (recode_poly & file.exists(paste0('data/', test, '_recode_score.csv'))){
          paste0('\tScore recoding:\t', 'data/', test, '_recode_score.csv')
        },
        if (save_data){
          paste0('\tData saved:\t', 'data/', test, '.xlsx\n\t\t\t',
               'data/', test, '.sav')
        },
        paste0('\tConverge check:\t', 'output/', test, '_convergence_check.pdf'),
        paste0('\tQA:\t\t', 'output/', test, '_Frequency_check.xlsx'),
        paste0('\tCCC:\t\t', 'output/', test, '_CCC.pdf'),
        if (CCCip2Wd){
          paste0('\tCCC_ipMap:\t', 'output/', test, '_CCC_ipMap', '.docx')
        },
        paste0('\tsummary:\t', 'results/', 'itn_', test, '.xlsx')
      ))
    } else {
      cat('Calibration of', test, 'completed.\n')
      return(smry)
    }
  }

  # remove cqs to save space
  rm(cqs)
}
