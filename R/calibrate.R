#' calibrate
#'
#' This function calibrates items in a test and adds comments and flags of
#' levels of priority for review. This is associated with test named 'test'.
#' If save_xlsx is TRUE, an Excel file with a summary of the above information
#' will be saved in 'results' folder in the working directory.
#'
#' @param test Name of the test.
#' @param data Dataframe with pid, covariables (e.g,, DIF variable), and
#' responses. Default is NULL where Excel file with name 'test' in 'data'
#' folder is used.
#' @param regr_vec_char Vector of character regressors' names.
#' @param pid Name of candidates' ID variable.
#' @param n_cov Number of covariates before responses.
#' @param n_dims Vector of numbers of responses the dimensions have.
#' Default is NULL. Define this vector if multi-dimensional model is to be run,
#' e.g., c(30, 45). Also should define this if there are variables after
#' response columns, e.g., 30.
#' @param dim_names Vector of the dimensions' names. Default is NULL.
#' Define this vector if multi-dimensional model is to be run.
#' @param keys Vector of keys in the test. Default is NULL.
#' @param labels Vector of item labels that correspond to order of item
#' response columns in data.
#' @param quick TRUE when testing. Default is FALSE.
#' @param delete Vector of orders or labels of items to be removed. Default is NULL.
#' @param dbl_key TRUE if any item has polytomous scoring. Default is NULL.
#' @param poly_key TRUE if the key of any item has polytomous scoring.
#' Default is FALSE.
#' @param anchor TRUE when anchor is to be done. Default is FALSE.
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
#' Default is FALSE.
#' @param long_label Whether item labels are longer than 16 characters' fixed
#' width. Default is FALSE.
#' @param missCode2Conv Missing codes that need to be converted to embedded and
#' trailing missing symbols such as 'M' and 'R'. Commonly used missing symbols are
#' '@@' (less or more),'7','8','9','88','99','.','',' ', and '-'.
#' @param filetype Format for input dataset. Default is 'sav'. Also support
#' csv and xlsx formats.
#' @param slope Slope to multiply ability estimates. Default is NULL
#' @param intercept Value/intercept to add to ability estimates. Default is NULL.
#' @param extrapolation Whether to extrapolate the minimum and maximum estimates.
#' Default is FALSE.
#' @param save_xlsx Whether to save summary file. Default is TRUE (single test).
#' @param est_type Type of ability estimate to use for score equivalence table,
#' 'wle' or 'mle'. Default is 'wle'.
#' @param anchor_read Whether to read from an anchor file in 'input' folder.
#' Default is FALSE. Then, should put anchors tbl (Item, Delta) on a sheet of
#' 'anchors.xlsx' in 'data' folder. If TRUE, an anchor .anc file should exist
#' in 'input' folder.
#' @param sparse_check Whether to check response column sparsity in general or
#'  regarding any DIF variable category. Default is FALSE. If TRUE, sparse
#'  response columns will be removed.
#' @param CCCip2Wd TRUE if to save CCC and item-person map to a Word file. Default
#' is FALSE.
#' @param pweight Variable name of person weights in response dataframe. Should
#' be specified if weight is used for modeling. Default is NULL.
#' @examples
#' # Not run
#' # calibrate(data=racp, test='RACP', pid="V1", n_cov=1, keys=cd$`Correct options`,
#' # labels=cd$`Question ID`, delete=c(3,4,5,36), dbl_key=list(`7`=c(1,3), `9`=c(3,4)))
#' @export

calibrate <- function(test, data=NULL,
                      pid, n_cov, regr_vec_char=NULL, n_dims=NULL, dim_names=NULL,
                      keys=NULL, labels=NULL, quick=FALSE, delete=NULL,
                      dbl_key=NULL, poly_key=FALSE, anchor=FALSE, section_extr=NULL,
                      easy=90, hard=10, iRst=.11, fit_w=1.1, fit_uw=1.2,
                      dFallThr=.5, dRiseThr=.1,
                      numAbilGrps=NULL, recode_poly=FALSE, long_label=FALSE,
                      missCode2Conv=c('@','@@','@@@','@@@@','7','8','9','88','99','.','',' ', '-'),
                      filetype='sav', slope=NULL, intercept=NULL,
                      extrapolation=FALSE, save_xlsx=TRUE, est_type='wle',
                      anchor_read=FALSE, sparse_check=FALSE, CCCip2Wd=FALSE,
                      pweight=NULL){
    options(warn=-1)
    # read data
    save_data <- TRUE
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
        } else if (filetype == 'xlsx'){
            data <- readxl::read_xlsx(paste0('data/', test, '.xlsx'))
        } else {
            stop('Data must use sav, csv, or xlsx.')
        }
        save_data <- FALSE
    }

    # check input
    cat('Checking inputs...\n')
    if (!all(c(pid, regr_vec_char) %in% names(data))) {
        stop('Pid or regressor is not in data column names!')
    }

    # calculate arguments
    cat('Using default arguments if not given...\n')
    if (is.null(n_dims)) n_dims <- ncol(data) - n_cov
    if (is.null(keys) & !poly_key) keys <- rep('1', sum(n_dims))
    if (is.null(labels)) {
        labels <- names(data)[(n_cov+1):(n_cov+sum(n_dims))]
    } else {
        names(data)[(n_cov+1):(n_cov+sum(n_dims))] <- labels
    }

    # ####### check folders that may contain files related to 'test'
    cat('Move existing files with test name into new folder if any...\n')
    if (anchor_read){
        folders_mov <- c('output', 'results')
    } else {
        folders_mov <- c('input', 'output', 'results')
    }
    map(folders_mov, ~move_into_folder(folder=.x, test=test))

    # ####### preprocess data
    if (poly_key){
        cat('Checking polytomou-score items; recode if score are not continuous...\n')
        keys <- readxl::read_xlsx(paste0('data/', 'keys.xlsx'), test)
        if (recode_poly) {
            data <- poly_recode(keys=keys, data=data, n_cov=n_cov,
                                miss_code=c('r','R','m','M','9','x','X','.','',' ',NA))
        }
    }

    if (sparse_check){
        cat('Checking and removing items without data on any item or DIF variable categories...\n')
        processed <- sparse_data_process(test=test, data=data, keys=keys, labels=labels,
                                         n_cov=n_cov, n_dims=n_dims,
                                         miss_code=c('.', 'r', 'R', 'x', 'X', '', ' '),
                                         DIFVar=NULL)
        data <- processed[['data']]
        n_dims <- processed[['n_dims']]
        keys <- processed[['keys']]
        labels <- processed[['labels']]
    }

    # recode data
    if (!is.null(missCode2Conv)){
        cat('Recoding embedded & trailing missing in responses to M & R...\n')
        for (i in 1:length(n_dims)){
            if (i==1){
                data <- miss_recode(
                    data, n_cov+1, n_cov+n_dims[[i]], 'M','R',
                    NA, missCode2Conv, F, F
                )
            }
            else {
                data <- miss_recode(
                    data, n_cov+sum(n_dims[1:(i-1)])+1, n_cov+sum(n_dims[1:i]),
                    'M','R', NA, missCode2Conv, F, F
                )
            }
        }
    }

    # find out deleted item order if delete is item labels
    if (!is.null(delete)){
        if (typeof(delete) == "character"){
            delete <- which(labels %in% delete)
        }
    }

    # save data
    if (save_data) {
        cat('Saving data into xlsx and sav...\n')
        writexl::write_xlsx(data, paste0('data/', test, '.xlsx'))
        # for .sav data
        tryCatch({
            haven::write_sav(data, paste0('data/', test, '.sav'))
        },
        error = function(e) {
            data_sav <- data
            iSPSS <- paste0('V', 1:ncol(data_sav))
            names(data_sav)[1:ncol(data_sav)] <- iSPSS

            N1 <- sum(n_cov, length(labels))
            if (ncol(data)==N1){
                iLabel <- c(names(data)[1:n_cov], labels)
            } else {
                iLabel <- c(names(data)[1:n_cov], labels, names(data)[(N1+1):ncol(data)])
            }

            write.csv(
                tibble(iSPSS = iSPSS, iLabel = iLabel),
                paste0('data/', test, '_labels.csv'),
                row.names = FALSE
            )
            haven::write_sav(data_sav, paste0('data/', test, '.sav'))
        })
    }

    # prepare arguments
    cat('Preparing ConQuest control file...\n')
    prep <- df_key_lab_args(test=test, data=data, DIFVar=NULL,
                            regr_vec_char=regr_vec_char, section_extr=section_extr,
                            pid=pid, n_cov=n_cov, n_resp=sum(n_dims),
                            keys=keys, labels=labels, anchor=anchor,
                            pweight=pweight)
    if (length(n_dims) > 1){
        if(is.null(dim_names)) stop('Please set dimension names \'dim_names\'!')
        if (poly_key) scrs <- 0:max(keys$Max_score) else scrs <- 0:1
        prep[['section_extr']] <- prep[['section_extr']] |>
            c(section_dim(scrs=scrs, n_dims=n_dims, dim_names=dim_names))
    }

    # ####### process anchor file
    if (anchor) {
        cat('Processing anchor file...\n')
        if (anchor_read){
            # put anchor file into 'input' folder beforehand
            # good when to use output '.anc' file from previous run
        } else {
            anchor_process(test=test, data=data, keys=keys, labels=labels,
                           delete=delete, poly_key=poly_key,
                           n_cov=n_cov, n_dims=n_dims)
        }
    }

    # ####### calibrate
    cat('Calibrating test items...\n')
    lab_cqc(test=test, run=NULL, run_ls=NULL,
            codes=prep$codes, pid_cols=prep$pid_cols, resps_cols=prep$resps_cols,
            quick=quick, delete=delete, dbl_key=dbl_key, poly_key=poly_key,
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

    if (anchor){
        # check: input .anc file vs. output .anc file
        anchor_dif <- read.table(paste0('input/', test, '_anc.txt')) |>
            dplyr::select(anchor=V3, input=V2) |>
            left_join(
                read.table(paste0('output/', test, '_anc.txt')) |>
                    mutate(output=V2, anchor=str_c(V3,V5,V6)) |>
                    dplyr::select(anchor, output),
                by = "anchor"
            ) |>
            mutate(
                dif = input - output,
                anchor = str_remove_all(anchor, '[(//)(/*)]')
            ) |>
            dplyr::filter(abs(dif) > 0.0001)
        if (nrow(anchor_dif) != 0){
            print(anchor_dif)
            stop('Anchor order was messed up! Check printed difference above.')
        }

        # get equivalence table
        cat('Generating equivalence table...\n')
        equiva_tbl(test, est_type, slope, intercept, extrapolation)
        est_cas(test)
        rm(cqs)

        # point users to files of varying purposes
        writeLines(c(
            paste0('\n========= Output Files =========\n'),
            paste0('Anchoring and scaling of ', toupper(test), ':'),
            paste0('\tScore equivalence table:\t', 'results/', 'eqv_tbl_', test, '.xlsx'),
            paste0('\tRaw and logit score table:\t', 'results/', 'estimates_', test, '.xlsx'),
            if (!is.null(intercept) | !is.null(slope)){
                paste0('\tScaled score table:\t\t', 'results/', 'scaled_tbl_', test, '.xlsx')

            }
        ))
    } else { # summarize item calibration
        # ####### check: Option frequencies
        cat('Checking option frequencies...\n')
        check_freq_resps_cat(test, data[(n_cov+1):(n_cov+sum(n_dims))])

        # ####### TODO: Plotting, summary stats for multi-dim models #######
        if (length(n_dims)>1) return('Results are in output folder.')

        # ####### CCC of categories and scores
        cat('Producing Category Characteristic Curve (CCC)...\n')
        # determine whether to use wle or pv1
        n_min <- min(map_int(data[-c(1:n_cov)], ~length(str_remove_all(na.omit(.x), 'R'))))
        abilEst2use <- ifelse(n_min >= 200, 'pv1', 'wle')
        plot_data <- CCC_ipMap(test, cqs, abilEst2use, numAbilGrps, long_label, poly_key)
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
                            dFallThr, dRiseThr, ccc_data, iType, quick)
        rm(cqs)
        if (save_xlsx){
            file_saved <- paste0('results/', paste0('itn_', test, '.xlsx'))
            writexl::write_xlsx(smry, file_saved)

            # point users to files of varying purposes
            writeLines(c(
                paste0('\n========= Output Files =========\n'),
                paste0('Item calibration of ', toupper(test), ':'),
                paste0('\tCQ output:\t', 'output/', ' (Files with \'', test, '\' in name)'),
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

}
