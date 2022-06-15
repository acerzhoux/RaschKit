#' calibrate
#'
#' This function calibrates items in a test and adds comments and flags of levels of priority for review. This is associated with test named 'test'. If sav_xlsx is TRUE, an Excel file with a summary of the above information will be saved in 'results' folder in the working directory.
#'
#' @param wd Working directory. Default is the folder where .Rproj is located.
#' @param test Name of the test.
#' @param folder Place to find CQ output files.
#' @param data Dataframe with pid, covariables (e.g,, DIF variable), and responses. Default is NULL where Excel file with name 'test' in 'data' folder is used.
#' @param regr_vec_char Vector of character regressors' names.
#' @param pid Name of candidates' ID variable.
#' @param n_cov Number of covariates before responses.
#' @param n_dims Vector of numbers of responses the dimensions have. Default is NULL. Define this vector if multi-dimensional model is to be run, e.g., c(30, 45). Also should define this if there are variables after response columns, e.g., 30.
#' @param dim_names Vector of the dimensions' names. Default is NULL. Define this vector if multi-dimensional model is to be run.
#' @param keys Vector of keys in the test. Default is NULL.
#' @param labels Vector of item labels that correspond to order of item response columns in data.
#' @param quick TRUE when testing. Default is FALSE.
#' @param step TRUE if polytomous items are involved. Default is FALSE.
#' @param delete Vector of orders of items to be removed. Default is NULL.
#' @param dbl_key TRUE if any item has polytomous scoring. Default is FALSE.
#' @param poly_key TRUE if the key of any item has polytomous scoring. Default is FALSE.
#' @param anchor TRUE when anchor is to be done. Default is FALSE.
#' @param section_extr Extra sections to be added to 'test.cqc' file in 'input' folder. Default is NULL.
#' @param easy Threshold to flag easy items. Default is 90 (means 90 percent correct).
#' @param hard Threshold to flag hard items. Default is 10 (means 10 percent correct).
#' @param iRst Threshold to flag low item-rest correlation statistics. Default is 0.11.
#' @param fit_w Threshold to flag large weighted item fit statistics. Default is 1.1.
#' @param fit_uw Threshold to flag large unweighted item fit statistics. Default is 1.2.
#' @param dFallThr Ability on last bin above which falling distractor is flagged. Default is 0.5.
#' @param dRiseThr Ability on last bin below which rising distractor is unflagged. Default is 0.1.
#' @param abilEst2use Ability type used for curve data. Default is 'pv1'. Use 'wle' for smaller samples.
#' @param numAbilGrps Number of ability groups. Default is NULL.
#' @param sav_xlsx Whether to save Excel file to 'results' folder. Default is TRUE.
#' @param long_label Whether item labels are longer than 16 characters' fixed width. Default is FALSE.
#' @examples
#' calibrate(data=cov_respns, test='randomData_Dim3', pid="pid", n_cov=6, n_dims=c(20, 10), regr_vec_char=c('nation', 'age', 'language', 'grade'), dim_names=c('English', 'Maths'), quick=TRUE, all_mc=FALSE, dim_multi=TRUE)
#' calibrate(data=elena, test='elena_Dich2', pid="IDSTUD", n_cov=9, quick=TRUE, delete=c(1,6,8,9,28,34,35,36))
#' calibrate(data=elena, test='elana_math1', pid="IDSTUD", n_cov=9, quick=TRUE, all_mc=FALSE, poly_key=TRUE)
#' calibrate(data=racp, test='racp1', pid="V1", n_cov=1, quick=TRUE, keys=cd$`Correct options`, labels=cd$`Question ID`)
#'calibrate(data=elena, test='elana_poly_score', pid="IDSTUD", n_cov=9, quick=TRUE, poly_key=TRUE)
#' @export

calibrate <- function(wd=here::here(), folder=here::here('output'), test, data=NULL,
                      pid, n_cov, regr_vec_char=NULL, n_dims=NULL, dim_names=NULL,
                      keys=NULL, labels=NULL, quick=FALSE, delete=NULL,
                      dbl_key=FALSE, poly_key=FALSE, anchor=FALSE, section_extr=NULL,
                      easy=90, hard=10, iRst=.11, fit_w=1.1, fit_uw=1.2,
                      dFallThr=.5, dRiseThr=.1, sav_xlsx=TRUE, abilEst2use='pv1',
                      numAbilGrps=NULL, recode=TRUE, long_label=FALSE){
    # check input
    cat('Checking inputs...\n')
    if (!all(c(pid, regr_vec_char) %in% names(data))) {
        stop('Pid or regressor is not in data column names!')
    }
    if (is.null(data)) data <- readxl::read_xlsx(here::here('data', paste0(test, '.xlsx')))
    if (is.null(n_dims)) n_dims <- ncol(data) - n_cov
    if (is.null(keys) & !poly_key) keys <- rep(1, sum(n_dims))
    if (is.null(labels)) {
        labels <- names(data)[(n_cov+1):(n_cov+sum(n_dims))]
    } else {
        names(data)[(n_cov+1):(n_cov+sum(n_dims))] <- labels
    }

    # ####### check folders that may contain files related to 'test'
    cat('Move existing files with test name into new folder if any...\n')
    map(c('input', 'output', 'results'),
        ~move_into_folder(folder=file.path(wd, .x), test=test))

    # ####### preprocess data
    cat('Checking and removing items without data on any item...\n')
    processed <- sparse_data_process(test=test, data=data, keys=keys, labels=labels,
                                     n_cov=n_cov, n_dims=n_dims,
                                     miss_code=c('.', 'r', 'R', 'x', 'X', '', ' '),
                                     DIFVar=NULL)
    data <- processed[['data']]
    n_dims <- processed[['n_dims']]
    keys <- processed[['keys']]
    labels <- processed[['labels']]

    # recode data
    cat('Recoding data...\n')
    if (recode & !anchor){
        for (i in 1:length(n_dims)){
            if (i==1) data <- miss_recode(df=data, begin=n_cov+1, end=n_cov+n_dims[[i]])
            else data <- miss_recode(df=data, begin=n_cov+sum(n_dims[1:(i-1)])+1,
                                     end=n_cov+sum(n_dims[1:i]))
        }
    }
    if (!is.null(data)) data %>% writexl::write_xlsx(here::here('data', paste0(test, '.xlsx')))

    # prepare arguments
    cat('Preparing ConQuest control file...\n')
    prep <- df_key_lab_args(test=test, data=data, DIFVar=NULL,
                            regr_vec_char=regr_vec_char, section_extr=section_extr,
                            pid=pid, n_cov=n_cov, n_resp=sum(n_dims),
                            keys=keys, labels=labels)
    if (length(n_dims) > 1){
        if(is.null(dim_names)) stop('Please set dimension names \'dim_names\'!')
        if (poly_key){
            keys <- readxl::read_xlsx(here::here('data', 'keys.xlsx'), test)
            scrs <- 0:max(keys$Max_score)
        } else scrs <- 0:1
        prep[['section_extr']] <- prep[['section_extr']] %>%
            c(section_dim(scrs=scrs, n_dims=n_dims, dim_names=dim_names))
    }

    # calibrate
    cat('Calibrating test items...\n')
    lab_cqc(wd=wd, test=test, run=NULL, run_ls=NULL,
            codes=prep$codes, pid_cols=prep$pid_cols, resps_cols=prep$resps_cols,
            quick=quick, delete=delete, dbl_key=dbl_key, poly_key=poly_key,
            anchor=anchor, step=poly_key, regr_ls=prep$regr_ls,
            section_extr=prep$section_extr,
            DIFVar=NULL, DIFVar_cols=prep$DIFVar_cols, poly_catgrs=NULL,
            poly_facet=FALSE, poly_group=FALSE, dich_code=NULL)

    # check: Convergence
    cat('Checking convergence...\n')
    convg_Vernon(test=test, folder=folder)

    # check: Option frequencies
    if (!anchor){
        cat('Checking option frequencies...\n')
        freq_resps_cat_check(resp=data[(n_cov+1):(n_cov+sum(n_dims))],
                             folder=folder, test=test)
    }

    # CCC of categories and scores
    cat('Producing Category Characteristic Curve (CCC)...\n')
    plot_data <- CCC_Vernon(folder=folder, test=test, abilEst2use=abilEst2use,
                            numAbilGrps=numAbilGrps, long_label=long_label)
    ccc_data <- plot_data[['ccc_data']]
    iType <- plot_data[['itype']]

    # item summary
    cat('Putting together item analysis summaries...\n')
    itn_summary(folder=folder, test=test, easy=easy, hard=hard, iRst=iRst,
                fit_w=fit_w, fit_uw=fit_uw, dFallThr=dFallThr, dRiseThr=dRiseThr,
                sav_xlsx=sav_xlsx, ccc_data=ccc_data, iType=iType)
}
