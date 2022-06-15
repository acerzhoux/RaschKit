#' DIF_dim_one
#'
#' This function can perform DIF analysis on either dichotomous or polytomous variable. This is associated with test named 'test'.
#'
#' Should preprocess data in three ways. First, convert non-used categories and missing values to NA. Second, remove item responses with all missing values. Third, remove items that have all missing values or all correct/incorrect responses on any category of the DIF variable.
#'
#' @param method One of 'chi_square', 'Bonferroni', or 'Facet'.
#' @param wd Working directory. Default is the folder where .Rproj is located.
#' @param DIFVar Name of DIF variable. Should be lowercase for ConQuest to run. Default is NULL.
#' @param vars Vector of length 2 such as c('girls','boys'). Its order corresponds to the alphabetic/numeric order of DIF variables' two categories in data.
#' @param poly_facet TRUE if facet model is to be run on a polytomous DIF variable. Default is FALSE.
#' @param test Name of the test.
#' @param data Dataframe with pid, covariables (e.g,, DIF variable), and responses. Default is NULL where Excel file with name 'test' in 'data' folder is used.
#' @param keys Vector of keys in the test. Default is NULL.
#' @param pid Name of candidates' ID variable.
#' @param n_cov Number of covariates before responses.
#' @param n_resp Number of items. Default is NULL.
#' @param regr_vec_char Vector of character regressors' names.
#' @param labels Vector of item labels that correspond to order of item response columns in data.
#' @param quick TRUE when testing. Default is FALSE.
#' @param section_extr Extra sections to be added to .cqc file in 'input' folder. Default is NULL.
#' @param miss_code Code for missing data that should be converted to NA. Default is c('.', 'r', 'R', 'x', 'X', '', ' ').
#' @examples
#' DIF_dim_one(method='chi_square', vars=c('Girls', 'Boys'), data=df_DIF, test='David', DIFVar="studGender", pid="studentId", n_cov=5, regr_vec_char=c('countryName', 'studAge'), quick=TRUE, miss_code='.')
#' DIF_dim_one(method='Bonferroni', data=df_DIF[, 1:100], test='David', DIFVar="studAge", pid="studentId", n_cov=5, quick=TRUE, miss_code='.')
#' DIF_dim_one(method='Facet', data=df_DIF[, 1:100], test='David', DIFVar="studAge", pid="studentId", n_cov=5, quick=TRUE, miss_code='.')
#' DIF_dim_one(method='chi_square', vars=c('Girls', 'Boys'), data=elena, test='elena1', DIFVar="gender", pid="IDSTUD", n_cov=9, regr_vec_char=c('province', 'quintile'), quick=TRUE, miss_code=NULL)
#' DIF_dim_one(method='Bonferroni', data=elena, test='Elena1', DIFVar="quintile", pid="IDSTUD", n_cov=9, regr_vec_char=c('province', 'gender'), quick=TRUE, miss_code=NULL)
#' DIF_dim_one(method='Facet', data=elena, test='Elena1', DIFVar="quintile", pid="IDSTUD", n_cov=9, regr_vec_char=c('province', 'gender'), quick=TRUE, miss_code=NULL)
#' @export

DIF_dim_one <- function(method=c('chi_square', 'Bonferroni', 'Facet'),
                        test, pid, n_cov, DIFVar, data=NULL, keys=NULL,
                        wd=here::here(), vars=NULL, poly_facet=FALSE,
                        n_resp=NULL, regr_vec_char=NULL,
                        labels=NULL, quick=FALSE, section_extr=NULL,
                        miss_code=c('.', 'r', 'R', 'x', 'X', '', ' ')){
    # ####### check inputs
    cat('Checking inputs...\n')
    if (length(method)!=1 || !(method %in% c('chi_square', 'Bonferroni', 'Facet'))) {
        stop('Please set \'method\' as one of \'chi_square\', \'Bonferroni\', or \'Facet\'.')
    }
    if (!all(c(pid, regr_vec_char) %in% names(data))) {
        stop('Pid or regressor is not in data column names!')
    }
    if (is.null(data)) data <- readxl::read_xlsx(here::here('data', paste0(test, '.xlsx')))
    if (is.null(n_resp)) n_resp <- ncol(data) - n_cov
    if (is.null(keys)) keys <- rep(1, n_resp)
    if (is.null(labels)) labels <- names(data)[(n_cov+1):(n_cov+n_resp)]
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
    cat('Checking and removing items without data on any category of DIF variable...\n')
    processed <- sparse_data_process(test=test, data=data, keys=keys, labels=labels,
                                     n_cov=n_cov, n_dims=n_resp, miss_code=miss_code,
                                     DIFVar=DIFVar)
    data <- processed[['data']]
    n_resp <- processed[['n_dims']]
    keys <- processed[['keys']]
    labels <- processed[['labels']]

    # ####### prepare arguments
    cat('Preparing ConQuest control file...\n')
    prep <- df_key_lab_args(test=test, data=data, DIFVar=DIFVar,
                    regr_vec_char=regr_vec_char, section_extr=section_extr,
                    pid=pid, n_cov=n_cov, n_resp=n_resp,
                    keys=keys, labels=labels)

    arg_DIF <- list(method=method, wd=wd, delete=NULL, anchor=FALSE, domain=NULL,
                    section_extr=prep$section_extr, dbl_key=FALSE, poly_key=FALSE,
                    quick=quick, steps=FALSE,
                    p_cut=0.05, DIF_cut=0.5, DIF_adj_cut=4, chi_cut=10,
                    facil_cut=10, desig_effect=1, test=test, DIFVar=DIFVar,
                    vars=vars, poly_facet=poly_facet, poly_group=FALSE,
                    poly_catgrs=NULL) %>%
        append(prep[-length(prep)])

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
        poly_facet <- TRUE
        do.call(DIF, arg_DIF)
    }
}
