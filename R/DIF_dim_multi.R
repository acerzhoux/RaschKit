#' DIF_dim_multi
#'
#' This function can perform DIF analysis on either dichotomous or polytomous variable. This is associated with test named 'test'. When dimension is above two, you would be continuously shown "Yes/No" nonstop. In that case, please stop the program and run the generated .cqc file in 'input' folder with ConQuest GUI or Console version on your computer.
#'
#' Sparse data will be processed in three ways before being used for DIF analysis. First, convert non-used categories and missing values to NA. Second, remove item responses with all missing values. Third, remove items that have no data on any category of the DIF variable. An Excel with processed data and removed items is saved in 'data' folder. Its name has both test name and DIF variable.
#'
#' @param method One of 'chi_square' or 'Bonferroni'.
#' @param wd Working directory. Default is the folder where .Rproj is located.
#' @param DIFVar Name of DIF variable. Should be lowercase for ConQuest to run. Default is NULL.
#' @param vars Vector of length 2 such as c('girls','boys'). Its order corresponds to the alphabetic/numeric order of DIF variables' two categories in data.
#' @param test Name of the test.
#' @param data Dataframe with pid, covariables (e.g,, DIF variable), and responses. Default is NULL where Excel file with name 'test' in 'data' folder is used.
#' @param regr_vec_char Vector of character regressors' names. Default is NULL. Make sure to check colinearity of the regressors.
#' @param pid Name of candidates' ID variable.
#' @param n_cov Number of covariates before responses.
#' @param n_dims Vector of numbers of responses the dimensions have.
#' @param dim_names Vector of the dimensions' names.
#' @param keys Vector of keys in the test. Default is NULL.
#' @param labels Vector of item labels that correspond to order of item response columns in data.
#' @param quick TRUE when testing. Default is TRUE
#' @param step TRUE if polytomous items are involved. Default is FALSE.
#' @param delete Vector of orders of items to be removed.
#' @param dbl_key TRUE if any item has polytomous scoring. Default is FALSE.
#' @param poly_key TRUE if the key of any item has polytomous scoring. Default is FALSE.
#' @param anchor TRUE when anchor is to be done. Default is FALSE.
#' @param section_extr Extra sections to be added to 'test.cqc' file in 'input' folder. Default is NULL.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param chi_cut Threshold of chi-square difference between two items Default is 10.
#' @param DIF_cut Threshold of an item's delta estimate difference between two tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference between two tests. Default is 4.
#' @param facil_cut Threshold of number of percent to flag an item with large facility difference between two groups of test takers. Default is 10.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @param dim_multi TRUE if the model is multidimensional. Default is FALSE.
#' @param scores Scores possible in the test, e.g., 0:3. Default is NULL.
#' @param miss_code Code for missing data that should be converted to NA. Default is c('.', 'r', 'R', 'x', 'X', '', ' ').
#' @param prep_process TRUE if it is needed to remove items without data on some categories of DIF variable. Default is FALSE.
#' @examples
#' DIF_dim_multi(method='chi_square', vars=c('Girls', 'Boys'), DIFVar="studGender", data=df_DIF, test='ELANA', pid='studentId', n_cov=5, n_dims=c(901, 171), dim_names=c('HUM', 'Maths'), regr_vec_char=c('countryName', 'studAge'), section_extr='group countryName;\n')
#' DIF_dim_multi(method='Bonferroni', DIFVar="studAge", data=df_DIF, test='ELANA', pid='studentId', n_cov=5, n_dims=c(901, 171), dim_names=c('HUM', 'Maths'), regr_vec_char=c('countryName', 'studGender'), quick=TRUE, section_extr='group countryName;\n')
#' @export

DIF_dim_multi <- function(method=c('chi_square', 'Bonferroni'), test, DIFVar,
                          pid, n_cov, n_dims, dim_names, data=NULL, keys=NULL,
                          labels=NULL, vars=NULL, regr_vec_char=NULL, wd=here::here(),
                          quick=TRUE, step=FALSE, delete=NULL,
                          dbl_key=FALSE, poly_key=FALSE, anchor=FALSE, section_extr=NULL,
                          p_cut=0.05, DIF_cut=0.5, DIF_adj_cut=4, chi_cut=10, facil_cut=10,
                          desig_effect=1, dim_multi=FALSE, scores=NULL,
                          miss_code=c('.', 'r', 'R', 'x', 'X', '', ' '),
                          prep_process=FALSE){
    # ####### check inputs
    cat('Checking inputs...\n')
    if (!all(c(pid, regr_vec_char) %in% names(data))) {
        stop('Pid or regressor is not in data column names!')
    }
    if (length(method)!=1 || !(method %in% c('chi_square', 'Bonferroni'))) {
        stop('Please set \'method\' as one of \'chi_square\', \'Bonferroni\', or \'Facet\'.')
    }
    if (is.null(data)) data <- readxl::read_xlsx(here::here('data', paste0(test, '.xlsx')))
    if (is.null(labels)) labels <- names(data)[(n_cov+1):(n_cov+sum(n_dims))]
    if (is.null(keys)) keys <- rep(1, sum(n_dims))
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
    # check colinearity?

    # ####### preprocess data
    if (prep_process){
        cat('Checking and removing items without data on any category of DIF variable...\n')
        processed <- sparse_data_process(test=test, data=data, keys=keys, labels=labels,
                                  n_cov=n_cov, n_dims=n_dims, miss_code=miss_code,
                                  DIFVar=DIFVar)
        data <- processed[['data']]
        n_dims <- processed[['n_dims']]
        keys <- processed[['keys']]
        labels <- processed[['labels']]
    }

    # ####### prepare arguments
    cat('Preparing ConQuest control file...\n')
    prep <- df_key_lab_args(test=test, data=data, DIFVar=DIFVar,
                            regr_vec_char=regr_vec_char, section_extr=section_extr,
                            pid=pid, n_cov=n_cov, n_resp=sum(n_dims),
                            keys=keys, labels=labels)
    if (dim_multi){
        if (is.null(scores)) scrs <- 0:1 else scrs <- scores
        prep[['section_extr']] <- prep[['section_extr']] %>%
            c(section_dim(scrs=scrs, n_dims=n_dims, dim_names=dim_names))
    }
    arg_cqc <- list(wd=wd, test=test, run=NULL, run_ls=NULL,
        codes=prep$codes, pid_cols=prep$pid_cols, resps_cols=prep$resps_cols,
        quick=quick, delete=delete, dbl_key=dbl_key, poly_key=poly_key,
        anchor=anchor, step=step, regr_ls=prep$regr_ls, section_extr=prep$section_extr,
        DIFVar=DIFVar, DIFVar_cols=prep$DIFVar_cols, poly_catgrs=NULL,
        poly_facet=FALSE, poly_group=FALSE) %>%
        append(list(dich_code=vars))
    arg_DIF <- list(DIFVar=DIFVar, test=test, p_cut=p_cut, step=step)

    lab_tbl <- tibble(item=1:length(labels), label=labels)
    N <- length(dim_names)

    # ####### DIF analysis
    if (method=='chi_square'){
        cat('Running ConQuest...\n')
        do.call(lab_cqc, arg_cqc)

        cat('Performing iterative chi_square tests...\n')
        fdr <- here::here('DIF', DIFVar)
        df <- delta_DIF_dich(folder=fdr, test=test, DIFVar=DIFVar) %>%
            bind_cols(delta_error_DIF_dich(folder=fdr, test=test,
                                           long_label=TRUE)) %>%
            select(item, everything()) %>%
            na.omit() %>%
            as_tibble()

        strt <- 1
        for (i in 1:N){
            arg_DIF[['test']] <- paste0(test, '_', dim_names[[i]])
            dat <- df[strt:(strt+n_dims[[i]]-1), ]
            do.call(DIF_dich,
                    arg_DIF %>% append(list(vars=vars, df=dat,
                                            DIF_cut=DIF_cut, DIF_adj_cut=DIF_adj_cut,
                                            chi_cut=chi_cut, facil_cut=facil_cut,
                                            desig_effect=desig_effect)))
            strt <- strt+n_dims[[i]]
        }
    }

    if (method=='Bonferroni'){
        cat('Running ConQuest...\n')
        lab_tbl <- mutate(lab_tbl, item=as.character(item))
        cats <- unique(data[[DIFVar]])
        cats <- cats[!is.na(cats)] %>% sort()
        arg_cqc[['poly_catgrs']] <- cats
        do.call(lab_cqc, arg_cqc)

        cat('Performing Bonferroni adjusted comparison...\n')
        strt <- 0
        for (i in 1:N){
            labs <- lab_tbl[(strt+1):(strt+n_dims[[i]]), ]
            do.call(DIF_poly_shw, arg_DIF %>%
                        append(list(folder=NULL,
                                    labels=labs,
                                    domain=dim_names[[i]])))
            strt <- strt+n_dims[[i]]
        }
    }
}

