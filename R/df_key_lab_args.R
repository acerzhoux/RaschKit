#' df_key_lab_args
#'
#' This function saves data, label, and keys into 'Data' folder in the working
#' directory. It also prepares the arguments needed for calibration functions.
#' This is associated with test named 'test'.
#'
#' @param test Name of the test.
#' @param data Dataframe with covariables, DIF variable, and responses.
#' @param pid Name of candidates' ID variable.
#' @param n_cov Number of covariates before responses.
#' @param n_resp Number of items in the data.
#' @param DIFVar DIF variable. Default is NULL.
#' @param regr_vec_char Vector of character regressors' names.
#' @param section_extr Sections to be included in the .cqc file. Default is NULL.
#' @param labels Item labels. Default is NULL.
#' @param useR TRUE when code 'R' is used for scoring. Default is FALSE.
#' @param pweight Variable name of person weights in response dataframe. Should
#' be specified if weight is used for modeling. Default is NULL.
#' @return A list of arguments with calculated elements.
#' @examples
#' # not run
#' # df_key_lab_args(data=cov_respns, test='randomData', DIFVar="age",
#' # keys=rep(1, 30), pid="pid", n_cov=6, n_resp=30,
#' # regr_vec_char=c('nation', 'language', 'grade'))
#' @export

df_key_lab_args <- function(test, data, pid, n_cov, n_resp,
                            DIFVar=NULL, regr_vec_char=NULL,
                            section_extr=NULL, labels=NULL, useR=FALSE,
                            pweight=NULL){
    create_folders(DIFVar=DIFVar)
    if (!is.null(regr_vec_char)){
        section_extr <- map(regr_vec_char, ~data[[.]] %>%
                            unique() %>% na.omit() %>% sort() %>%
                            str_pad(max(nchar(.)), 'right', ' ') %>%
                            paste0(collapse='\":\"') %>%
                            paste0('categorise ', .x, '(\"', ., '\");\n')) %>%
            unlist() %>%
            c(section_extr) %>%
            paste0(collapse='')
    }
    data <- data %>%
        modify_at(1:(n_cov+n_resp), ~as.character(.))

    # save data, label
    data_into_Data(test=test, data=data, DIFVar=DIFVar)
    labels_into_Data(test=test, labels=labels)

    # prepare arguments
    prr <- pid_resp_regrs_cols(df=data, pid=pid, n_cov=n_cov,
                               n_resp=n_resp, regr_vec=regr_vec_char,
                               pweight=pweight)
    if (!is.null(DIFVar)) {
        DIFVar_cols <- var_cols(df=data, var_name=DIFVar)[[DIFVar]]
    } else {
        DIFVar_cols <- NULL
    }

    codes <- map(data[, (n_cov+1):(n_cov+n_resp)], ~unique(.)) %>% #codes
        reduce(c) %>%
        unique()

    if (useR){
        codes <- codes %>%
            .[!(. %in% c(NA, 'X', 'x', '', ' '))] %>%
            sort()
    } else {
        codes <- codes %>%
            .[!(. %in% c(NA, 'r', 'R', 'X', 'x', '', ' '))] %>%
            sort()
    }

    list(
        codes=codes,
        pid_cols=prr$pid_cols,
        pw_cols=prr$pw_cols,
        resps_cols=prr$resps_cols,
        regr_ls=prr$regr_ls,
        DIFVar_cols=DIFVar_cols,
        section_extr=section_extr
    )
}
