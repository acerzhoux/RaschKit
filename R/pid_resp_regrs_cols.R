#' pid_resp_regrs_cols
#'
#' This function calculates and outputs some variables' beginning and ending in a dataframe if saved as a fixed-width .txt file. Those variables include person ID, responses, and perhaps regressors.

#' @param df Dataframe with ID, covariates and responses.
#' @param pid Variable name of person ID.
#' @param n_cov Number of covariates before responses.
#' @param n_resp Number of responses.
#' @param regr_vec String vector of regressors' names. Default is NULL.
#' @return e.g., list(pid_cols='14-30', resps_cols='35-308', regr_ls=list(...))
#' @examples
#' pid_resp_regrs_cols(df=cov_respns, pid='pid', n_cov=5, n_resp=2)
#' pid_resp_regrs_cols(df=cov_respns, pid='pid', n_cov=5, n_resp=2, regr_vec=c('nation', 'gender', 'G2'))
#' @export

pid_resp_regrs_cols <- function(df, pid, n_cov, n_resp, regr_vec=NULL){
    # pid_cols
    pid_cols <- var_cols(df=df, var_name=pid)[[pid]]
    # resps_cols
    strt <- sum(map_int(1:n_cov, ~df[[.]] %>% na.omit() %>% nchar %>% max))
    edj <-  strt + n_resp
    resps_cols <- paste0((strt + 1), '-', edj)
    # regr_ls
    if (!is.null(regr_vec)) {
        regr_ls <- map(regr_vec, ~var_cols(df=df, var_name=.)) %>%
            reduce(append)
        list(pid_cols=pid_cols, resps_cols=resps_cols, regr_ls=regr_ls)
    } else {
        list(pid_cols=pid_cols, resps_cols=resps_cols, regr_ls=NULL)
    }
}
